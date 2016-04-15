{-|
Module      : Language.X86.Lifetime
Description : Definitions for computing variable lifetimes.
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Lifetime analysis consists in determining the lifetime for every variable in a
given function (that is the level we operate at here). The lifetime is the span
between a variable's first and last uses. This information is used later on in
register allocation.

Here, lifetimes cannot have "holes", that is portions of code where the variable
does not contain anything useful. This will make our register allocation slightly
worse, at the advantage of a much simpler implementation.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.X86.Lifetime where

import Language.X86.Core
import Language.X86.Virtual

import Control.Monad.Trans.Free
import Control.Monad.Identity
import Control.Monad.State

import qualified Data.Map.Strict as M

-- | Extends a register's lifetime (as a side-effect).
extendLifetime :: SizedVirtualRegister -> LifetimeAnalysis ()
extendLifetime r = do
    cur <- gets _curLocation
    -- If the lifetime already exists, it's replaced with a version that has the
    -- ending of the default lifetime, which is just the current program point.
    modify (\s -> s { _lifetimes = M.insertWith
                        (\new old -> LifetimeSpan (_start old) (_end new))
                        r
                        (LifetimeSpan cur cur)
                        (_lifetimes s) })

-- | Extends the lifetime of any registers contained in this operand.
checkLifetime :: Operand SizedVirtualRegister Int -> LifetimeAnalysis ()
checkLifetime o = case o of
    Register d -> case d of
        Direct reg -> extendLifetime reg
        Indirect disp -> case disp of
            Offset _ reg -> extendLifetime reg

    -- If an operand does no contain a register, it affects no lifetimes.
    _ -> pure ()

-- | In the case of a backwards jump, we want to extend the lifetimes of anything
-- that is live between the jump's target and the jump's location. This ensures
-- that variables are live throughout a loop.
extendLifetimeForJump :: Int -> Int -> LifetimeAnalysis ()
extendLifetimeForJump begin end = do
    lts <- gets _lifetimes
    let lts' = M.map (\lt -> LifetimeSpan
            { _start = min begin $ _start lt
            , _end = if _end lt > begin then end else _end lt
            })
            lts
    modify (\s -> s {_lifetimes = lts'})

-- | Determine the lifetimes for every virtual register in the given assembly
-- code. We are only interested in the state of this computation.
createLifetimes :: VirtualAsm Int () -> LifetimeAnalysis ()
createLifetimes = iterM phi where
    phi :: AsmF Int (Operand SizedVirtualRegister Int) (LifetimeAnalysis ())
        -> LifetimeAnalysis ()
    phi a = case a of
        -- Generate a new label. This is fairly arbitrary; we just need them to
        -- be unique for this computation, as we keep a record of when labels
        -- get set.
        NewLabel f -> do
            lbl <- gets _nextLabel
            modify $ \s -> s {_nextLabel = _nextLabel s + 1}
            f lbl

        SetLabel l n -> do
            c <- gets _curLocation
            modify (\s -> s {_labelLocations = M.insert l c $ _labelLocations s})
            n

        Emit i n -> do
            checkLifetimeForInst i
            modify (\s -> s {_curLocation = _curLocation s + 1})
            n

        -- Ignore pseudoinstructions
        Scratch _ n -> n
        Prologue _ n -> n

    checkLifetimeForInst i = case i of
        Ret -> pure ()
        Nop -> pure ()
        Syscall -> pure ()
        Mov v1 v2 -> (checkLifetime v1) >>= (const $ checkLifetime v2)
        Call v -> checkLifetime v
        Add v1 v2 -> (checkLifetime v1) >>= (const $ checkLifetime v2)
        Sub v1 v2 -> (checkLifetime v1) >>= (const $ checkLifetime v2)
        Mul _ v1 v2 mv3 -> do
            checkLifetime v1
            checkLifetime v2
            case mv3 of
                Nothing -> pure ()
                Just v -> checkLifetime v
        Xor v1 v2 -> (checkLifetime v1) >>= (const $ checkLifetime v2)
        Inc v -> checkLifetime v
        Dec v -> checkLifetime v
        Push v -> checkLifetime v
        Pop v -> checkLifetime v
        Int v -> checkLifetime v
        Cmp v1 v2 -> (checkLifetime v1) >>= (const $ checkLifetime v2)
        Test v1 v2 -> (checkLifetime v1) >>= (const $ checkLifetime v2)
        Sal v1 v2 -> (checkLifetime v1) >>= (const $ checkLifetime v2)
        Sar v1 v2 -> (checkLifetime v1) >>= (const $ checkLifetime v2)
        Jump _ v -> case v of
            Label l -> checkLifetimeForJump l
            _ -> error "unimplemented: error when encountering a jump to something not a label"
            -- Still undecided on what should happen here. In some cases our
            -- analysis is genuinely thrown off (such as with jumps with a
            -- variable offset) but others might be "ignorable".
        Neg1 v -> checkLifetime v
        Neg2 v -> checkLifetime v
        Setc _ v -> checkLifetime v
        _ -> error "unimplemented: checkLifetimeForInst"

    -- In the case of a jump, we do some basic control flow analysis to ensure
    -- that the ranges are still correct.
    checkLifetimeForJump l = do
        lbls <- gets _labelLocations
        cur <- gets _curLocation
        case M.lookup l lbls of

            -- If this label doesn't have a location yet, then presumably its
            -- target is somewhere ahead, in which case we don't have to worry
            -- about it.
            --
            -- Note: this assumption is of course violated if the label later
            -- gets a SetLabel to a program point before the current one...
            Nothing -> pure ()
            Just p -> when (p < cur) $ extendLifetimeForJump p cur

-- | The state used in lifetime analysis. The analysis scans the list of
-- instructions forwards, updating lifetimes for every value encountered.
data LifetimeAnalysisState
    = LifetimeAnalysisState
        { _lifetimes :: M.Map SizedVirtualRegister LifetimeSpan
        -- ^ Each virtual register has its own lifetime. This also includes
        -- pre-assigned hardware registers, whose lifetime is computed to allow
        -- the possibility of re-assigning something to them once they are dead.
        , _curLocation :: Int
        -- ^ The current program point we are at.
        , _nextLabel :: Int
        -- ^ The next label to generate.
        , _labelLocations :: M.Map Int Int
        -- ^ An association between labels and their locations, as given by
        -- occurrences of "SetLabel"s in the code.
        }

-- | The lifetime of a variable. Start and end values are inclusive.
-- The units are program points.
data LifetimeSpan
    = LifetimeSpan
        { _start :: Int
        , _end :: Int
        }
    deriving (Show)

-- | Determines if two lifetimes overlap, which is the case when their ranges
-- intersect.
overlaps :: LifetimeSpan -> LifetimeSpan -> Bool
overlaps a b
    =  (_start a >= _start b && _start a <= _end b)
    || (_end a >= _start b && _end a <= _end b)
    || (_start a <= _start b && _end a >= _end b)

-- | Two lifetime spans are equal if their starting and ending positions are the same.
instance Eq LifetimeSpan where
    a == b = (_start a == _start b) && (_end a == _end b)

-- | Lifetime spans are ordered by start position, then by end position.
instance Ord LifetimeSpan where
    a <= b = (_start a < _start b) || ((_start a == _start b) && (_end a < _end b))

-- | Given a virtual assembly program, computes the lifetime intervals of the
-- virtual registers it contains.
computeLifetimes :: VirtualAsm Int () -> M.Map SizedVirtualRegister LifetimeSpan
computeLifetimes vasm =
    let start = LifetimeAnalysisState {
        _lifetimes = M.empty
        , _curLocation = 0
        , _nextLabel = 0
        , _labelLocations = M.empty
        } in
    _lifetimes
    $ runIdentity
    $ execStateT (runLifetimeAnalysisT $ createLifetimes vasm) start

-- A monad transformer for lifetime analysis.
newtype LifetimeAnalysisT m a
    = LifetimeAnalysisT
        { runLifetimeAnalysisT
            :: StateT LifetimeAnalysisState m a
        }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState LifetimeAnalysisState
        , MonadTrans
        )

type LifetimeAnalysis = LifetimeAnalysisT Identity
