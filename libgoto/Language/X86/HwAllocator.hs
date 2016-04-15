{-|
Module      : Language.X86.HwAllocator
Description : Definitions for the hardware register allocator.
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

The register allocator turns virtual registers into hardware memory locations.
The allocation strategy is linear. Every interval is considered in increasing
order of starting positions, and given, if possible, a hardware register of the
correct kind that is not used by any of the conflicting intervals. If no
registers are available, the interval with the largest end position is spilled.

The register @rax@ is never assigned; it is reserved for return values and as a
scratch register when an actual register is required. For instance, suppose that
given the code @mov vreg2, vreg1@, the allocator decides to spill both @vreg1@
and @vreg2@ to memory locations @mem1@ and @mem2@ respectively. It is illegal to
@mov@ between memory locations. In this situation, @rax@ can be used to instead
generate the following:
@
    mov rax, QWORD PTR [mem1]
    mov QWORD PTR [mem2], rax
@
-}

module Language.X86.HwAllocator
( allocate
) where

import Language.X86.Core
import Language.X86.Hardware
import Language.X86.Lifetime
import Language.X86.Virtual

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.PQueue.Prio.Min as PQ

import Control.Monad.ST
import Data.STRef

import Prelude hiding ( span, mod )

-- | Performs all the steps of register allocation, taking a map of lifetime
-- spans to a list of register pairings.
allocate :: M.Map SizedVirtualRegister LifetimeSpan -> [RegisterPairing]
allocate s = runST $ do
    is <- (buildConflicts <$> (lifetimesToIntervals s))
    preallocateFixed is
    pairs <- allocate' is
    extractCells pairs

-- | Transforms a list of lifetime spans into a priority queue of intervals.
-- This priority queue is ordered by start time of lifetime spans. The values
-- are pairings between virtual registers and memory cells that will later
-- contain the allocated memory location (it starts out as unassigned).
lifetimesToIntervals
    :: M.Map SizedVirtualRegister LifetimeSpan
    -> ST s (PQ.MinPQueue LifetimeSpan (RegisterPairingST s))
lifetimesToIntervals = M.foldlWithKey' (\acc vreg span -> do
        acc' <- acc
        hreg <- newSTRef Unassigned
        pure $ PQ.insert span (RegisterPairingST vreg hreg) acc')
    (pure PQ.empty)

-- | Constructs a list of conflicts for every interval.
--
-- Warning: this is extremely naive (quadratic time) but thankfully we only
-- need to do it once.
buildConflicts
    :: (PQ.MinPQueue LifetimeSpan (RegisterPairingST s))
    -> (PQ.MinPQueue LifetimeSpan (RegisterPairingST s, [RegisterInterval s]))
buildConflicts pq = PQ.mapWithKey (\ourSpan us ->
    (us, PQ.assocsU $ PQ.filterWithKey
        (\theirSpan them -> us /= them && ourSpan `overlaps` theirSpan) pq)) pq

-- | Performs register allocation linearly. Returned is a list of pairings
-- between virtual registers and (cells containing) physical ones.
allocate'
    :: (PQ.MinPQueue LifetimeSpan (RegisterPairingST s, [RegisterInterval s]))
    -> ST s [RegisterPairingST s]
allocate' = PQ.foldlWithKey (\acc span (us, others) ->
    case _vregST us of
        -- If we are a fixed register, we've already been allocated at this point.
        (SizedRegister _ (FixedHardwareRegister _)) -> do
            acc' <- acc
            pure $ us:acc'

        (SizedRegister _ (VirtualRegister mod _)) -> do
            let allocatableRegs = allocatableRegsForMode mod
            r <- checkFree allocatableRegs others
            case r of
                -- There is a free register, so we take it.
                Right hreg -> writeSTRef (_hregST us) $ Reg hreg False
                Left (i, hloc) ->
                    -- If our span ends after the latest conflicting interval,
                    -- we spill ourselves.
                    if _end span > i then writeSTRef (_hregST us) $ Mem (-1)
                    else do
                        -- Otherwise we spill them and take their register.
                        hw <- readSTRef hloc
                        writeSTRef hloc $ Mem (-1)
                        writeSTRef (_hregST us) hw
            acc' <- acc
            pure $ us:acc') (pure [])

-- | Performs the first phase of allocation, in which lifetimes referring to
-- fixed hardware registers are automatically granted their hardware registers.
--
-- Note that it cannot be the case that two different lifetimes refer to the
-- same fixed hardware register.
--
-- Returns its argument for chaining purposes.
preallocateFixed
    :: (PQ.MinPQueue LifetimeSpan (RegisterPairingST s, [RegisterInterval s]))
    -> ST s ()
preallocateFixed = mapM_ f . map fst . PQ.elemsU where
    f us = do
        case _vregST us of
            (SizedRegister _ (VirtualRegister _ _)) -> pure ()
            (SizedRegister sz (FixedHardwareRegister hreg)) ->
                writeSTRef (_hregST us) $ Reg (SizedRegister sz hreg) True


-- | Checks if any register among the candidates is free among the given intervals.
-- The return value is either a register from the candidate list that was free,
-- or a reference to the hardware location of the virtual register whose
-- lifetime span ends the latest.
checkFree
    :: [SizedHardwareRegister]
    -> [RegisterInterval s]
    -> ST s (Either (Int, STRef s HardwareLocation) SizedHardwareRegister)
checkFree c i = do
    fakeCell <- newSTRef Unassigned
    let fakeReg = (-1, fakeCell)
    -- We carry around the register that is associated with the span that ends
    -- the latest.
    checkFree' fakeReg c i where
    checkFree' maximal [] [] = pure $ Left maximal
    checkFree' _ (x:_) [] = pure $ Right x
    checkFree' maximal candidates (x:xs) = do
        con <- readSTRef $ _hregST $ snd x
        let max' =  if (_start $ fst x) > fst maximal
                    then (_start $ fst x, _hregST $ snd x)
                    else maximal
        case con of
            Reg r fixed ->
                let candidates' = L.delete r candidates in
                -- If the register is fixed, we don't update the "maximal"
                -- parameter; this means that this reg will never be designated
                -- for spilling.
                if fixed then
                    checkFree' maximal candidates' xs
                else
                    checkFree' max' candidates' xs
            _ -> checkFree' max' candidates xs


-- | Extracts data from the register pairing cells into its pure equivalent.
extractCells :: [RegisterPairingST s] -> ST s [RegisterPairing]
extractCells = traverse (\p -> do
    h' <- readSTRef $ _hregST p
    pure (_vregST p, h'))


-- | A stateful pairing between a virtual register and a hardware location.
-- It carries state around so that updating the hardware location is immediately
-- propagated to other instances of the pairing.
data RegisterPairingST s =
    RegisterPairingST
    { _vregST :: SizedVirtualRegister
    -- ^ The virtual register of this pairing.
    , _hregST :: STRef s HardwareLocation
    -- ^ A memory cell in which lives the hardware location currently assigned
    -- to this interval.
    }

-- | When we compare stateful register pairings, we are only interested in
-- which virtual register is being paired, not the actual state.
instance Eq (RegisterPairingST s) where
    a == b = _vregST a == _vregST b

-- | An interval used for allocation, combining a stateful register pairing with
-- the lifetime associated with the virtual register of the pairing.
type RegisterInterval s = (LifetimeSpan, RegisterPairingST s)

-- | The registers that can be allocated for a given access mode. Here, we are
-- not concerned about register sizes, so the registers returned have the maximal
-- size.
allocatableRegsForMode :: RegisterAccessMode -> [SizedHardwareRegister]
allocatableRegsForMode m = case m of
    IntegerMode -> allocatableIntRegs
    FloatingMode -> allocatableFloatRegs

-- | Every integer register can be allocated except @rax@, which is designated
-- as scratch and return value register.
allocatableIntRegs :: [SizedHardwareRegister]
allocatableIntRegs = map ((SizedRegister Extended64) . IntegerHwRegister)
    [ Rbx
    , Rcx
    , Rdx
    , Rbp
    , Rsi
    , Rdi
    , Rsp
    , R8
    , R9
    , R10
    , R11
    , R12
    , R13
    , R14
    , R15
    ]

-- | Every floating-point register can be allocated except @xmm0@.
allocatableFloatRegs :: [SizedHardwareRegister]
allocatableFloatRegs = map
    ((SizedRegister Extended64) . FloatingHwRegister  . FloatingRegister)
    [1..15]
