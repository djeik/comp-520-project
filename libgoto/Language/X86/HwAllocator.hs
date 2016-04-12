{-|
Module      : Language.X86.HwAllocator
Description : Definitions for the hardware register allocator.
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

The register allocator turns virtual assembly instruction into hardware assembly
instructions; all references to virtual registers are eliminated and replaced
with references to hardware registers.

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

module Language.X86.HwAllocator where

import Language.X86.Core
import Language.X86.Hardware
import Language.X86.Lifetime
import Language.X86.Virtual

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.PQueue.Prio.Min as PQ

import Control.Monad.ST
import Data.STRef

lifetimesToIntervals
    :: M.Map SizedVirtualRegister LifetimeSpan
    -> ST s (PQ.MinPQueue LifetimeSpan (RegisterPairingST s))
lifetimesToIntervals = M.foldlWithKey' (\acc vreg span -> do
        acc' <- acc
        hreg <- newSTRef Unassigned
        pure $ PQ.insert span (RegisterPairingST vreg hreg) acc')
    (pure PQ.empty)

buildConflicts
    :: (PQ.MinPQueue LifetimeSpan (RegisterPairingST s))
    -> ST s (PQ.MinPQueue LifetimeSpan (RegisterPairingST s, [RegisterInterval s]))
buildConflicts = undefined

allocate
    :: (PQ.MinPQueue LifetimeSpan (RegisterPairingST s, [RegisterInterval s]))
    -> ST s [RegisterPairingST s]
allocate = PQ.foldlWithKey (\acc span (us, others) -> do
    -- TODO must use the acceptable candidate list based on the register access kind
    r <- checkFree undefined others
    case r of
        Right hreg -> writeSTRef (_hregST us) $ Reg hreg
        Left (i, vreg) ->
            if _end span > i then writeSTRef (_hregST us) Mem
            else do
                hw <- spillDesignated vreg others
                writeSTRef (_hregST us) hw
    acc' <- acc
    pure $ us:acc') (pure [])

spillDesignated :: SizedVirtualRegister -> [RegisterInterval s] -> ST s HardwareLocation
spillDesignated r [] = error "Unimplemented: spill failure"
spillDesignated r (x:xs) =
    if (_vregST $ snd x) == r
    then do
        hloc <- readSTRef (_hregST $ snd x)
        writeSTRef (_hregST $ snd x) Mem
        pure hloc
    else spillDesignated r xs

checkFree
    :: [SizedHardwareRegister]
    -> [RegisterInterval s]
    -> ST s (Either (Int, SizedVirtualRegister) SizedHardwareRegister)
checkFree = let fakeReg = (0, SizedRegister Extended64 $ VirtualRegister IntegerMode (-1)) in
    checkFree' fakeReg where
    checkFree' maximal [] [] = pure $ Left maximal
    checkFree' _ (x:xs) [] = pure $ Right x
    checkFree' maximal candidates (x:xs) = do
        con <- readSTRef $ _hregST $ snd x
        let max' =  if (_start $ fst x) > fst maximal
                    then (_start $ fst x, _vregST $ snd x)
                    else maximal
        case con of
            Reg r -> checkFree' max' (L.delete r candidates) xs
            _ -> checkFree' max' candidates xs

