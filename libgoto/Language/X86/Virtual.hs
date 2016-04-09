{-|
Module      : Language.X86.Virtual
Description : X86 assembly with virtual registers
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Monad that represents an abstract form of x86 with labels.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.X86.Virtual
(
) where

import Language.X86.Core

import Control.Monad.State

-- | A virtual register is either a fixed hardware register (e.g. for accessing
-- variables on the stack, we /must/ use @rbp@) or a truly virtual register
-- that can be assigned to any general-purpose register.
data VirtualRegister
    = VirtualRegister RegisterAccessMode Int
    -- ^ Virtual registers are associated with unique identifiers.
    | FixedHardwareRegister HardwareRegister
    deriving (Eq, Ord, Read, Show)

-- | A virtual register together with a size modifier.
type SizedVirtualRegister = SizedRegister VirtualRegister

type VirtualOperand = Operand SizedVirtualRegister

data VirtualRegisterAllocatorState
    = VirtualRegisterAllocatorState
        { _nextVirtualRegister :: Int
        }
    deriving (Eq, Ord, Read, Show)

newtype VirtualRegisterAllocatorT m a
    = VirtualRegisterAllocatorT
        { runVirtualRegisterAllocatorT
            :: StateT VirtualRegisterAllocatorState m a
        }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState VirtualRegisterAllocatorState
        , MonadTrans
        )

-- | Gets a fresh virtual register
freshVirtualRegister
    :: Monad m
    => RegisterAccessMode
    -> RegisterSize
    -> VirtualRegisterAllocatorT m SizedVirtualRegister
freshVirtualRegister mode sz = do
    i <- gets _nextVirtualRegister <* modify (\s ->
        s { _nextVirtualRegister = _nextVirtualRegister s + 1 })
    pure $ SizedRegister sz (VirtualRegister mode i)
