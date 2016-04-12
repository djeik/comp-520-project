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
( -- * Virtual assembly
  VirtualAsm
, VirtualRegister(..)
, SizedVirtualRegister
, VirtualOperand
  -- * Virtual register allocator
, VirtualRegisterAllocatorT
, VirtualRegisterAllocator
, runVirtualRegisterAllocatorT
  -- ** Virtual registers
, freshVirtualRegister
  -- ** Fixed hardware registers
, rax
, rbx
, rcx
, rdx
, rsp
, rbp
, rsi
, rdi
, r8
, r9
, r10
, r11
, r12
, r13
, r14
, r15
) where

import Language.X86.Core

import Control.Monad.Identity
import Control.Monad.State

type VirtualAsm addr label = Asm SizedVirtualRegister addr label

-- | A virtual register together with a size modifier.
type SizedVirtualRegister = SizedRegister VirtualRegister

-- | A virtual register is either a fixed hardware register (e.g. for accessing
-- variables on the stack, we /must/ use @rbp@) or a truly virtual register
-- that can be assigned to any general-purpose register.
data VirtualRegister
    = VirtualRegister RegisterAccessMode Int
    -- ^ Virtual registers are associated with unique identifiers.
    | FixedHardwareRegister HardwareRegister
    deriving (Eq, Ord, Read, Show)

-- | An operand in virtual-register assembly code uses sized virtual registers.
type VirtualOperand = Operand SizedVirtualRegister

-- | A monad transformer to imbue a monad with virtual register allocator
-- capabilities.
newtype VirtualRegisterAllocatorT m a
    = VirtualRegisterAllocatorT
        { unVirtualRegisterAllocatorT
            :: StateT VirtualRegisterAllocatorState m a
        }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState VirtualRegisterAllocatorState
        , MonadTrans
        )

runVirtualRegisterAllocatorT
    :: Monad m
    => VirtualRegisterAllocatorT m a
    -> m a
runVirtualRegisterAllocatorT v
    = evalStateT (unVirtualRegisterAllocatorT v) initialVirtualRegisterAllocatorState

-- | A convenient synonym.
type VirtualRegisterAllocator = VirtualRegisterAllocatorT Identity

-- | The state of the virtual register allocator just keeps track of how many
-- registers have been allocated.
data VirtualRegisterAllocatorState
    = VirtualRegisterAllocatorState
        { _nextVirtualRegister :: Int
        }
    deriving (Eq, Ord, Read, Show)

-- | The initial state for the virtual register allocator.
initialVirtualRegisterAllocatorState :: VirtualRegisterAllocatorState
initialVirtualRegisterAllocatorState
    = VirtualRegisterAllocatorState
        { _nextVirtualRegister = 0
        }

instance
    Monad m
    => MonadVirtualRegisterAllocator (VirtualRegisterAllocatorT m) where

    freshVirtualRegister mode sz = do
        i <- gets _nextVirtualRegister <* modify (\s ->
            s { _nextVirtualRegister = _nextVirtualRegister s + 1 })
        pure $ SizedRegister sz (VirtualRegister mode i)

class MonadVirtualRegisterAllocator m where
    -- | Gets a fresh virtual register
    freshVirtualRegister
        :: RegisterAccessMode
        -> RegisterSize
        -> m SizedVirtualRegister

fixedIntReg64 :: IntegerRegister -> VirtualOperand addr label
fixedIntReg64
    = DirectRegister
    . SizedRegister Extended64
    . FixedHardwareRegister
    . IntegerHwRegister

rax :: VirtualOperand addr label
rax = fixedIntReg64 Rax

rbx :: VirtualOperand addr label
rbx = fixedIntReg64 Rbx

rcx :: VirtualOperand addr label
rcx = fixedIntReg64 Rcx

rdx :: VirtualOperand addr label
rdx = fixedIntReg64 Rdx

rsp :: VirtualOperand addr label
rsp = fixedIntReg64 Rsp

rbp :: VirtualOperand addr label
rbp = fixedIntReg64 Rbp

rsi :: VirtualOperand addr label
rsi = fixedIntReg64 Rsi

rdi :: VirtualOperand addr label
rdi = fixedIntReg64 Rdi

r8 :: VirtualOperand addr label
r8 = fixedIntReg64 R8

r9 :: VirtualOperand addr label
r9 = fixedIntReg64 R9

r10 :: VirtualOperand addr label
r10 = fixedIntReg64 R10

r11 :: VirtualOperand addr label
r11 = fixedIntReg64 R11

r12 :: VirtualOperand addr label
r12 = fixedIntReg64 R12

r13 :: VirtualOperand addr label
r13 = fixedIntReg64 R13

r14 :: VirtualOperand addr label
r14 = fixedIntReg64 R14

r15 :: VirtualOperand addr label
r15 = fixedIntReg64 R15
