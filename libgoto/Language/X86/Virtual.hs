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
, VirtualAsmT
, VirtualRegister(..)
, SizedVirtualRegister
, VirtualOperand
  -- * Virtual register allocator
, VirtualRegisterAllocatorT
, VirtualRegisterAllocator
, MonadVirtualRegisterAllocator(..)
, runVirtualRegisterAllocatorT
) where

import Language.Common.Pretty
import Language.X86.Core

import Control.Monad.Identity
import Control.Monad.State
import Text.PrettyPrint as P

type VirtualAsmT label = AsmT SizedVirtualRegister label

type VirtualAsm label = Asm SizedVirtualRegister label

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

instance Pretty VirtualRegister where
    pretty vreg = case vreg of
        VirtualRegister ram i ->
            let rp = text $ case ram of IntegerMode -> "i" ; FloatingMode -> "f"
            in text "v" <> rp <> P.int i
        FixedHardwareRegister r -> pretty r

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

    freshVirtualRegister m sz = do
        i <- gets _nextVirtualRegister <* modify (\s ->
            s { _nextVirtualRegister = _nextVirtualRegister s + 1 })
        pure $ SizedRegister sz (VirtualRegister m i)

class MonadVirtualRegisterAllocator m where
    -- | Gets a fresh virtual register
    freshVirtualRegister
        :: RegisterAccessMode
        -> RegisterSize
        -> m SizedVirtualRegister
