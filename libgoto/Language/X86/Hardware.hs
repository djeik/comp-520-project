{-|
Module      : Language.X86.Hardware
Description : Datatypes specific to the hardware of the x86
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
The mirror image of "Language.X86.Virtual", except with hardware registers.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.X86.Hardware
( -- * Hardware assembly definitions
  HardwareAsm
, HardwareAsmT
, HardwareOperand
 -- * Register definitions
, SizedHardwareRegister
, HardwareLocation(..)
, RegisterPairing
, safeRegisters
, scratchRegisters
 -- * Translation
, HardwareTranslation
, HardwareTranslationT(..)
, HardwareTranslationState(..)
, HardwareTranslationError(..)
, runHardwareTranslationT
, runHardwareTranslation
) where

import Language.X86.Core
import Language.X86.Virtual

import qualified Data.Set as S

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State

type HardwareAsm label = Asm SizedHardwareRegister label

type HardwareAsmT label = AsmT SizedHardwareRegister label

type HardwareOperand = Operand SizedHardwareRegister

type SizedHardwareRegister = SizedRegister HardwareRegister

-- | Indicates the hardware location of a virtual register
type RegisterPairing = (SizedVirtualRegister, HardwareLocation)

-- | The possible locations for a virtual register.
data HardwareLocation =
      Reg SizedHardwareRegister Bool
      -- ^ A hardware register. A flag indicates whether this location is fixed
      -- or not. If it is fixed, it will never be spilled.
    | Mem Int
    -- ^ A memory location. Concretely, this indicates an offset on the stack.
    | Unassigned
    -- ^ Indicates that this virtual register was not yet assigned a location.
    deriving (Show, Eq)

data HardwareTranslationState label
    = HardwareTranslationState
        { _currentSpillOffset :: Int
        -- ^ At which offset should we put the next spilled variable.
        , _safeRegistersUsed :: S.Set SizedHardwareRegister
        -- ^ A list of the safe registers that have been allocated.
        , _ip :: Int
        -- ^ The current _virtual_ program point we are at.
        , _latestSavedRegisters :: [SizedHardwareRegister]
        -- ^ Indicates, in order, which are the registers that were saved before
        -- the last call.
        }

initialHardwareTranslationState :: HardwareTranslationState label
initialHardwareTranslationState
    = HardwareTranslationState
        { _currentSpillOffset = 0
        , _safeRegistersUsed = S.empty
        , _ip = 0
        , _latestSavedRegisters = []
        }

-- | A monad for virtual-to-hardware translation.
newtype HardwareTranslationT label m a
    = HardwareTranslationT
        { unHardwareTranslationT
            :: ExceptT HardwareTranslationError (
                StateT (HardwareTranslationState label) m
            ) a
        }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState (HardwareTranslationState label)
        , MonadError HardwareTranslationError
        )

runHardwareTranslationT
    :: Monad m
    => HardwareTranslationT label m a
    -> m (Either HardwareTranslationError a)
runHardwareTranslationT
    = flip evalStateT initialHardwareTranslationState
    . runExceptT
    . unHardwareTranslationT

runHardwareTranslation
    :: HardwareTranslation label a
    -> Either HardwareTranslationError a
runHardwareTranslation = runIdentity . runHardwareTranslationT

type HardwareTranslation label a = HardwareTranslationT label Identity a

data HardwareTranslationError
    = InvariantViolation String
    deriving (Eq, Ord, Read, Show)

-- | The registers that are marked as safe (or, in ABI parlance, as "belonging
-- to the calling function"), and which must therefore be saved and restored by
-- every function at entry and exit.
--
-- Note that @rbp@ is also safe, but is saved and restored in the function
-- prologue/epilogue.
safeRegisters :: [SizedHardwareRegister]
safeRegisters = map ((SizedRegister Extended64) . IntegerHwRegister)
    [ Rbx
    , R12
    , R13
    , R14
    , R15
    ]

-- | The registers that are marked as scratch (or, in ABI parlance, as
-- "belonging to the called function"), and which must must therefore be saved
-- and restored before and after calls.
--
-- Note that @rax@ and @xmm0@ are also scratch, but are never allocated outright
-- in order to have a register available for register-only operations.
scratchRegisters :: [SizedHardwareRegister]
scratchRegisters = (map ((SizedRegister Extended64) . IntegerHwRegister)
    [ Rcx
    , Rdx
    , Rsi
    , Rdi
    , R8
    , R9
    , R10
    , R11
    ])
    ++
    (map ((SizedRegister Extended64) . FloatingHwRegister  . FloatingRegister)
    [1..15])
