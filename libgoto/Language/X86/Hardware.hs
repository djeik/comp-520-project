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
( -- * Hardware assembly
  HardwareAsm
, HardwareOperand
  -- * Hardware register allocation
, SizedHardwareRegister
, RegisterPairingST(..)
, RegisterPairing
, RegisterInterval
, HardwareLocation(..)
, allocatableRegsForMode
) where

import Language.X86.Core
import Language.X86.Lifetime
import Language.X86.Virtual


import Data.STRef

type HardwareAsm addr label = Asm SizedHardwareRegister addr label

type SizedHardwareRegister = SizedRegister HardwareRegister

type HardwareOperand = Operand SizedHardwareRegister


data RegisterPairingST s =
    RegisterPairingST
    { _vregST :: SizedVirtualRegister
    -- ^ The virtual register corresponding to this interval
    , _hregST :: STRef s HardwareLocation
    -- ^ A memory cell in which lives the hardware location currently assigned
    -- to this interval.
    }

instance Eq (RegisterPairingST s) where
    a == b = _vregST a == _vregST b

-- | The possible locations for a virtual register.
data HardwareLocation =
      Reg SizedHardwareRegister Bool
      -- ^ A hardware register. A flag indicates whether this location is fixed
      -- or not. If it is fixed, it will never be spilled.
    | Mem
    | Unassigned

type RegisterPairing = (SizedVirtualRegister, SizedHardwareRegister)

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
