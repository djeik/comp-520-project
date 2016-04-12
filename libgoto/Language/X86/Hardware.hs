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
    -- ^ A memory cell in which lives the hardware register currently assigned
    -- to this interval.
    }

-- | Small helper
data HardwareLocation =
      Reg SizedHardwareRegister
    | Mem
    | Unassigned

type RegisterPairing = (SizedVirtualRegister, SizedHardwareRegister)

type RegisterInterval s = (LifetimeSpan, RegisterPairingST s)
