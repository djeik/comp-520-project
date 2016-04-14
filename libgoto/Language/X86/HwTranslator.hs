{-|
Module      : Language.X86.HwTranslator
Description : Virtual Asm to Hardware Asm translator
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

This module is reponsible for the translation between virtual and hardware
assembly. The main steps of this translation are:

    * Compute lifetimes for every virtual register in the program (see
      "Language.X86.Lifetime")
    * Allocate hardware locations for every virtual register (see
      "Language.X86.HwAllocator")
-}

module Language.X86.HwTranslator where

import Language.Common.Storage
import Language.X86.Core
import Language.X86.Hardware
import Language.X86.Virtual

import Control.Monad.Except
import Control.Monad.State


computeAllocState :: [RegisterPairing] -> HardwareTranslation [RegisterPairing]
computeAllocState = foldl (\acc (v, h) -> do
    acc' <- acc
    case h of
        Unassigned -> throwError $ InvariantViolation "A hardware location should\
                                                        \ have been assigned"
        Mem _ -> do
            let space = storageSize $ getVRegSize v
            off <- gets _currentSpillOffset
            modify $ \s -> s { _currentSpillOffset = off - space}
            pure $ (v, Mem space):acc'

        Reg r _ -> do
            when (r `elem` safeRegisters)
                $ modify $ \s -> s { _safeRegistersUsed = r:(_safeRegistersUsed s) }
            pure $ (v, h):acc'
    ) (pure [])

getVRegSize :: SizedVirtualRegister -> RegisterSize
getVRegSize (SizedRegister sz _) = sz

