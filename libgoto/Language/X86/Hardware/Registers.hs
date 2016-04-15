{-|
Module      : Language.X86.Hardware.Registers
Description : Definitions of the hardware registers
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

module Language.X86.Hardware.Registers where

import Language.X86.Core
import Language.X86.Hardware

fixedIntReg64 :: IntegerRegister -> HardwareOperand addr label
fixedIntReg64
    = DirectRegister
    . SizedRegister Extended64
    . IntegerHwRegister

rax :: HardwareOperand addr label
rax = fixedIntReg64 Rax

rbx :: HardwareOperand addr label
rbx = fixedIntReg64 Rbx

rcx :: HardwareOperand addr label
rcx = fixedIntReg64 Rcx

rdx :: HardwareOperand addr label
rdx = fixedIntReg64 Rdx

rsp :: HardwareOperand addr label
rsp = fixedIntReg64 Rsp

rbp :: HardwareOperand addr label
rbp = fixedIntReg64 Rbp

rsi :: HardwareOperand addr label
rsi = fixedIntReg64 Rsi

rdi :: HardwareOperand addr label
rdi = fixedIntReg64 Rdi

r8 :: HardwareOperand addr label
r8 = fixedIntReg64 R8

r9 :: HardwareOperand addr label
r9 = fixedIntReg64 R9

r10 :: HardwareOperand addr label
r10 = fixedIntReg64 R10

r11 :: HardwareOperand addr label
r11 = fixedIntReg64 R11

r12 :: HardwareOperand addr label
r12 = fixedIntReg64 R12

r13 :: HardwareOperand addr label
r13 = fixedIntReg64 R13

r14 :: HardwareOperand addr label
r14 = fixedIntReg64 R14

r15 :: HardwareOperand addr label
r15 = fixedIntReg64 R15

