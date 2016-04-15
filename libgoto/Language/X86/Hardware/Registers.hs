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

fixedIntReg64 :: IntegerRegister -> HardwareOperand label
fixedIntReg64
    = Register
    . Direct
    . SizedRegister Extended64
    . IntegerHwRegister

rax :: HardwareOperand label
rax = fixedIntReg64 Rax

rbx :: HardwareOperand label
rbx = fixedIntReg64 Rbx

rcx :: HardwareOperand label
rcx = fixedIntReg64 Rcx

rdx :: HardwareOperand label
rdx = fixedIntReg64 Rdx

rsp :: HardwareOperand label
rsp = fixedIntReg64 Rsp

rbp :: HardwareOperand label
rbp = fixedIntReg64 Rbp

rsi :: HardwareOperand label
rsi = fixedIntReg64 Rsi

rdi :: HardwareOperand label
rdi = fixedIntReg64 Rdi

r8 :: HardwareOperand label
r8 = fixedIntReg64 R8

r9 :: HardwareOperand label
r9 = fixedIntReg64 R9

r10 :: HardwareOperand label
r10 = fixedIntReg64 R10

r11 :: HardwareOperand label
r11 = fixedIntReg64 R11

r12 :: HardwareOperand label
r12 = fixedIntReg64 R12

r13 :: HardwareOperand label
r13 = fixedIntReg64 R13

r14 :: HardwareOperand label
r14 = fixedIntReg64 R14

r15 :: HardwareOperand label
r15 = fixedIntReg64 R15

