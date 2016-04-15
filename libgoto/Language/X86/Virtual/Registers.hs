{-|
Module      : Language.X86.Virtual.Registers
Description : Definitions of the virtual registers
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

module Language.X86.Virtual.Registers where

import Language.X86.Core
import Language.X86.Virtual

fixedIntReg64 :: IntegerRegister -> VirtualOperand label
fixedIntReg64
    = Register
    . Direct
    . SizedRegister Extended64
    . FixedHardwareRegister
    . IntegerHwRegister

rax :: VirtualOperand label
rax = fixedIntReg64 Rax

rbx :: VirtualOperand label
rbx = fixedIntReg64 Rbx

rcx :: VirtualOperand label
rcx = fixedIntReg64 Rcx

rdx :: VirtualOperand label
rdx = fixedIntReg64 Rdx

rsp :: VirtualOperand label
rsp = fixedIntReg64 Rsp

rbp :: VirtualOperand label
rbp = fixedIntReg64 Rbp

rsi :: VirtualOperand label
rsi = fixedIntReg64 Rsi

rdi :: VirtualOperand label
rdi = fixedIntReg64 Rdi

r8 :: VirtualOperand label
r8 = fixedIntReg64 R8

r9 :: VirtualOperand label
r9 = fixedIntReg64 R9

r10 :: VirtualOperand label
r10 = fixedIntReg64 R10

r11 :: VirtualOperand label
r11 = fixedIntReg64 R11

r12 :: VirtualOperand label
r12 = fixedIntReg64 R12

r13 :: VirtualOperand label
r13 = fixedIntReg64 R13

r14 :: VirtualOperand label
r14 = fixedIntReg64 R14

r15 :: VirtualOperand label
r15 = fixedIntReg64 R15

xmm :: Int -> VirtualOperand label
xmm = Register . Direct . SizedRegister Extended64 . FixedHardwareRegister . hwxmm
