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

xmm :: Int -> VirtualOperand addr label
xmm = DirectRegister . SizedRegister Extended64 . FixedHardwareRegister . hwxmm
