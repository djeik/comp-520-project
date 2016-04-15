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

{-# LANGUAGE ScopedTypeVariables #-}

module Language.X86.HwTranslator where

import Language.Common.Storage
import Language.GoLite.Misc
import Language.X86.Core
import Language.X86.Hardware
import Language.X86.Hardware.Registers
import Language.X86.Lifetime
import Language.X86.Virtual

import Control.Monad ( void )
import Control.Monad.Free
import Control.Monad.Except
import Control.Monad.State

import qualified Data.Map.Strict as M

asm :: Monad m => m a -> HardwareTranslationT m a
asm = HardwareTranslationT . lift . lift

translate
    :: forall addr label
    . M.Map SizedVirtualRegister HardwareLocation
    -> [(SizedHardwareRegister, LifetimeSpan)]
    -> Int
    -> VirtualAsm addr label ()
    -> HardwareTranslation addr label ()
translate vToH live stkSz = iterM phi where
    phi :: AsmF addr label (Operand SizedVirtualRegister addr label) (HardwareTranslation addr label ())
        -> HardwareTranslation addr label ()
    phi a = case a of

        -- Pass through here
        Here f -> do
            h <- asm here
            f h

        -- Pass through newLabel
        NewLabel f -> do
            l <- asm newLabel
            f l

        -- Pass through setLabel
        SetLabel l ad n -> do
            asm $ setLabel l ad
            n

        Prologue di n -> case di of

            -- Function prologue: make space on the stack for spills, push
            -- safe registers that are in use throughout this function.
            Save -> do
                off <- gets _currentSpillOffset
                pregs <- gets _safeRegistersUsed
                asm $ sub rsp (Immediate $ ImmI $ fromIntegral $ -(off - stkSz))
                void $ forM pregs (\(SizedRegister _ reg) -> case reg of
                        FloatingHwRegister _ -> throwError $ InvariantViolation
                            "No floating register is safe"
                        IntegerHwRegister r -> asm $ push $ fixedIntReg64 r
                    )
                n

            -- Mirror image of Prologue Save (pop safe registers, clear the
            -- space on the stack.
            Load -> do
                off <- gets _currentSpillOffset
                pregs <- gets _safeRegistersUsed

                -- Obviously, this popping needs to be done in reverse.
                void $ forM (reverse pregs) (\(SizedRegister _ reg) -> case reg of
                        FloatingHwRegister _ -> throwError $ InvariantViolation
                            "No floating register is safe"
                        IntegerHwRegister r -> asm $ pop $ fixedIntReg64 r
                    )

                asm $ add rsp (Immediate $ ImmI $ fromIntegral $ -(off - stkSz))
                n

        Scratch di n ->
            case di of
                -- On a scratch save, push all the scratch registers that are
                -- currently live.
                Save -> do
                    i <- gets _ip
                    let sr = getLiveScratch i live
                    modify $ \s -> s { _latestSavedRegisters = sr }
                    void $ forM sr (\r -> asm $ push $ DirectRegister r)
                    n

                -- Mirror: pop in reverse the registers we had saved before.
                Load -> do
                    sr <- gets _latestSavedRegisters
                    void $ forM (reverse sr) (\r -> asm $ pop $ DirectRegister r)
                    n

        Emit i n -> do
            translateInst i
            modify $ \s -> s {_ip = _ip s + 1}
            n

    translateInst i = case i of

        Ret -> asm ret

        Mov v1 v2 -> undefined

        Call v -> do
            movIfIndirectSpilled v
            v' <- translateOp v
            asm $ call v'

        Add v1 v2 -> undefined

        Sub v1 v2 -> undefined

        Mul s v1 v2 mv3 -> undefined

        Xor v1 v2 -> undefined

        Inc v -> do
            movIfIndirectSpilled v
            v' <- translateOp v
            asm $ inc v'

        Dec v -> do
            movIfIndirectSpilled v
            v' <- translateOp v
            asm $ dec v'

        Push v -> do
            movIfIndirectSpilled v
            v' <- translateOp v
            asm $ push v'

        Pop v -> do
            movIfIndirectSpilled v
            v' <- translateOp v
            asm $ pop v'

        Nop -> asm nop

        Syscall -> asm syscall

        Int v -> do
            movIfIndirectSpilled v
            v' <- translateOp v
            asm $ int v'

        Cmp v1 v2 -> undefined

        Test v1 v2 -> undefined

        Sal v1 v2 -> undefined

        Sar v1 v2 -> undefined

        Jump cond v -> do
            movIfIndirectSpilled v
            v' <- translateOp v
            asm $ jump cond v'

        Setc cond v -> do
            movIfIndirectSpilled v
            v' <- translateOp v
            asm $ setc cond v'

        Neg1 v -> do
            movIfIndirectSpilled v
            v' <- translateOp v
            asm $ neg1 v'

        Neg2 v -> do
            movIfIndirectSpilled v
            v' <- translateOp v
            asm $ neg2 v'


    {- TODO: Need a function such that it will take the two operands, check whether
    they are both indirects, and if yes, will do the following:

        mov [rcx + 2], [rdx + 1] ===>

        mov rax, [rdx + 1]
        mov [rcx + 2], rax

        In general, the pattern being:
            mov rax, [src]
            inst [dst], rax

        and otherwise just emit the normal instruction

        But now suppose we had:
        mov [vreg2 + 6], [rcx + 2]
        and vreg2 got spilled to [rbp - 4]


        What you WANT is:

            mov rax, [rbp - 4]
            mov [rax + 6], [rcx + 2]

        And now we're fucked again. Or are we???
        Yes. There's no way around it.
        To fix, we need to free some register.

        So we got
            mov [vreg2 + 6], [rcx + 2] ; vreg2 is now [rbp - 4]

            Make that into:

            push rbx
            mov rbx, [rcx + 2] ; put the source into rbx
            mov rax, [rbp - 4] ; get the dest address
            mov [rax + 6], rbx ; do the ACTUAL MOVE
            pop rbx ; restore rbx
    -}



    translateOp
        :: VirtualOperand addr label
        -> HardwareTranslation addr label (HardwareOperand addr label)
    translateOp o = case o of
        -- Any non-reg operand is just passed as-is. We have to do this boilerplate
        -- for typechecking reasons.
        Address a -> pure $ Address a
        Label l -> pure $ Label l
        Internal s -> pure $ Internal s
        External s -> pure $ External s

        (DirectRegister r) -> translateReg r

        {-
        If the original register was an offset (e.g. [vreg56 + 12]), we need to
        verify whether it was spilled or not. If it was, then vreg56 is in fact
        [ebp - xx]. Therefore, we need to load THAT into some register and do
        the original offset from that register. So if vreg56 was spilled to
        offset -8, the instruction "inst [vreg56 + 12]" really becomes:

            mov rax, [ebp - 8]
            inst [rax + 12]

        Luckily, no instruction is allowed to take two indirects, so we will
        always have rax at our disposal.

        The code to generate the mov is in a different function
        (movIfIndirectSpilled). The code here will just replace the spilled
        register with an indirect reference to rax with the same offset.
        -}
        (IndirectRegister off) -> case off of
            Offset d r -> case M.lookup r vToH of
                Just loc -> case loc of
                    Reg r' _ -> pure $ DirectRegister r'
                    Mem i -> pure $ IndirectRegister $ Offset d $
                            SizedRegister Extended64 $ IntegerHwRegister Rax
                    Unassigned -> throwError $ InvariantViolation
                        "Virtual register has not been assigned"
                Nothing -> throwError $ InvariantViolation
                    "Virtual register with no corresponding hardware location"


    {- If the given virtual operand represents an indirect reference to a spilled
    register, generates a mov from the spill location to rax, so that we can
    access the indirection. -}
    movIfIndirectSpilled
        :: VirtualOperand addr label
        -> HardwareTranslation addr label ()
    movIfIndirectSpilled o = case o of
        IndirectRegister off -> case off of
            Offset d r -> case M.lookup r vToH of
                Just loc -> case loc of
                    Reg _ _ -> pure ()
                    Mem i -> asm $ mov rax $ spillOperand i
                    Unassigned -> throwError $ InvariantViolation
                        "Virtual register has not been assigned"
                Nothing -> throwError $ InvariantViolation
                    "Virtual register with no corresponding hardware location"
        _ -> pure ()

    translateReg
        :: SizedVirtualRegister
        -> HardwareTranslation addr label (HardwareOperand addr label)
    translateReg r = case M.lookup r vToH of
        Just loc -> case loc of
            Reg r' _ -> pure $ DirectRegister r'
            Mem i -> pure $ spillOperand i
            Unassigned -> throwError $ InvariantViolation
                "Virtual register has not been assigned"
        Nothing -> throwError $ InvariantViolation
            "Virtual register with no corresponding hardware location"

    -- Make sure that the values obey a certain predicate
    -- If they do, translate the operands
    -- If they don't, add some funky stuff.


-- | Given an offset, constructs the operand to access the spill location
-- associated with it.
spillOperand :: Int -> HardwareOperand addr label
spillOperand i = IndirectRegister $ Offset (fromIntegral i) $
                SizedRegister Extended64 $ IntegerHwRegister Rbp

-- | Obtains all the unsafe registers that are live at the given program point.
getLiveScratch
    :: Int
    -> [(SizedHardwareRegister, LifetimeSpan)]
    -> [SizedHardwareRegister]
getLiveScratch i = map fst . filter (\(h, l) ->
    h `elem` scratchRegisters -- The register must be scratch.
    && i >= _start l && i <= _end l) -- Its lifetime must encompass the ip.


computeAllocState :: [RegisterPairing] -> HardwareTranslation addr label [RegisterPairing]
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

