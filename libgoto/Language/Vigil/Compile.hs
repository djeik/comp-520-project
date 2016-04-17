{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Language.Vigil.Compile where

import Language.Common.Annotation
import Language.Common.Misc ( unFix )
import Language.GoLite.Types ( stringFromSymbol )
import Language.Vigil.Syntax as T
import Language.Vigil.Syntax.TyAnn
import Language.Vigil.Syntax.Basic
import Language.Vigil.Types
import Language.X86.Core
import Language.X86.Mangling ( mangleFuncName )
import Language.X86.Virtual
import Language.X86.Virtual.Registers

import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Foldable ( cata )
import qualified Data.Map as M
import Data.List ( foldl' )

import Debug.Trace ( trace )

newtype Compiler label a
    = Compiler
        { unCompiler
            :: ReaderT (CompilerEnv label) (
                VirtualRegisterAllocatorT (VirtualAsm label)
            ) a
        }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader (CompilerEnv label)
        )

instance MonadVirtualRegisterAllocator (Compiler label) where
    freshVirtualRegister ram rs
        = Compiler $ lift $ freshVirtualRegister ram rs

-- | The read-only environment of the function compiler.
data CompilerEnv label
    = CompilerEnv
        { _identMap :: M.Map GlobalId (RegisterAccessMode, VirtualOperand label)
        -- ^ Precomputed map that assigns each GlobalId used by the function to
        -- a virtual operand that represents that data.
        }
    deriving (Eq, Ord, Read, Show)

-- | Looks up how to access the given data.
lookupIdent
    :: GlobalId
    -> (String -> Directness String)
    -> Compiler label (RegisterAccessMode, VirtualOperand label)
lookupIdent gid dir = do
    imap <- asks _identMap
    pure $ case M.lookup gid imap of
        -- if it's not in the ident map for the function, then we need to
        -- generate a reference outside the function.
        Nothing -> case unFix $ gidTy gid of
            -- if it's a builtin, then we have to create an external reference,
            -- which will eventually be linked to the runtime.
            BuiltinType _ ->
                ( IntegerMode
                , External . Direct . stringFromSymbol $ gidOrigName gid
                )
            -- anything else is something on the top level, so we generate an
            -- internal reference
            FuncType {} ->
                ( IntegerMode
                , Internal $ Direct $ stringFromSymbol $ gidOrigName gid
                )
            _ ->
                ( IntegerMode
                , Internal $ dir $ stringFromSymbol $ gidOrigName gid
                )
        Just o -> o

-- | Emit raw assembly.
asm :: VirtualAsm label a -> Compiler label a
asm = Compiler . lift . lift

-- | Compiles a function.
runCompiler :: TyAnnFunDecl -> VirtualAsm label ()
runCompiler decl
    = runVirtualRegisterAllocatorT $ do
        env <- makeCompilerEnv decl
        runReaderT (unCompiler (compileFunction decl)) env

data CompilerEnvAllocState label
    = CompilerEnvAllocState
        { stkOffset :: Displacement
        , intregs :: [IntegerRegister]
        , floatregs :: [Int]
        , identMap :: M.Map GlobalId (RegisterAccessMode, VirtualOperand label)
        }
    deriving (Eq, Ord, Read, Show)

makeCompilerEnv
    :: forall label. TyAnnFunDecl
    -> VirtualRegisterAllocatorT (VirtualAsm label) (CompilerEnv label)
makeCompilerEnv (FunDecl { _funDeclArgs = args, _funDeclVars = vars })
    = CompilerEnv <$> is where
        is :: VirtualRegisterAllocatorT
            (VirtualAsm label)
            (M.Map GlobalId (RegisterAccessMode, VirtualOperand label))
        is = identMap <$> execStateT go initial

        go :: StateT
            (CompilerEnvAllocState label)
            (VirtualRegisterAllocatorT (VirtualAsm label))
            ()
        go = do
            forM_ vars $ \(VarDecl gid) -> do
                v <- case unFix $ gidTy gid of
                    FloatType _ -> (FloatingMode,) . Register . Direct
                        <$> lift (freshVirtualRegister FloatingMode Extended64)
                    _ -> (IntegerMode,) . Register . Direct
                        <$> lift (freshVirtualRegister IntegerMode Extended64)

                record gid v

            forM_ args $ \(VarDecl gid) -> do
                case unFix $ gidTy gid of
                    FloatType _ -> paramRegAssign nextfreg FloatingMode xmm' gid
                    _ -> paramRegAssign nextireg IntegerMode rXx gid

        paramRegAssign
            :: StateT
                (CompilerEnvAllocState label)
                (VirtualRegisterAllocatorT (VirtualAsm label))
                (Maybe a)
            -> RegisterAccessMode
            -> (a -> SizedVirtualRegister)
            -> GlobalId
            -> StateT
                (CompilerEnvAllocState label)
                (VirtualRegisterAllocatorT (VirtualAsm label))
                ()
        paramRegAssign regAllocator ram boxer gid = do
            m <- regAllocator
            case m of
                Just reg -> record gid (ram, Register . Direct $ boxer reg)
                Nothing -> do
                    offset <- nextparam (8 :: Displacement)
                    record gid (ram, Register $ Indirect (Offset offset $ rXx Rbp))

        fixhw64 = SizedRegister Extended64 . FixedHardwareRegister

        xmm' = fixhw64 . hwxmm

        rXx = fixhw64 . IntegerHwRegister

        -- the initial state for the parameter and local allocator
        initial :: CompilerEnvAllocState label
        initial = CompilerEnvAllocState
            { stkOffset = 16
            , intregs = [Rdi, Rsi, Rdx, Rcx, R8, R9]
            , floatregs = [0..7]
            , identMap = M.empty
            }

        -- safe head from a component of the state tuple
        nextreg
            :: (CompilerEnvAllocState label -> [a])
            -> ( [a]
                -> CompilerEnvAllocState label
                -> CompilerEnvAllocState label
            )
            -> StateT
                (CompilerEnvAllocState label)
                (VirtualRegisterAllocatorT (VirtualAsm label))
                (Maybe a)
        nextreg getter setter = do
            regs <- gets getter
            case regs of
                [] -> pure Nothing
                (x:xs) -> do
                    modify (setter xs)
                    pure (Just x)

        record k v = modify $ \s -> s { identMap = M.insert k v (identMap s) }

        -- get the next integer register, or 'Nothing' if there are none
        -- available
        nextireg = nextreg intregs (\x s -> s { intregs = x })

        -- get the next floating register, or 'Nothing' if there are none
        -- available
        nextfreg = nextreg floatregs (\x s -> s { floatregs = x })

        nextmemory getter setter increment = do
            n <- gets getter
            modify (setter $ n + increment)
            pure n

        nextparam = nextmemory stkOffset (\x s -> s { stkOffset = x } )

-- | Compile a function
compileFunction
    :: TyAnnFunDecl
    -> Compiler label ()
compileFunction decl = wrapFunction $ compileBody none $ _funDeclBody decl where
    none :: (Maybe label, Maybe label)
    none = (Nothing, Nothing)

    compileBody
        :: (Maybe label, Maybe label)
        -> [TyAnnStatement]
        -> Compiler label ()
    compileBody t = mapM_ (compileStmt t)

    compileStmt
        :: (Maybe label, Maybe label)
        -> TyAnnStatement
        -> Compiler label ()
    compileStmt t = ($ t) . cata f where
        f :: TyAnnStatementF ((Maybe label, Maybe label) -> Compiler label ())
            -> (Maybe label, Maybe label) -> Compiler label ()
        f stmt ml@(mEnd, mBeginning) = case stmt of
            ExprStmt expr -> void $ compileExpr expr

            CondExprStmt cond -> void $ compileCondExpr cond

            Assign (Ann ty ref) expr -> do
                (ramR, r) <- compileRef ref
                (ramE, o) <- compileExpr expr

                let copy s = do
                        mov rdi o
                        call (External $ Direct s)
                        mov r rax

                asm $ case let t = unFix ty in trace (show t) t of
                    IntType _ -> mov r o
                    FloatType _ -> undefined -- TODO
                    StringType -> copy (mangleFuncName "deepcopy_array")
                    ArrayType _ _ -> copy (mangleFuncName "deepcopy_array")
                    SliceType _ -> copy (mangleFuncName "shallowcopy_slice")
                    StructType _ _ -> copy (mangleFuncName "deepcopy_struct")

            Initialize i -> do
                let diRef = mov rdi (Internal $ Direct (stringFromSymbol (gidOrigName i) ++ "_ini"))
                let directI = lookupIdent i Direct
                let indirectI = lookupIdent i (Indirect . Offset 0)
                let cEx s i' = do
                        diRef
                        call (External $ Direct s)
                        mov i' rax

                case unFix (gidTy i) of
                    IntType _ -> do
                        (ram, i') <- indirectI
                        asm $ mov i' (Immediate $ ImmI 0)
                    FloatType _ -> undefined -- TODO
                    StringType ->
                        asm . cEx (mangleFuncName "new_array") . snd =<< directI
                    ArrayType _ _ ->
                        asm . cEx (mangleFuncName "new_array") . snd =<< directI
                    SliceType _ ->
                        asm . cEx (mangleFuncName "new_slice") . snd =<< directI

            PrintStmt vs -> forM_ vs $ \(Ann ty v) -> do
                (ram, o) <- compileRef v
                let sty = serializeType ty

                asm $ withScratch $ do
                    mov rdi (Immediate $ ImmI $ fromIntegral sty)
                    mov rsi o
                    call (External . Direct $ mangleFuncName "goprint")

            ReturnStmt (Just (Ann _ ref)) -> do
                (ram, r) <- compileRef ref
                asm $ mov rax r

            ReturnStmt Nothing -> do
                asm $ xor rax rax

            IfStmt cond thenBody me -> do
                -- sets the flags and gives us the jump variant
                j <- compileCondExpr cond
                l <- asm newLabel
                asm $ jump j (Label l)
                -- check for an else clause
                mapM_ ($ ml) thenBody
                asm $ setLabel l
                case me of
                    Nothing -> pure ()
                    Just elseBody -> mapM_ ($ ml) elseBody

            SwitchStmt
                { switchGuard = mg
                , switchCases = cs
                , switchDefaultCase = c
                } -> do
                    -- to jump out of the switch
                    switchEnd <- asm newLabel

                    -- TODO refactor this (both cases are *essentially* the
                    -- same.
                    case mg of
                        Just expr -> do
                            (ramG, g) <- compileExpr expr

                            forM_ cs $ \(hd, bd) -> do
                                postCaseBody <- asm newLabel

                                -- to jump over the case body to
                                -- the next case head
                                preCaseBody <- asm newLabel

                                -- compile the conditions to check
                                -- to enter this case
                                forM_ hd $ \(e, ec) -> do
                                    -- TODO investigate whether
                                    -- continue/break can appear in these
                                    -- weird contexts
                                    mapM_ ($ none) ec
                                    (ramE', e') <- compileExpr e
                                    asm $ do
                                        cmp e' g
                                        jump OnEqual (Label preCaseBody)

                                -- if none of the alternatives in
                                -- the case head match the guard,
                                -- then jump past the case body
                                asm $ do
                                    jump Unconditionally (Label postCaseBody)
                                    setLabel preCaseBody

                                mapM_ ($ (Just switchEnd, mBeginning)) bd

                                asm $ do
                                    jump Unconditionally (Label switchEnd)
                                    setLabel postCaseBody

                            if null c
                                -- no default case: jump past the switch
                                then asm $ jump Unconditionally (Label switchEnd)
                                else do
                                    mapM_ ($ (Just switchEnd, mBeginning)) c

                        Nothing -> do
                            forM_ cs $ \(hd, bd) -> do
                                postCaseBody <- asm newLabel
                                preCaseBody <- asm newLabel

                                forM_ hd $ \(e, ec) -> do
                                    mapM_ ($ none) ec
                                    (ramE, e') <- compileExpr e
                                    asm $ do
                                        test e' e'
                                        jump OnNotEqual (Label preCaseBody)

                                asm $ do
                                    jump Unconditionally (Label postCaseBody)
                                    setLabel preCaseBody

                                mapM_ ($ (Just switchEnd, mBeginning)) bd

                                asm $ do
                                    jump Unconditionally (Label switchEnd)
                                    setLabel postCaseBody

                            -- default case
                            if null c
                                then asm $ jump Unconditionally (Label switchEnd)
                                else do
                                    mapM_ ($ (Just switchEnd, mBeginning)) c

                    asm $ setLabel switchEnd

            ForStmt Nothing body -> do
                (forEnd, forStart) <- (,) <$> asm newLabel <*> asm newLabel
                mapM_ ($ (Just forEnd, Just forStart)) body
                asm $ setLabel forEnd

            ForStmt (Just (code, cond)) body -> do
                (forEnd, forStart) <- (,) <$> asm newLabel <*> asm newLabel

                asm $ setLabel forStart

                mapM_ ($ (Just forEnd, Just forStart)) code
                j <- compileCondExpr cond

                asm $ jump j (Label forEnd)

                mapM_ ($ (Just forEnd, Just forStart)) body

                asm $ do
                    jump Unconditionally (Label forStart)
                    setLabel forEnd

            BreakStmt -> maybe
                (error "invariant violation")
                (asm . jump Unconditionally . Label)
                mEnd

            ContinueStmt -> maybe
                (error "invariant violation")
                (asm . jump Unconditionally . Label)
                mBeginning

-- | Generates code to compute the integer absolute value of the given
-- 'VirutalOperand' in place.
integerAbs :: VirtualOperand label -> Compiler label (VirtualOperand label)
integerAbs o = do
    t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
    s <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
    asm $ do
        mov t o
        mov s o
        sar t (Immediate $ ImmI 31)
        xor s t
        sub s t
    pure s

floatingAbs :: VirtualOperand label -> Compiler label (VirtualOperand label)
floatingAbs o = undefined

-- | Generates the code to evaluate an expression. The computed
-- 'VirtualOperand' contains the result of the expression and can be accessed
-- via the returned mode.
compileExpr
    :: TyAnnExpr
    -> Compiler label (RegisterAccessMode, VirtualOperand label)
compileExpr (Ann ty e) = case e of
    Conversion dstTy (Ann srcTy ref) -> do
        (ram, o) <- compileRef ref
        case (unFix dstTy, unFix srcTy) of
            (IntType _, IntType _) -> pure (IntegerMode, o)
            (FloatType _, FloatType _) -> pure (FloatingMode, o)
            (FloatType _, IntType _) -> do
                t <- Register . Direct <$> freshVirtualRegister FloatingMode Extended64
                asm $ cvt ScalarDouble SingleInteger t o
                pure (FloatingMode, t)
            (IntType _, FloatType _) -> do
                t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ cvt SingleInteger ScalarDouble t o
                pure (IntegerMode, t)
            _ -> error "Impossible conversion"


    Binary v1 op v2 -> do
        ((ram1, r1), (ram2, r2)) <- (,) <$> compileVal v1 <*> compileVal v2

        (ram1,) <$> case op of
            Plus -> case ram1 of
                IntegerMode -> do
                    t <- Register . Direct
                        <$> freshVirtualRegister IntegerMode Extended64
                    asm $ do
                        mov t r1
                        add t r2
                    pure t
                FloatingMode -> do
                    t <- Register . Direct
                        <$> freshVirtualRegister FloatingMode Extended64
                    asm $ do
                        mov t r1
                        addsse ScalarDouble t r2
                    pure t

            Minus -> case ram1 of
                IntegerMode -> do
                    t <- Register . Direct
                        <$> freshVirtualRegister IntegerMode Extended64
                    asm $ do
                        mov t r1
                        sub t r2
                    pure t
                FloatingMode -> do
                    t <- Register . Direct
                        <$> freshVirtualRegister FloatingMode Extended64
                    asm $ do
                        mov t r1
                        subsse ScalarDouble t r2
                    pure t

            Times -> case ram1 of
                IntegerMode -> do
                    t <- Register . Direct
                        <$> freshVirtualRegister IntegerMode Extended64
                    asm $ do
                        mov t r1
                        imul t r2
                    pure t
                FloatingMode -> do
                    t <- Register . Direct
                        <$> freshVirtualRegister FloatingMode Extended64
                    asm $ do
                        mov t r1
                        mulsse ScalarDouble t r2
                    pure t

            Divide -> do
                t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov rax r1 -- low 64 bits of dividend
                    mov t r2
                    cqo rdx rax -- sign extend rax into rdx
                    idiv rdx rax t -- perform the division
                    mov t rax
                pure t

            Modulo -> do
                t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ do
                    xor rdx rdx
                    mov rax r1
                    mov t r2
                    cqo rdx rax
                    idiv rdx rax t
                    mov t rdx
                pure t

            ShiftLeft -> do
                t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov t r1
                    sal t r2
                pure t

            ShiftRight -> do
                t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov t r1
                    sar t r2
                pure t

            BitwiseAnd -> do
                t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov t r1
                    bwand t r2
                pure t

            BitwiseOr -> do
                t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov t r1
                    bwor t r2
                pure t

            BitwiseAndNot -> do
                t1 <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                t2 <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov t1 r1
                    mov t2 r2
                    neg1 t2
                    bwand t1 t2
                pure t1

            BitwiseXor -> do
                t1 <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov t1 r1
                    xor t1 r2
                pure t1

    Unary op v -> case op of
        Positive -> do
            -- compute the integer absolute value
            -- http://stackoverflow.com/questions/2639173/x86-assembly-abs-implementation
            (ram, o) <- compileVal v
            case ram of
                IntegerMode -> (IntegerMode,) <$> integerAbs o
                FloatingMode -> (FloatingMode,) <$> floatingAbs o

        Negative -> do
            (ram, o) <- compileVal v

            case ram of
                IntegerMode -> do
                    t <- Register . Direct
                        <$> freshVirtualRegister IntegerMode Extended64

                    asm $ do
                        mov t o
                        neg2 t

                    pure (IntegerMode, t)

                FloatingMode -> do
                    t <- Register . Direct
                        <$> freshVirtualRegister FloatingMode Extended64

                    asm $ do
                        mov t o
                        pxor t t

                    pure (FloatingMode, t)

        BitwiseNot -> do
            (ram, o) <- compileVal v

            case ram of
                FloatingMode -> error "invariant violation: bitwise operation on float"
                IntegerMode -> do
                    asm $ neg1 o
                    pure (IntegerMode, o)

    Ref (Ann _ r) -> compileRef r

    Cond c -> do
        j <- compileCondExpr c
        t <- Register . Direct <$> freshVirtualRegister IntegerMode Low8

        asm $ do
            mov rax (Immediate $ ImmI 0)
            setc (invertFlag j)
                $ Register
                $ Direct
                $ SizedRegister Low8
                $ FixedHardwareRegister
                $ IntegerHwRegister
                $ Rax
            mov t rax

        pure (IntegerMode, t)

    T.Call i vs -> do
        -- for functions, the access mode will always be integer
        (ram, f) <- lookupIdent i Direct

        asm $ scratch Save
        prepareCall vs
        asm $ call f
        asm $ scratch Load

        case unFix ty of
            VoidType -> pure (IntegerMode, rax)

            FloatType _ -> do
                t <- Register . Direct <$> freshVirtualRegister FloatingMode Extended64
                asm $ movq t (xmm 0)
                pure (FloatingMode, t)

            _ -> do
                t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ mov t rax
                pure (FloatingMode, t)

    InternalCall name vs -> do
        asm $ scratch Save
        prepareCall vs
        asm $ call (External . Direct $ name)
        asm $ scratch Load

        case unFix ty of
            FloatType _ -> do
                t <- Register . Direct <$> freshVirtualRegister FloatingMode Extended64
                asm $ movq t (xmm 0)
                pure (FloatingMode, t)

            _ -> do
                t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ mov t rax
                pure (IntegerMode, t)

-- | Compiles a conditional expression. These translate to comparisons in x86,
-- which set flags, so the only thing that a called would need to know is which
-- jump variant to invoke in order to perform the correct branch.
compileCondExpr
    :: TyAnnCondExpr
    -> Compiler label FlagCondition
compileCondExpr e = case e of
    CondRef (Ann _ ref) -> do
        (ram, r) <- compileRef ref

        -- this will always be a boolean, so integer
        case ram of
            FloatingMode -> error "boolean cannot be floating"
            IntegerMode -> do
                asm $ test r r
                pure OnEqual -- jump if false, so zero

    BinCond v1 op v2 -> do
        let simpleCompare j ssep ram o1 o2 = case ram of
                IntegerMode -> asm (cmp o1 o2) *> pure j
                FloatingMode -> do
                    t <- Register . Direct
                        <$> freshVirtualRegister FloatingMode Extended64
                    asm $ do
                        movq t o1
                        cmpsse SseEqual ScalarDouble t o2
                        movq rax t
                        test rax rax
                    pure OnEqual

        -- don't compile v2 just yet so we can respect short-circuiting
        case op of
            -- both branches of a logical or must be booleans (i.e. integers)
            -- at this stage, so we can disregard handling floats
            LogicalOr -> do
                true <- asm newLabel

                -- compile the first operand
                (ram1, o1) <- compileVal v1
                when (ram1 == FloatingMode) $ error "boolean cannot be floating"

                asm $ do
                    -- if it's true, jump over the second operand
                    test o1 o1
                    jump OnNotEqual (Label true)

                -- compile the second operand
                (ram2, o2) <- compileVal v2
                when (ram2 == FloatingMode) $ error "boolean cannot be floating"

                asm $ do
                    -- set the flags for whether it's true
                    test o2 o2

                asm $ setLabel true
                -- at this point, ZF = 1 if either one of the operands is true.
                -- Hence to jump into the else branch, we would have to jump on
                -- ZF = 0, i.e. OnEqual
                pure OnEqual

            LogicalAnd -> do
                false <- asm newLabel

                (ram1, o1) <- compileVal v1
                when (ram1 == FloatingMode) $ error "boolean cannot be floating"

                asm $ do
                    test o1 o1
                    jump OnEqual (Label false)

                (ram2, o2) <- compileVal v2
                when (ram2 == FloatingMode) $ error "boolean cannot be floating"

                asm $ do
                    test o2 o2

                asm $ setLabel false
                -- At this point, ZF = 1 if *both* operands are true.
                -- Hence, to jump into the else branch, we would have to jump
                -- on ZF = 0, i.e. OnEqual.
                pure OnEqual

            Equal -> do
                ((ram1, o1), (ram2, o2)) <- (,) <$> compileVal v1 <*> compileVal v2
                simpleCompare OnNotEqual SseEqual ram1 o1 o2
            NotEqual -> do
                ((ram1, o1), (ram2, o2)) <- (,) <$> compileVal v1 <*> compileVal v2
                simpleCompare OnEqual SseNotEqual ram1 o1 o2
            LessThan -> do
                ((ram1, o1), (ram2, o2)) <- (,) <$> compileVal v1 <*> compileVal v2
                simpleCompare (OnNotBelow Signed) SseLessThan ram1 o1 o2
            LessThanEqual -> do
                ((ram1, o1), (ram2, o2)) <- (,) <$> compileVal v1 <*> compileVal v2
                simpleCompare (OnAbove Signed) SseLessThanOrEqual ram1 o1 o2
            GreaterThan -> do
                ((ram1, o1), (ram2, o2)) <- (,) <$> compileVal v1 <*> compileVal v2
                invertFlag
                    <$> simpleCompare (OnAbove Signed) SseLessThanOrEqual ram1 o1 o2
            GreaterThanEqual -> do
                ((ram1, o1), (ram2, o2)) <- (,) <$> compileVal v1 <*> compileVal v2
                invertFlag
                    <$> simpleCompare (OnNotBelow Signed) SseLessThan ram1 o1 o2

    UnCond op v -> case op of
        LogicalNot -> do
            (ram, o) <- compileVal v
            asm $ test o o
            -- need jump to fail if v is false, i.e. the bitwise and works out
            -- to zero, so the jump must succeed if the bitwise and is nonzero
            pure OnNotEqual

-- | Computes the register class for a given Vigil type.
registerClass :: Type -> RegisterAccessMode
registerClass (Fix ty) = case ty of
    -- basic data types, stack-allocated
    IntType _ -> IntegerMode
    FloatType _ -> FloatingMode
    -- heap-allocated complex data
    StructType {} -> IntegerMode
    ArrayType _ _ -> IntegerMode
    StringType -> IntegerMode
    -- impossible situations
    FuncType {} -> IntegerMode
    SliceType _ -> IntegerMode
    VoidType -> IntegerMode
    BuiltinType _ -> IntegerMode

compileVal
    :: TyAnnVal
    -> Compiler label (RegisterAccessMode, VirtualOperand label)
compileVal val = case val of
    IdentVal ident -> lookupIdent ident (Indirect . Offset 0)
    Literal lit -> compileLiteral lit
    IdentValD ident -> lookupIdent ident Direct

-- | Compile a literal.
-- The strategy used is to move the literal (as an immediate) into a fresh
-- virtual register and to return the register. The determined mode of the
-- register is also returned so callers may determine whether to use integer or
-- floating point instructions.
compileLiteral
    :: TyAnnLiteral
    -> Compiler label (RegisterAccessMode, VirtualOperand label)
compileLiteral (Ann _ lit) = case lit of
    IntLit n -> do
        t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
        asm $ mov t $ Immediate (ImmI $ fromIntegral n)
        pure (IntegerMode, t)
    FloatLit n -> do
        t <- Register . Direct <$> freshVirtualRegister FloatingMode Extended64
        asm $ do
            pxor t t
            mov rax $ Immediate (ImmF n)
            movq t rax
        pure (FloatingMode, t)
    RuneLit n -> do
        t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
        asm $ mov t (Immediate (ImmI $ fromIntegral $ fromEnum n))
        pure (IntegerMode, t)

compileRef
    :: Ref BasicIdent (Ident ()) TyAnnVal ()
    -> Compiler label (RegisterAccessMode, VirtualOperand label)
compileRef r = case r of
    ArrayRef i vs -> do
        (ram, i') <- lookupIdent i (Indirect . Offset 0)
        asm $ mov rax i'
        forM_ vs $ \v -> do
            (ramV, o') <- compileVal v -- ramV will always be IntegerMode
            -- compilVal will never write to rax, so it's safe to use rax to
            -- stoew the array pointer
            asm $ do
                mov rdi rax
                mov rsi o'
                call (External . Direct $ mangleFuncName "index_array")

        t <- freshVirtualRegister IntegerMode Extended64

        -- TODO check ultimate elem type for float
        asm $ mov (Register $ Direct t) rax
        pure (IntegerMode, (Register $ Indirect $ Offset 0 t))

    SelectRef i sels -> do
        (ram, i') <- lookupIdent i (Indirect . Offset 0)
        asm $ mov rax i'
        foldl'
            (\acc n -> do
                (ramO, o) <- acc
                v <- freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov rdi o
                    mov rsi (Immediate $ ImmI $ fromIntegral n)
                    call (External . Direct $ "struct_field")
                    mov (Register . Direct $ v) rax
                pure $ (IntegerMode, Register $ Direct v)
            )
            (pure (IntegerMode, rax))
            sels

    SliceRef i slis -> do

        -- For the first slice of the chain, we need to know if we're slicing
        -- an array or a slice.
        let meth = case gidTy i of
                Fix (ArrayType _ _) -> "slice_array"
                Fix (SliceType _) -> "slice_slice"
                _ -> error "Unsliceable type"

        -- slices are pointers, so this will always be integermode
        (ram, i') <- lookupIdent i (Indirect . Offset 0)

        o <- compileSliceExpr i' (head slis) meth
        (IntegerMode,) <$> foldl'
            (\cur idxs -> do
                o' <- cur
                compileSliceExpr o' idxs "slice_slice"
            )
            (pure o)
            (tail slis)

    ValRef val -> compileVal val

-- | Compiles a slice expression from an operand which contains the thing to
-- slice, the indices of the slice (as vals), a string indicating the slice
-- function to call (which depends on the type of the operand).
--
-- The result is a virtual register containing the new slice.
compileSliceExpr
    :: Operand SizedVirtualRegister label
     -> (Maybe TyAnnVal, Maybe TyAnnVal, Maybe TyAnnVal)
     -> String
     -> Compiler label (Operand SizedVirtualRegister label1)
compileSliceExpr o idxs func = do
    let (m, l, h, b) = unMaybeSliceTriple idxs
    (ramL, l') <- compileVal l
    (ramH, h') <- compileVal h
    (ramB, b') <- compileVal b

    v <- freshVirtualRegister IntegerMode Extended64
    asm $ do
        mov rdi o
        mov rsi $ Immediate $ ImmI $ fromIntegral m
        mov rdx l'
        mov rcx h'
        mov r8 b'
        call (External . Direct $ func)
        mov (Register . Direct $ v) rax
    pure $ Register $ Direct v

-- | Transform a slice triple-maybe-index into something that we can use to call
-- _slice_* functions from the runtime. The quadruple returned has the mode and
-- either the given indices or default values.
unMaybeSliceTriple
    :: (Maybe TyAnnVal, Maybe TyAnnVal, Maybe TyAnnVal)
    -> (Int, TyAnnVal, TyAnnVal, TyAnnVal)
unMaybeSliceTriple v@(ml, mh, mb) =
    (mode v, valOrDef ml, valOrDef mh, valOrDef mb)
    where
        valOrDef :: Maybe TyAnnVal -> TyAnnVal
        valOrDef mv = case mv of
            Nothing -> Literal $ Ann (intType I8) $ IntLit 0
            Just v' -> v'
        mode ms = case ms of
            (Nothing, Nothing, Nothing) -> 0
            (Just _, Nothing, Nothing) -> 1
            (Nothing , Just _, Nothing) -> 2
            (Just _, Just _, Nothing) -> 3
            (Nothing, Just _, Just _) -> 4
            (Just _, Just _, Just _) -> 5
            _ -> error "Impossible combination of slice indices"



prepareCall :: [TyAnnVal] -> Compiler label ()
prepareCall vals = mapM_ (uncurry assign) (reverse $ zip rams' vals) where
    assign m v = do
        case m of
            Nothing -> compileVal v >>= asm . push . snd
            Just r -> compileVal v >>= asm . mov (ihw r) . snd

    ihw = Register . Direct . SizedRegister Extended64 . FixedHardwareRegister

    -- the preferred access mode for each parameter
    rams :: [RegisterAccessMode]
    rams = registerClass . valType <$> vals

    -- the actual hardware register for each parameter, where Nothing means "on
    -- the stack"
    rams' :: [Maybe HardwareRegister]
    rams' = go regInitial rams where
        go _ [] = []
        go ([], f) (IntegerMode:xs) = Nothing : go ([], f) xs
        go (i, []) (FloatingMode:xs) = Nothing : go (i, []) xs
        go (i:is, fs) (IntegerMode:xs) = Just i : go (is, fs) xs
        go (is, f:fs) (FloatingMode:xs) = Just f : go (is, fs) xs

    regInitial =
        ( IntegerHwRegister <$> [Rdi, Rsi, Rdx, Rcx, R8, R9]
        , hwxmm <$> [0..7]
        )

-- | Wraps some code with the function prologue and epilogue.
wrapFunction :: Compiler label () -> Compiler label ()
wrapFunction v = do
    asm $ do
        push rbp
        mov rbp rsp

    asm $ prologue Save
    v
    asm $ prologue Load

    asm $ do
        mov rsp rbp
        pop rbp
        ret

deepSerializeType :: Type -> [Int]
deepSerializeType = cata f where
    f ty = case ty of
        IntType s -> [serializeType $ Fix $ IntType s]
        FloatType s -> [serializeType $ Fix $ FloatType s]
        StringType -> [7, 1, serializeType $ Fix $ IntType I1]
        ArrayType n tyn -> 7:n:tyn
        SliceType tyn -> 8:tyn
        StructType fields sz -> 9:(length fields):sz:(concat $ map snd fields)
        _ -> error "Type cannot be deep-serialized"

-- | Computes an integer representation of a type
serializeType :: Type -> Int
serializeType t = case unFix t of
    IntType s -> case s of
        I1 -> 1
        I2 -> 2
        I4 -> 3
        I8 -> 4
    FloatType _ -> 5
    StringType -> 6
    ArrayType _ _ -> 7
    SliceType _ -> 8
    StructType _ _ -> 9
    _ -> 0
