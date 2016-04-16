{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Vigil.Compile where

import Language.Common.Annotation
import Language.Common.Misc ( unFix )
import Language.GoLite.Types ( stringFromSymbol )
import Language.Vigil.Syntax as T
import Language.Vigil.Syntax.TyAnn
import Language.Vigil.Syntax.Basic
import Language.Vigil.Types
import Language.X86.Core
import Language.X86.Virtual
import Language.X86.Virtual.Registers

import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Foldable ( cata )
import qualified Data.Map as M
import Data.List ( foldl' )

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
        { _identMap :: M.Map GlobalId (VirtualOperand label)
        -- ^ Precomputed map that assigns each GlobalId used by the function to
        -- a virtual operand that represents that data.
        }
    deriving (Eq, Ord, Read, Show)

-- | Looks up how to access the given data.
lookupIdent
    :: GlobalId
    -> (String -> Directness String)
    -> Compiler label (VirtualOperand label)
lookupIdent gid dir = do
    imap <- asks _identMap
    pure $ case M.lookup gid imap of
        -- if it's not in the ident map for the function, then we need to
        -- generate a reference outside the function.
        Nothing -> case unFix $ gidTy gid of
            -- if it's a builtin, then we have to create an external reference,
            -- which will eventually be linked to the runtime.
            BuiltinType _ -> External . Direct . stringFromSymbol $ gidOrigName gid
            -- anything else is something on the top level, so we generate an
            -- internal reference
            FuncType {} -> Internal $ Direct $ stringFromSymbol $ gidOrigName gid
            _ ->
                Internal $ dir $ stringFromSymbol $ gidOrigName gid
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
        , identMap :: M.Map GlobalId (VirtualOperand label)
        }
    deriving (Eq, Ord, Read, Show)

makeCompilerEnv
    :: forall label. TyAnnFunDecl
    -> VirtualRegisterAllocatorT (VirtualAsm label) (CompilerEnv label)
makeCompilerEnv (FunDecl { _funDeclArgs = args, _funDeclVars = vars })
    = CompilerEnv <$> is where
        is :: VirtualRegisterAllocatorT
            (VirtualAsm label)
            (M.Map GlobalId (VirtualOperand label))
        is = identMap <$> execStateT go initial

        go :: StateT
            (CompilerEnvAllocState label)
            (VirtualRegisterAllocatorT (VirtualAsm label))
            ()
        go = do
            forM_ vars $ \(VarDecl gid) -> do
                v <- case unFix $ gidTy gid of
                    FloatType _ -> Register . Direct
                        <$> lift (freshVirtualRegister FloatingMode Extended64)
                    _ -> Register . Direct
                        <$> lift (freshVirtualRegister IntegerMode Extended64)

                record gid v

            forM_ args $ \(VarDecl gid) -> do
                case unFix $ gidTy gid of
                    FloatType _ -> paramRegAssign nextfreg xmm' gid
                    _ -> paramRegAssign nextireg rXx gid

        paramRegAssign
            :: StateT
                (CompilerEnvAllocState label)
                (VirtualRegisterAllocatorT (VirtualAsm label))
                (Maybe a)
            -> (a -> SizedVirtualRegister)
            -> GlobalId
            -> StateT
                (CompilerEnvAllocState label)
                (VirtualRegisterAllocatorT (VirtualAsm label))
                ()
        paramRegAssign regAllocator boxer gid = do
            m <- regAllocator
            case m of
                Just reg -> record gid (Register . Direct $ boxer reg)
                Nothing -> do
                    offset <- nextparam (8 :: Displacement)
                    record gid (Register $ Indirect (Offset offset $ rXx Rbp))

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
                r <- compileRef ref
                o <- compileExpr expr

                let copy s = do
                        mov rdi o
                        call (External $ Direct s)
                        mov r rax

                asm $ case unFix ty of
                    IntType _ -> mov r o
                    FloatType _ -> undefined -- TODO
                    StringType -> copy "_deepcopy_array"
                    ArrayType _ _ -> copy "_deepcopy_array"
                    SliceType _ -> copy "_shallowcopy_slice"
                    StructType _ -> copy "_deepcopy_struct"

            Initialize i -> do
                i' <- lookupIdent i Direct
                let diRef = mov rdi (Internal $ Direct (stringFromSymbol (gidOrigName i) ++ "_ini"))
                let cEx s = do
                        diRef
                        call (External $ Direct s)
                        mov i' rax in

                        asm $ case unFix $ gidTy i of
                                IntType _ -> mov i' $ Immediate $ ImmI 0
                                FloatType _ -> undefined -- TODO
                                StringType -> cEx "_new_array"
                                ArrayType _ _ -> cEx "_new_array"
                                SliceType _ -> cEx "_new_slice"
                                StructType _ -> cEx "_new_struct"

            PrintStmt vs -> forM_ vs $ \(Ann ty v) -> do
                o <- compileRef v
                let sty = serializeType ty

                asm $ withScratch $ do
                    mov rdi (Immediate $ ImmI $ fromIntegral sty)
                    mov rsi o
                    call (External . Direct $ "_goprint")

            ReturnStmt (Just (Ann _ ref)) -> do
                r <- compileRef ref
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
                            g <- compileExpr expr

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
                                    e' <- compileExpr e
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
                                    e' <- compileExpr e
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
integerAbs :: VirtualOperand label -> Compiler label ()
integerAbs o = do
    t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
    asm $ do
        mov t o
        sar t (Immediate $ ImmI 31)
        xor o t
        sub o t

-- | Generates the code to evaluate an expression. The computed
-- 'VirtualOperand' contains the result of the expression.
compileExpr
    :: TyAnnExpr
    -> Compiler label (VirtualOperand label)
compileExpr (Ann ty e) = case e of
    Conversion dstTy (Ann srcTy ref) -> do
        o <- compileRef ref
        case (unFix dstTy, unFix srcTy) of
            (IntType _, IntType _) -> pure o
            (FloatType _, FloatType _) -> pure o
            (FloatType _, IntType _) -> do
                t <- Register . Direct <$> freshVirtualRegister FloatingMode Extended64
                asm $ cvt ScalarDouble SingleInteger t o
                pure t
            (IntType _, FloatType _) -> do
                t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ cvt SingleInteger ScalarDouble t o
                pure t
            _ -> error "wtf?"


    Binary v1 op v2 -> do
        (r1, r2) <- (,) <$> compileVal v1 <*> compileVal v2

        case op of
            -- TODO need to implement for floats
            Plus -> do
                t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov t r1
                    add t r2
                pure t

            Minus -> do
                t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov t r1
                    sub t r2
                pure t

            Times -> do
                t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov t r1
                    imul t r2
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
            o <- compileVal v
            integerAbs o
            pure o

        Negative -> do
            o <- compileVal v
            integerAbs o
            asm $ neg2 o
            pure o

        BitwiseNot -> do
            o <- compileVal v
            asm $ neg1 o
            pure o

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

        pure t

    T.Call i vs -> do
        f <- lookupIdent i Direct

        asm $ scratch Save
        prepareCall vs
        asm $ call f
        asm $ scratch Load

        case unFix ty of
            VoidType -> pure rax

            FloatType _ -> do
                t <- Register . Direct <$> freshVirtualRegister FloatingMode Extended64
                asm $ mov t (xmm 0)
                pure t

            _ -> do
                t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ mov t rax
                pure t

    InternalCall name vs -> do
        asm $ scratch Save
        prepareCall vs
        asm $ call (External . Direct $ name)
        asm $ scratch Load

        case unFix ty of
            FloatType _ -> do
                t <- Register . Direct <$> freshVirtualRegister FloatingMode Extended64
                asm $ mov t (xmm 0)
                pure t

            _ -> do
                t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
                asm $ mov t rax
                pure t

-- | Compiles a conditional expression. These translate to comparisons in x86,
-- which set flags, so the only thing that a called would need to know is which
-- jump variant to invoke in order to perform the correct branch.
compileCondExpr
    :: TyAnnCondExpr
    -> Compiler label FlagCondition
compileCondExpr e = case e of
    CondRef (Ann _ ref) -> do
        r <- compileRef ref
        asm $ test r r
        pure OnEqual -- jump if false, so zero

    BinCond v1 op v2 -> do
        let simpleCompare j = do
                (o1, o2) <- (,) <$> compileVal v1 <*> compileVal v2
                asm $ cmp o1 o2
                pure j

        -- don't compile v2 just yet so we can respect short-circuiting
        case op of
            LogicalOr -> do
                true <- asm newLabel

                -- compile the first operand
                o1 <- compileVal v1

                asm $ do
                    -- if it's true, jump over the second operand
                    test o1 o1
                    jump OnNotEqual (Label true)

                -- compile the second operand
                o2 <- compileVal v2
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

                o1 <- compileVal v1

                asm $ do
                    test o1 o1
                    jump OnEqual (Label false)

                o2 <- compileVal v2
                asm $ do
                    test o2 o2

                asm $ setLabel false
                -- At this point, ZF = 1 if *both* operands are true.
                -- Hence, to jump into the else branch, we would have to jump
                -- on ZF = 0, i.e. OnEqual.
                pure OnEqual

            Equal -> simpleCompare OnNotEqual
            NotEqual -> simpleCompare OnEqual
            LessThan -> simpleCompare (OnNotBelow Signed)
            LessThanEqual -> simpleCompare (OnAbove Signed)
            GreaterThan -> simpleCompare (OnBelowOrEqual Signed)
            GreaterThanEqual -> simpleCompare (OnBelow Signed)

    UnCond op v -> case op of
        LogicalNot -> do
            o <- compileVal v
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
    -> Compiler label (VirtualOperand label)
compileVal val = case val of
    IdentVal ident -> lookupIdent ident (Indirect . Offset 0)
    Literal lit -> compileLiteral lit
    IdentValD ident -> lookupIdent ident Direct

compileLiteral
    :: TyAnnLiteral
    -> Compiler label (VirtualOperand label)
compileLiteral (Ann _ lit) = case lit of
    IntLit n -> do
        t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
        asm $ mov t $ Immediate (ImmI $ fromIntegral n)
        pure t
    FloatLit n -> do
        t <- Register . Direct <$> freshVirtualRegister FloatingMode Extended64
        asm $ mov t $ Immediate (ImmF n)
        pure t
    RuneLit n -> do
        t <- Register . Direct <$> freshVirtualRegister IntegerMode Extended64
        asm $ mov t (Immediate (ImmI $ fromIntegral $ fromEnum n))
        pure t

compileRef
    :: Ref BasicIdent (Ident ()) TyAnnVal ()
    -> Compiler label (VirtualOperand label)
compileRef r = case r of
    ArrayRef i vs -> do
        i' <- lookupIdent i (Indirect . Offset 0)
        asm $ mov rax i'
        foldl'
            (\m v -> do
                o <- m
                o' <- compileVal v
                v1 <- freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov rdi o
                    mov rsi o'
                    call (External . Direct $ "_index_array")
                    mov (Register . Direct $ v1) rax
                pure $ Register $ Direct v1
            )
            (pure rax)
            vs

    SelectRef i sels -> do
        i' <- lookupIdent i (Indirect . Offset 0)
        asm $ mov rax i'
        foldl'
            (\acc n -> do
                o <- acc
                v <- freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov rdi o
                    mov rsi (Immediate $ ImmI $ fromIntegral n)
                    call (External . Direct $ "_struct_field")
                    mov (Register . Direct $ v) rax
                pure $ Register $ Direct v
            )
            (pure rax)
            sels

    SliceRef i slis -> do

        -- For the first slice of the chain, we need to know if we're slicing
        -- an array or a slice.
        let meth = case gidTy i of
                Fix (ArrayType _ _) -> "_slice_array"
                Fix (SliceType _) -> "_slice_slice"
                _ -> error "Unsliceable type"

        i' <- lookupIdent i (Indirect . Offset 0)
        o <- compileSliceExpr i' (head slis) meth
        foldl'
            (\cur idxs -> do
                o' <- cur
                compileSliceExpr o' idxs "_slice_slice"
            )
            (pure o)
            (tail slis)

        -- asm $ mov rax i'
        undefined
        -- Check the type of the ident. Is it a slice array?
        -- Do something about potentially missing values...
        -- Call slice_xx in the first case,
        -- then fold over the tail of the list, calling slice_slice

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
    l' <- compileVal l
    h' <- compileVal h
    b' <- compileVal b

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
            Nothing -> compileVal v >>= asm . push
            Just r -> compileVal v >>= asm . mov (ihw r)

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
        StructType fields -> 9:(concat fields)
        _ -> undefined

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
    StructType _ -> 9
    _ -> 0
