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

            Assign (Ann _ ref) expr -> do
                r <- compileRef ref
                o <- compileExpr expr
                -- this is absolutely wrong
                asm $ mov r o

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
                    mul Signed t r2 Nothing
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
        vreg <- Register . Direct <$> freshVirtualRegister IntegerMode Low8
        asm $ setc j vreg
        pure vreg

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
        pure OnEqual

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
            pure OnEqual

-- | Computes the register class for a given Vigil type.
registerClass :: Type -> RegisterAccessMode
registerClass (Fix ty) = case ty of
    -- basic data types, stack-allocated
    IntType _ -> IntegerMode
    FloatType _ -> FloatingMode
    -- heap-allocated complex data
    StructType {} -> IntegerMode
    ArrayType _ -> IntegerMode
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
    IntLit n -> pure $ Immediate (ImmI $ fromIntegral n)
    FloatLit n -> pure $ Immediate (ImmF n)
    RuneLit n -> pure $ Immediate (ImmI $ fromIntegral $ fromEnum n)

compileRef
    :: Ref BasicIdent (Ident ()) TyAnnVal ()
    -> Compiler label (VirtualOperand label)
compileRef r = case r of
    ArrayRef i vs -> do
        i' <- lookupIdent i (Indirect . Offset 0)
        foldl'
            (\m v -> do
                o <- m
                o' <- compileVal v
                v1 <- freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov rdi o
                    mov rsi o'
                    call (External . Direct $ "_array_index")
                    mov (Register . Direct $ v1) rax
                pure $ Register $ Indirect $ Offset 0 v1
            )
            (pure i')
            vs

    ValRef val -> compileVal val

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
    ArrayType _ -> 7
    SliceType _ -> 8
    StructType _ -> 9
    _ -> 0

-- | Computes how many bytes of padding are needed to reach an alignment goal.
alignmentPadding
    :: Int -- ^ Current size
    -> Int -- ^ Alignment goal
    -> Int -- ^ Number of padding bytes required
alignmentPadding sz g = g - (sz `div` g)
{-# INLINE alignmentPadding #-}
