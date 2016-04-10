{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Vigil.Compile where

import Language.Common.Annotation
import Language.GoLite.Misc ( unFix )
import Language.GoLite.Types ( stringFromSymbol )
import Language.Vigil.Syntax as T
import Language.Vigil.Syntax.TyAnn
import Language.Vigil.Syntax.Basic
import Language.Vigil.Types
import Language.X86.Core
import Language.X86.Virtual

import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Foldable ( cata )
import qualified Data.Map as M
import Data.List ( foldl' )

newtype Compiler addr label a
    = Compiler
        { unCompiler
            :: ReaderT (CompilerEnv addr label) (
                VirtualRegisterAllocatorT (VirtualAsm addr label)
            ) a
        }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader (CompilerEnv addr label)
        )

instance MonadVirtualRegisterAllocator (Compiler addr label) where
    freshVirtualRegister ram rs
        = Compiler $ lift $ freshVirtualRegister ram rs

-- | The read-only environment of the function compiler.
data CompilerEnv addr label
    = CompilerEnv
        { _identMap :: M.Map GlobalId (VirtualOperand addr label)
        -- ^ Precomputed map that assigns each GlobalId used by the function to
        -- a virtual operand that represents that data.
        }
    deriving (Eq, Ord, Read, Show)

-- | Looks up how to access the given data.
lookupIdent :: GlobalId -> Compiler addr label (VirtualOperand addr label)
lookupIdent gid = do
    identMap <- asks _identMap
    pure $ case M.lookup gid identMap of
        -- if it's not in the ident map for the function, then we need to
        -- generate a reference outside the function.
        Nothing -> case unFix $ gidTy gid of
            -- if it's a builtin, then we have to create an external reference,
            -- which will eventually be linked to the runtime.
            BuiltinType _ -> External $ stringFromSymbol $ gidOrigName gid
            -- anything else is something on the top level, so we generate an
            -- internal reference
            _ -> Internal $ stringFromSymbol $ gidOrigName gid
        Just o -> o

-- | Emit raw assembly.
asm :: VirtualAsm addr label a -> Compiler addr label a
asm = Compiler . lift . lift

-- | Compiles a function.
runCompiler :: TyAnnFunDecl -> VirtualAsm addr label ()
runCompiler decl
    = runVirtualRegisterAllocatorT
    $ runReaderT (unCompiler (compileFunction decl))
    $ makeCompilerEnv decl

type CompilerEnvAllocState addr label =
    ( Displacement
    , Displacement
    , [IntegerRegister]
    , [Int]
    , M.Map GlobalId (VirtualOperand addr label)
    )

makeCompilerEnv :: forall addr label. TyAnnFunDecl -> CompilerEnv addr label
makeCompilerEnv (FunDecl { _funDeclArgs = args, _funDeclVars = vars })
    = CompilerEnv { _identMap = identMap } where

        (_, _, _, _, identMap) = execState go initial

        go :: State (CompilerEnvAllocState addr label) ()
        go = do
            forM_ vars $ \(VarDecl gid) -> do
                offset <- nextlocal (8 :: Displacement)
                record gid (IndirectRegister (Offset offset $ rXx Rbp))

            forM_ args $ \(VarDecl gid) -> do
                case unFix $ gidTy gid of
                    FloatType _ -> paramRegAssign nextfreg xmm' gid
                    _ -> paramRegAssign nextireg rXx gid

        paramRegAssign
            :: State (CompilerEnvAllocState addr label) (Maybe a)
            -> (a -> SizedVirtualRegister)
            -> GlobalId
            -> State (CompilerEnvAllocState addr label) ()
        paramRegAssign regAllocator boxer gid = do
            m <- regAllocator
            case m of
                Just reg -> record gid (DirectRegister $ boxer reg)
                Nothing -> do
                    offset <- nextparam (8 :: Displacement)
                    record gid (IndirectRegister (Offset offset $ rXx Rbp))

        fixhw64 = SizedRegister Extended64 . FixedHardwareRegister

        xmm' = fixhw64 . hwxmm

        rXx = fixhw64 . IntegerHwRegister

        -- the initial state for the parameter and local allocator
        initial ::
            ( Displacement
            , Displacement
            , [IntegerRegister]
            , [Int]
            , M.Map GlobalId (VirtualOperand addr label)
            )
        initial =
            ( (negate 8) -- stack offset for locals
            , 16 -- stack offset for params
            , [Rdi, Rsi, Rdx, Rcx, R8, R9] -- available integer registers
            , [0..7] -- available floating registers
            , M.empty
            )

        -- safe head from a component of the state tuple
        nextreg
            :: (CompilerEnvAllocState addr label -> [a])
            -> ( [a]
                -> CompilerEnvAllocState addr label
                -> CompilerEnvAllocState addr label
            )
            -> State (CompilerEnvAllocState addr label) (Maybe a)
        nextreg getter setter = do
            regs <- gets getter
            case regs of
                [] -> pure Nothing
                (x:xs) -> do
                    modify (setter xs)
                    pure (Just x)

        record k v = modify $ \(a, b, c, d, m) -> (a, b, c, d, M.insert k v m)

        -- get the next integer register, or 'Nothing' if there are none
        -- available
        nextireg = nextreg
            (\(_, _, xs, _, _) -> xs)
            (\xs (a, b, _, c, d) -> (a, b, xs, c, d))

        -- get the next floating register, or 'Nothing' if there are none
        -- available
        nextfreg :: State (CompilerEnvAllocState addr label) (Maybe Int)
        nextfreg = nextreg
            (\(_, _, _, xs, _) -> xs)
            (\xs (a, b, c, _, d) -> (a, b, c, xs, d))

        nextmemory getter setter increment = do
            n <- gets getter
            modify (setter $ n + increment)
            pure n

        nextlocal = nextmemory
            (\(n, _, _, _, _) -> n)
            (\n (_, a, b, c, d) -> (n, a, b, c, d))

        nextparam = nextmemory
            (\(_, n, _, _, _) -> n)
            (\n (a, _, b, c, d) -> (a, n, b, c, d))

-- | Compile a function
compileFunction
    :: TyAnnFunDecl
    -> Compiler addr label ()
compileFunction decl = wrapFunction $ compileBody none $ _funDeclBody decl where
    none :: (Maybe label, Maybe label)
    none = (Nothing, Nothing)

    compileBody
        :: (Maybe label, Maybe label)
        -> [TyAnnStatement]
        -> Compiler addr label ()
    compileBody t = mapM_ (compileStmt t)

    compileStmt
        :: (Maybe label, Maybe label)
        -> TyAnnStatement
        -> Compiler addr label ()
    compileStmt t = ($ t) . cata f where
        f :: TyAnnStatementF ((Maybe label, Maybe label) -> Compiler addr label ())
            -> (Maybe label, Maybe label) -> Compiler addr label ()
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
                    call (External "goprint")

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
                asm $ setLabelHere l
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
                                    setLabelHere preCaseBody

                                mapM_ ($ (Just switchEnd, mBeginning)) bd

                                asm $ do
                                    jump Unconditionally (Label switchEnd)
                                    setLabelHere postCaseBody

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
                                    setLabelHere preCaseBody

                                mapM_ ($ (Just switchEnd, mBeginning)) bd

                                asm $ do
                                    jump Unconditionally (Label switchEnd)
                                    setLabelHere postCaseBody

                            -- default case
                            if null c
                                then asm $ jump Unconditionally (Label switchEnd)
                                else do
                                    mapM_ ($ (Just switchEnd, mBeginning)) c


                    asm $ setLabelHere switchEnd

            ForStmt Nothing body -> do
                (forEnd, forStart) <- (,) <$> asm newLabel <*> asm newLabel
                mapM_ ($ (Just forEnd, Just forStart)) body
                asm $ setLabelHere forEnd

            ForStmt (Just (code, cond)) body -> do
                (forEnd, forStart) <- (,) <$> asm newLabel <*> asm newLabel

                asm $ setLabelHere forStart

                mapM_ ($ (Just forEnd, Just forStart)) code
                j <- compileCondExpr cond

                asm $ jump j (Label forEnd)

                mapM_ ($ (Just forEnd, Just forStart)) body

                asm $ do
                    jump Unconditionally (Label forStart)
                    setLabelHere forEnd

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
integerAbs :: VirtualOperand addr label -> Compiler addr label ()
integerAbs o = do
    t <- DirectRegister <$> freshVirtualRegister IntegerMode Extended64
    asm $ do
        mov t o
        sar t (Immediate $ ImmI 31)
        xor o t
        sub o t

-- | Generates the code to evaluate an expression. The computed
-- 'VirtualOperand' contains the result of the expression.
compileExpr
    :: TyAnnExpr
    -> Compiler addr label (VirtualOperand addr label)
compileExpr (Ann ty e) = case e of
    Conversion dstTy (Ann srcTy ref) -> do
        o <- compileRef ref
        case (unFix dstTy, unFix srcTy) of
            (IntType _, IntType _) -> pure o
            (FloatType _, FloatType _) -> pure o
            (FloatType _, IntType _) -> do
                t <- DirectRegister <$> freshVirtualRegister FloatingMode Extended64
                asm $ cvt ScalarDouble SingleInteger t o
                pure t
            (IntType _, FloatType _) -> do
                t <- DirectRegister <$> freshVirtualRegister IntegerMode Extended64
                asm $ cvt SingleInteger ScalarDouble t o
                pure t
            _ -> error "wtf?"


    Binary v1 op v2 -> do
        (r1, r2) <- (,) <$> compileVal v1 <*> compileVal v2

        case op of
            -- TODO need to implement for floats
            Plus -> do
                t <- DirectRegister <$> freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov t r1
                    add t r2
                pure t

            Minus -> do
                t <- DirectRegister <$> freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov t r1
                    sub t r2
                pure t

            Times -> do
                t <- DirectRegister <$> freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov t r1
                    mul Signed t r2 Nothing
                pure t

            Divide -> do
                t <- DirectRegister <$> freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov rax r1 -- low 64 bits of dividend
                    cqo rdx rax -- sign extend rax into rdx
                    idiv rdx rax r2 -- perform the division
                    mov t rax
                pure t

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
        vreg <- DirectRegister <$> freshVirtualRegister IntegerMode Low8
        asm $ setc j vreg
        pure vreg

    T.Call i vs -> do
        f <- compileIdent i

        asm $ scratch Save
        prepareCall vs
        asm $ call f
        asm $ scratch Load

        case unFix ty of
            IntType _ -> do
                t <- DirectRegister <$> freshVirtualRegister IntegerMode Extended64
                asm $ mov t rax
                pure t
            FloatType _ -> do
                t <- DirectRegister <$> freshVirtualRegister FloatingMode Extended64
                asm $ mov t (xmm 0)
                pure t

    InternalCall name vs -> do
        asm $ scratch Save
        prepareCall vs
        asm $ call (External name)
        asm $ scratch Load

        case unFix ty of
            IntType _ -> do
                t <- DirectRegister <$> freshVirtualRegister IntegerMode Extended64
                asm $ mov t rax
                pure t
            FloatType _ -> do
                t <- DirectRegister <$> freshVirtualRegister FloatingMode Extended64
                asm $ mov t (xmm 0)
                pure t



-- | Compiles a conditional expression. These translate to comparisons in x86,
-- which set flags, so the only thing that a called would need to know is which
-- jump variant to invoke in order to perform the correct branch.
compileCondExpr
    :: TyAnnCondExpr
    -> Compiler addr label FlagCondition
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

                asm $ setLabelHere true
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

                asm $ setLabelHere false
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
    -> Compiler addr label (VirtualOperand addr label)
compileVal val = case val of
    IdentVal ident -> compileIdent ident
    Literal lit -> compileLiteral lit

compileLiteral
    :: TyAnnLiteral
    -> Compiler addr label (VirtualOperand addr label)
compileLiteral (Ann _ lit) = case lit of
    IntLit n -> pure $ Immediate (ImmI $ fromIntegral n)
    FloatLit n -> pure $ Immediate (ImmF n)
    RuneLit n -> pure $ Immediate (ImmI $ fromIntegral $ fromEnum n)

compileRef
    :: Ref BasicIdent (Ident ()) TyAnnVal ()
    -> Compiler addr label (VirtualOperand addr label)
compileRef r = case r of
    ArrayRef i vs -> do
        i' <- compileIdent i
        foldl'
            (\m v -> do
                o <- m
                o' <- compileVal v
                v1 <- freshVirtualRegister IntegerMode Extended64
                asm $ do
                    mov rdi o
                    mov rsi o'
                    call (External "array_index")
                    mov (DirectRegister v1) rax
                pure $ IndirectRegister $ Offset 0 v1
            )
            (pure i')
            vs





prepareCall :: [TyAnnVal] -> Compiler addr label ()
prepareCall vals = mapM_ (uncurry assign) (reverse $ zip rams' vals) where
    assign m v = do
        case m of
            Nothing -> compileVal v >>= asm . push
            Just r -> compileVal v >>= asm . mov (ihw r)

    ihw = DirectRegister . SizedRegister Extended64 . FixedHardwareRegister

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

compileIdent
    :: GlobalId
    -> Compiler addr label (VirtualOperand addr label)
compileIdent = lookupIdent

-- | Wraps some code with the function prologue and epilogue.
wrapFunction :: Compiler addr label () -> Compiler addr label ()
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
    IntType _ -> 1
    FloatType _ -> 2
    StringType -> 3
    _ -> 0

-- | Computes how many bytes of padding are needed to reach an alignment goal.
alignmentPadding
    :: Int -- ^ Current size
    -> Int -- ^ Alignment goal
    -> Int -- ^ Number of padding bytes required
alignmentPadding sz g = g - (sz `div` g)
{-# INLINE alignmentPadding #-}
