{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Vigil.Compile where

import Language.Common.Annotation
import Language.GoLite.Misc ( unFix )
import Language.GoLite.Types ( stringFromSymbol )
import Language.Vigil.Syntax
import Language.Vigil.Syntax.TyAnn
import Language.Vigil.Syntax.Basic
import Language.Vigil.Types
import Language.X86.Core
import Language.X86.Virtual

import Control.Monad.Reader
import Control.Monad.State
import Data.Functor.Foldable ( cata )
import qualified Data.Map as M

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

makeCompilerEnv :: TyAnnFunDecl -> CompilerEnv addr label
makeCompilerEnv
    = flip evalStateT initial $ do
        undefined
    where
        initial = undefined

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
                asm $ mov r o

            PrintStmt _ -> undefined -- generate call to Print

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
        sar t (Immediate 31)
        xor o t
        sub o t

-- | Generates the code to evaluate an expression. The computed
-- 'VirtualOperand' contains the result of the expression.
compileExpr
    :: TyAnnExpr
    -> Compiler addr label (VirtualOperand addr label)
compileExpr (Ann _ e) = case e of
    Binary v1 op v2 -> case op of
        Plus -> do
            r1 <- compileVal v1
            r2 <- compileVal v2
            asm $ add r1 r2
            pure r1

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
compileVal = undefined

compileRef
    :: Ref BasicIdent (Ident ()) TyAnnVal ()
    -> Compiler addr label (VirtualOperand addr label)
compileRef r = case r of
    ArrayRef i _ -> do
        _ <- compileIdent i
        undefined

compileIdent
    :: GlobalId
    -> Compiler addr label (VirtualOperand addr label)
compileIdent = undefined

-- | Wraps some code with the function prologue and epilogue.
wrapFunction :: Compiler addr label () -> Compiler addr label ()
wrapFunction v = do
    asm $ do
        push rbp
        mov rbp rsp
    v
    asm $ do
        mov rsp rbp
        pop rbp
        ret

-- | Computes how many bytes of padding are needed to reach an alignment goal.
alignmentPadding
    :: Int -- ^ Current size
    -> Int -- ^ Alignment goal
    -> Int -- ^ Number of padding bytes required
alignmentPadding sz g = g - (sz `div` g)
{-# INLINE alignmentPadding #-}
