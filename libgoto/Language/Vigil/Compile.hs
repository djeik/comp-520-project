{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Vigil.Compile where

import Language.Common.Annotation
import Language.Vigil.Syntax
import Language.Vigil.Syntax.TyAnn
import Language.Vigil.Syntax.Basic
import Language.Vigil.Types
import Language.X86.Core
import Language.X86.Virtual

import Control.Monad.Reader
import Data.Functor.Foldable ( cata )

newtype Compiler addr label a
    = Compiler
        { unCompiler
            :: ReaderT CompilerEnv (
                VirtualRegisterAllocatorT (VirtualAsm addr label)
            ) a
        }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader CompilerEnv
        )

-- | The read-only environment of the function compiler.
data CompilerEnv
    = CompilerEnv
        { compilerFunc :: FunDecl BasicIdent BasicVarDecl TyAnnStatement
        -- ^ The function currently being compiled.
        }
    deriving (Eq, Ord, Read, Show)

-- | Emit raw assembly.
asm :: VirtualAsm addr label a -> Compiler addr label a
asm = Compiler . lift . lift

-- | Compiles a function.
runCompiler :: TyAnnFunDecl -> VirtualAsm addr label ()
runCompiler decl
    = runVirtualRegisterAllocatorT
    $ runReaderT (unCompiler (compileFunction decl))
    $ CompilerEnv
        { compilerFunc = decl
        }

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
                    -- to jump into the default case
                    defaultCaseL <- asm newLabel

                    case mg of
                        Just expr -> do
                            g <- compileExpr expr

                            code <- foldr
                                (\(hd, bd) code -> do
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
                                            jump JE (Label preCaseBody)

                                    -- if none of the alternatives in
                                    -- the case head match the guard,
                                    -- then jump past the case body
                                    asm $ do
                                        jump JMP (Label postCaseBody)
                                        setLabelHere preCaseBody

                                    mapM_ ($ (Just switchEnd, mBeginning)) bd

                                    asm $ do
                                        jump JMP (Label switchEnd)
                                        setLabelHere postCaseBody

                                    code
                                )
                                (pure ())
                                cs

                            asm $ setLabelHere defaultCaseL
                            if null c
                                -- no default case: jump past the switch
                                then asm $ jump JMP (Label switchEnd)
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

                j <- compileCondExpr cond

                asm $ jump j (Label forEnd)

                mapM_ ($ (Just forEnd, Just forStart)) body

                asm $ do
                    jump JMP (Label forStart)
                    setLabelHere forEnd

            BreakStmt ->
                maybe (error "invariant violation") (asm . jump JMP . Label) mEnd

            ContinueStmt -> do
                maybe (error "invariant violation") (asm . jump JMP . Label) mBeginning

-- | Generates the code to evaluate an expression. The computed
-- 'VirtualOperand' contains the result of the expression.
compileExpr
    :: TyAnnExpr
    -> Compiler addr label (VirtualOperand addr label)
compileExpr (Ann a e) = case e of
    Binary v1 op v2 -> case op of
        Plus -> do
            r1 <- compileVal v1
            r2 <- compileVal v2
            asm $ add r1 r2
            pure r1

    Unary op v -> case op of
        Positive -> undefined

    Ref (Ann b r) -> compileRef r

-- | Compiles a conditional expression. These translate to comparisons in x86,
-- which set flags, so the only thing that a called would need to know is which
-- jump variant to invoke in order to perform the correct branch.
compileCondExpr
    :: TyAnnCondExpr
    -> Compiler addr label JumpVariant
compileCondExpr = undefined

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
    ArrayRef i vs -> do
        i' <- compileIdent i
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
