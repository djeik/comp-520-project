{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Vigil.Compile where

import Language.Common.Annotation
import Language.Vigil.Syntax
import Language.Vigil.Syntax.TyAnn
import Language.Vigil.Syntax.Basic
import Language.X86.Core
import Language.X86.Virtual

import Control.Monad.Reader

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

-- | Compiles a function.
runCompiler :: TyAnnFunDecl -> VirtualAsm addr label ()
runCompiler decl
    = runVirtualRegisterAllocatorT
    $ runReaderT (unCompiler (compileFunction decl))
    $ CompilerEnv
        { compilerFunc = decl
        }

data CompilerEnv
    = CompilerEnv
        { compilerFunc :: FunDecl BasicIdent BasicVarDecl TyAnnStatement
        }
    -- deriving (Eq, Ord, Read, Show)

-- | Emit raw assembly.
asm :: VirtualAsm addr label a -> Compiler addr label a
asm = Compiler . lift . lift

compileFunction
    :: TyAnnFunDecl
    -> Compiler addr label ()
compileFunction decl = wrapFunction $ mapM_ compileStmt $ _funDeclBody decl where
    compileStmt :: TyAnnStatement -> Compiler addr label ()
    compileStmt = undefined

-- | Generates the code to evaluate an expression. The result of the expression
-- is returned a 'VirtualOperand'.
compileExpr
    :: TyAnnExpr
    -> Compiler addr label (VirtualOperand addr label)
compileExpr (Ann a e) = undefined {- case e of
    Binary (Ann av1 v1) op (Ann av2 v2) -> case op of
        Plus -> do
            r1 <- compileRef v1
            r2 <- compileRef v2
            undefined asm $ add r1 r2


    Unary op v -> case op of
        Positive -> undefined

    Ref (Ann b r) -> compileRef r -}

compileVal
    :: TyAnnVal
    -> Compiler addr label ()
compileVal = undefined

compileRef = undefined

{-
compileRef
    :: Ref BasicIdent (Ident ()) TyAnnVal
    -> Compiler addr label (VirtualOperand addr label)
compileRef r = case r of
    ArrayRef i vs -> do
        i' <- getIdent i
        -}

callInternal
    :: String
callInternal = undefined


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

-- | Arranges the registers and the stack to perform a call.
{- prepareCall
    :: [VirtualOperand addr label]
    -> -}
