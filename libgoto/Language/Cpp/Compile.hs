{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Cpp.Compile where

import Language.Common.Misc
import Language.Common.Pretty
import Language.Common.Types
import qualified Language.Common.GlobalId as Gid
import Language.Common.Annotation
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Syntax.Types as T
import Language.GoLite.Syntax.Typecheck
import Language.GoLite.Types

import Control.Monad.State
import Data.Maybe ( fromMaybe )
import qualified Data.Set as S

data CompilerState
    = CompilerState
        { _decls :: S.Set GlobalId
        }

newtype Compiler a
    = Compiler
        { unCompiler :: State CompilerState a
        }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState CompilerState
        )

compileCpp :: TySrcAnnPackage -> Compiler Doc
compileCpp (Package (Ann _ (Ident packageName)) topLevelDecls) = do
    decls <- mapM compileTopLevelDecls topLevelDecls

    pure $ text "//" <+> text packageName
        $+$ text "#include <iostream>"
        $+$ text "#include <memory>"
        $+$ vcat decls

compileTopLevelDecls :: TySrcAnnTopLevelDecl -> Compiler Doc
compileTopLevelDecls topDecl = case topDecl of
    TopLevelDecl decl -> compileDecl decl
    TopLevelFun funDecl -> compileFunDecl funDecl

compileDecl :: TySrcAnnDeclaration -> Compiler Doc
compileDecl decl = (<> semi) <$> case decl of
    TypeDecl tyDecl -> compileTypeDecl tyDecl
    VarDecl varDecl -> compileVarDecl varDecl

compileVarDecl :: TySrcAnnVarDecl -> Compiler Doc

compileVarDecl (VarDeclBody idents _ []) = fmap vcat $ forM idents $ \gid -> do
    tyName <- compileTypeName (Gid.gidTy gid)
    varName <- makeVarName gid
    pure $ tyName <+> varName <> semi

compileVarDecl (VarDeclBody idents _ exprs)
    = vcat <$> mapM (uncurry emitVarDecl) (zip idents exprs) where

emitVarDecl :: GlobalId -> TySrcAnnExpr -> Compiler Doc
emitVarDecl gid expr = do
    tyName <- compileTypeName (Gid.gidTy gid)
    varName <- makeVarName gid
    expr' <- compileExpr expr
    pure $ tyName <+> varName <+> text "=" <+> expr' <> semi

makeVarName :: GlobalId -> Compiler Doc
makeVarName gid = pure $ text "govar_" <> int (gidNum gid)

compileTypeDecl :: SrcAnnTypeDecl -> Compiler Doc
compileTypeDecl (TypeDeclBody (Ann _ (Ident name)) ty) = do
    ty' <- compileType ty
    pure $ text "typedef" <+> ty' <+> text name <> semi

compileFunDecl :: TySrcAnnFunDecl -> Compiler Doc
compileFunDecl (FunDecl gid args retTy body) = do
    retTy' <- mapM (compileTypeName . fst . topAnn) retTy
    args' <- fmap (hsep . punctuate comma) $ forM args $ \(argGid, ty) -> do
        ty' <- compileTypeName . fst . topAnn $ ty
        argName <- makeVarName argGid
        pure $ ty' <+> argName
    body' <- mapM (fmap (<> semi) . compileStmt) body

    funName <- makeVarName gid

    pure $
        fromMaybe (text "void") retTy' <+>
        funName <> prettyParens True args' <+> text "{"
        $+$ nest indentLevel (
            vcat body'
        )

hasDecl :: GlobalId -> Compiler Bool
hasDecl gid = do
    s <- gets _decls
    pure $ gid `S.member` s

declare :: GlobalId -> Compiler ()
declare gid = modify $ \s -> s { _decls = S.insert gid $ _decls s }

compileStmt :: TySrcAnnStatement -> Compiler Doc
compileStmt = annCata phi where
    phi :: SrcSpan -> Algebra TySrcAnnStatementF (Compiler Doc)
    phi _ stmt = case stmt of
        DeclStmt decl -> compileDecl decl

        ExprStmt expr -> compileExpr expr

        ShortVarDecl idents exprs ->
            fmap vcat $ forM (zip idents exprs) $ \(gid, expr) -> do
                v <- makeVarName gid
                b <- hasDecl gid
                expr' <- compileExpr expr
                if b
                then pure $ v <+> text "=" <+> expr'
                else do
                    declare gid
                    emitVarDecl gid expr

        Assignment exprsl (Ann _ Assign) exprsr -> do
            fmap vcat $ forM (zip exprsl exprsr) $ \(exprl, exprr) -> do
                l <- compileExpr exprl
                r <- compileExpr exprr
                pure $ l <+> text "=" <+> r

        Assignment [exprl] (Ann _ assOp) [exprr] -> do
            l <- compileExpr exprl
            r <- compileExpr exprr

            if assOp == BitwiseAndNotEq
            then pure $ l <+> text "= ~" <> r
            else pure $ l <+> pretty assOp <+> r

        Assignment _ _ _ -> error "impossible assignment"

        PrintStmt vs -> do
            vs' <- mapM compileExpr vs
            pure $ text "cout <<" <+> hsep (punctuate (text "<<") vs')

        IfStmt minit

compileExpr :: TySrcAnnExpr -> Compiler Doc
compileExpr = annCata phi where
    phi :: (Type, SrcSpan) -> Algebra TySrcAnnExprF (Compiler Doc)
    phi (ty, _) expr = case expr of
        BinaryOp (Ann _ bin) e1 e2 -> do
            e1' <- e1
            e2' <- e2
            bin' <- compileBinaryOp bin
            pure $ prettyParens True (e1' <+> bin' <+> e2')

        UnaryOp (Ann _ un) e -> do
            e' <- e
            un' <- compileUnaryOp un
            pure $ prettyParens True (un' <> prettyParens True e')

        Conversion cty e -> do
            e' <- e
            cty' <- compileTypeName . fst . topAnn $ cty
            pure $ prettyParens True cty' <> prettyParens True e'

        Selector e (Ann _ (Ident sel)) -> do
            e' <- e
            pure $ e' <> text "." <> text sel

        Index e1 e2 -> do
            e1' <- e1
            e2' <- e2
            pure $ prettyParens True e1' <> prettyBrackets True e2'

        T.Slice e elo ehi ecap -> do
            e' <- e
            elo' <- sequence elo
            ehi' <- sequence ehi
            ecap' <- sequence ecap

            pure $
                text "take_slice" <> prettyParens True (
                    e' <> comma
                    <+> hsep (
                        punctuate
                            comma
                            (map (fromMaybe (text "-1")) [elo', ehi', ecap'])
                    )
                )

        Call e Nothing args -> do
            e' <- e
            args' <- sequence args
            pure $ e' <> prettyParens True (hsep (punctuate comma args'))

        Literal lit -> compileLiteral lit

        Variable v -> compileVariable v

        TypeAssertion _ _ -> error "type assertion not supported"

compileBinaryOp :: BinaryOp a -> Compiler Doc
compileBinaryOp = pure . pretty

compileUnaryOp :: UnaryOp a -> Compiler Doc
compileUnaryOp = pure . pretty

compileTyType :: TySrcAnnType -> Compiler Doc
compileTyType = undefined

compileType :: SrcAnnType -> Compiler Doc
compileType = undefined

compileTypeName :: Type -> Compiler Doc
compileTypeName = undefined

compileLiteral :: TySrcAnnLiteral -> Compiler Doc
compileLiteral (Ann _ lit) = pure $ case lit of
    IntLit i -> int i
    FloatLit f -> double f
    RuneLit r -> text (show r)
    StringLit s -> text (show s)

compileVariable :: GlobalId -> Compiler Doc
compileVariable = makeVarName
