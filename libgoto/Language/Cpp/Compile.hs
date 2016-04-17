{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Cpp.Compile where

import Language.Common.Misc
import Language.Common.Pretty
import Language.Common.Types
import qualified Language.Common.GlobalId as Gid
import Language.Common.Annotation
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Syntax.Types as Syn
import Language.GoLite.Syntax.Typecheck
import Language.GoLite.Types as T

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

data CompilerState
    = CompilerState
        { _decls :: S.Set GlobalId
        -- ^ keep track of declarations so that we know whether to emit a
        -- declaration or an assignment for short variable declarations.
        , _undecidedTypes :: [Type]
        -- ^ We keep the code of the anonymous type so that we can generate the
        -- typedefs after passing though the whole program
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

-- | We use a function type to delay deciding how to render types, due to
-- anonymous structs needing to be typedef'd.
type DocCompiler = Compiler (M.Map Type Doc -> Doc)

runCompiler :: TySrcAnnPackage -> Doc
runCompiler p = evalState (unCompiler $ compileCpp p) initial where
    -- (f, s) = runState (unCompiler $ compileCpp p) initial
    initial = CompilerState
        { _decls = S.empty
        , _undecidedTypes = []
        }

compileCpp :: TySrcAnnPackage -> Compiler Doc
compileCpp (Package (Ann _ (Ident packageName)) topLevelDecls) = do
    decls <- mapM compileTopLevelDecls topLevelDecls

    undecidedTypes <- gets _undecidedTypes
    (preamble, decided) <- decideTypes undecidedTypes

    pure $
        text "//" <+> text packageName $+$
        text "#include <iostream>" $+$
        text "#include <memory>" $+$
        text "#include <array>" $+$
        text "#include <vector>" $+$
        text "#include <string>" $+$
        text "#include \"goto.hpp\"" $+$
        text "using namespace std;" $+$
        preamble $+$
        text "bool gocode_true = true;" $+$
        text "bool gocode_false = false;" $+$
        vcat (map ($ decided) decls) $+$
        text "int main() { gocode_main(); }"

decideTypes :: [Type] -> Compiler (Doc, M.Map Type Doc)
decideTypes = foldr phi (pure (empty, M.empty)) . enumerate where
    phi :: (Int, Type) -> Compiler (Doc, M.Map Type Doc) -> Compiler (Doc, M.Map Type Doc)
    phi (i, ty) c = do
        let typeName = text "type" <> int i
        (d, m) <- c
        let preamble = text "typedef" <+> compileType m ty <+> typeName <> semi
        pure (d $+$ preamble, M.insert ty typeName m)

compileTopLevelDecls :: TySrcAnnTopLevelDecl -> DocCompiler
compileTopLevelDecls topDecl = case topDecl of
    TopLevelDecl decl -> fmap2 (<> semi) $ compileDecl decl
    TopLevelFun funDecl -> compileFunDecl funDecl

compileDecl :: TySrcAnnDeclaration -> DocCompiler
compileDecl decl = case decl of
    TypeDecl tyDecl -> compileTypeDecl tyDecl
    VarDecl varDecl -> compileVarDecl varDecl

compileVarDecl :: TySrcAnnVarDecl -> DocCompiler

compileVarDecl (VarDeclBody idents _ [])
    = fmap2 (hsep . punctuate semi) $ fmap sequence $ forM idents $ \gid -> do
        undecideType (Gid.gidTy gid)
        let varName = makeVarName gid
        pure $ \m -> getType m (Gid.gidTy gid) <+> varName

compileVarDecl (VarDeclBody idents _ exprs)
    = fmap (fmap (hsep . punctuate semi) . sequence) $ mapM (uncurry emitVarDecl) (zip idents exprs) where

emitVarDecl :: GlobalId -> TySrcAnnExpr -> DocCompiler
emitVarDecl gid expr = do
    undecideType (Gid.gidTy gid)
    let varName = makeVarName gid
    expr' <- compileExpr expr

    pure $ \m ->
        getType m (Gid.gidTy gid) <+>
        varName <+>
        text "=" <+>
        expr' m

makeVarName :: GlobalId -> Doc
makeVarName gid = text $ stringFromSymbol $ bare $ gidOrigName gid

compileTypeDecl :: TySrcAnnTypeDecl -> DocCompiler
compileTypeDecl (TypeDeclBody (Ann _ (Ident name)) ty) = do
    let type_ = fst (topAnn ty)
    undecideType type_
    pure $ \m -> text "typedef" <+> getType m type_ <+> text name

compileFunDecl :: TySrcAnnFunDecl -> DocCompiler
compileFunDecl (FunDecl gid args retTy body) = do
    let retTy' = fmap (fst . topAnn) retTy

    maybe (pure ()) undecideType retTy'

    args' <- fmap (fmap (hsep . punctuate comma) . sequence) $ forM args $ \(argGid, ty) -> do
        let ty' = fst (topAnn ty)
        undecideType ty'
        let argName = makeVarName argGid
        pure $ \m -> getType m ty' <+> argName

    body' <- mapM compileStmt body

    let funName = makeVarName gid

    pure $ \m ->
        maybe (text "void") (getType m) retTy' <+>
        funName <> prettyParens True (args' m) $+$
        wrapBlock (($ m) $ fmap (vcat . map (<> semi)) $ sequence body')

hasDecl :: GlobalId -> Compiler Bool
hasDecl gid = do
    s <- gets _decls
    pure $ gid `S.member` s

declare :: GlobalId -> Compiler ()
declare gid = modify $ \s -> s { _decls = S.insert gid $ _decls s }

undecideType :: Type -> Compiler ()
undecideType ty = do
    tys <- gets _undecidedTypes
    when (not (ty `elem` tys)) $ do
        modify $ \s -> s { _undecidedTypes = ty : _undecidedTypes s }

compileStmt :: TySrcAnnStatement -> DocCompiler
compileStmt = annCata phi where
    phi :: SrcSpan -> Algebra TySrcAnnStatementF (DocCompiler)
    phi _ stmt = case stmt of
        DeclStmt decl -> compileDecl decl

        ExprStmt expr -> compileExpr expr

        ShortVarDecl idents exprs ->
            fmap (fmap (hsep . punctuate semi) . sequence) $ forM (zip idents exprs) $ \(gid, expr) -> do
                let v = makeVarName gid
                b <- hasDecl gid
                expr' <- compileExpr expr
                if b
                then pure $ \m -> v <+> text "=" <+> expr' m
                else do
                    declare gid
                    emitVarDecl gid expr

        Assignment exprsl (Ann _ Assign) exprsr -> do
            fmap (fmap (hsep . punctuate semi) . sequence) $
                forM (zip exprsl exprsr) $ \(exprl, exprr) -> do
                    l <- compileExpr exprl
                    r <- compileExpr exprr
                    pure $ \m -> l m <+> text "=" <+> r m

        Assignment [exprl] (Ann _ assOp) [exprr] -> do
            l <- compileExpr exprl
            r <- compileExpr exprr

            if assOp == BitwiseAndNotEq
            then pure $ \m -> l m <+> text "= ~" <> r m
            else pure $ \m -> l m <+> pretty assOp <+> r m

        Assignment _ _ _ -> error "impossible assignment"

        PrintStmt vs -> do
            vs' <- mapM compileExpr vs
            pure $ \m ->
                text "cout << boolalpha <<" <+>
                hsep (punctuate (text " <<") $ sequence vs' m)

        IfStmt minit e tb eb -> fmap2 wrapBlock $ do
            minit' <- sequence minit
            tb' <- sequence tb
            eb' <- sequence2 eb
            e' <- compileExpr e

            pure $ \m ->
                maybe empty ($ m) minit' <> semi $+$
                text "if" <+> prettyParens True (e' m) $+$
                wrapBlock (vcat $ map (<> semi) $ sequence tb' m) $+$
                maybe
                    empty
                    ( (text "else" $+$)
                    . wrapBlock
                    . vcat
                    . map ((<> semi) . ($ m))
                    )
                    eb'

        SwitchStmt minit me cases -> fmap2 wrapBlock $ do
            minit' <- sequence minit
            me' <- traverse compileExpr me
            cases' <- mapM (\(hd, bd) -> compileCase hd (fmap vcat $ sequence bd)) =<< sequence3 cases

            pure $ \m ->
                maybe empty ($ m) minit' <> semi $+$
                text "switch" <+> prettyParens True (maybe (text "true") ($ m) me') $+$
                wrapBlock (vcat $ map ($ m) cases')

        ReturnStmt mval -> do
            mval' <- traverse compileExpr mval
            pure $ \m -> text "return" <+> maybe empty ($ m) mval'

        ForStmt mpre mcond mpost body -> do
            mpre' <- sequence mpre
            mcond' <- traverse compileExpr mcond
            mpost' <- sequence mpost
            body' <- sequence body

            pure $ \m ->
                maybe empty ($ m) mpre' <> semi $+$
                text "while" <> prettyParens True (
                    maybe (text "true") ($ m) mcond'
                ) $+$
                wrapBlock (
                    vcat (map (<> semi) $ sequence body' m) $+$
                    maybe empty ((<> semi) . ($ m)) mpost'
                )

        EmptyStmt -> pure $ const $ empty
        Block body -> do
            body' <- sequence body
            pure $ \m -> wrapBlock (vcat $ map (<> semi) $ sequence body' m)

        ContinueStmt -> pure $ const $ text "continue"
        BreakStmt -> pure $ const $ text "break"
        FallthroughStmt -> error "fallthrough unsupported"
        IncDecStmt d expr -> do
            expr' <- compileExpr expr
            let s = case d of Increment -> "++" ; Decrement -> "--"
            pure $ \m -> expr' m <> text s

compileCase :: TySrcAnnCaseHead -> (M.Map Type Doc -> Doc) -> DocCompiler
compileCase h body = case h of
    CaseDefault -> pure $ \m -> text "default:" $+$ nest indentLevel (body m $+$ text "break;")
    CaseExpr exprs -> do
        exprs' <- mapM compileExpr exprs

        pure $ \m ->
            vcat (map (\x -> text "case" <+> x <> colon) $ sequence exprs' m) $+$
            nest indentLevel (body m $+$ text "break;")

wrapBlock :: Doc -> Doc
wrapBlock d = text "{" $+$ nest indentLevel d $+$ text "}"

compileExpr :: TySrcAnnExpr -> DocCompiler
compileExpr = annCata phi where
    phi :: (Type, SrcSpan) -> Algebra TySrcAnnExprF DocCompiler
    phi _ expr = case expr of
        BinaryOp (Ann _ bin) e1 e2 -> do
            e1' <- e1
            e2' <- e2
            bin' <- compileBinaryOp bin
            pure $ \m -> prettyParens True (e1' m <+> bin' <+> e2' m)

        UnaryOp (Ann _ un) e -> do
            e' <- e
            un' <- compileUnaryOp un
            pure $ \m -> prettyParens True (un' <> prettyParens True (e' m))

        Conversion cty e -> do
            e' <- e
            let cty' = fst . topAnn $ cty
            pure $ \m ->
                prettyParens True (getType m cty') <>
                prettyParens True (e' m)

        Selector e (Ann _ (Ident sel)) -> do
            e' <- e
            pure $ \m -> e' m <> text "." <> text sel

        Index e1 e2 -> do
            e1' <- e1
            e2' <- e2
            pure $ \m -> prettyParens True (e1' m) <> prettyBrackets True (e2' m)

        Syn.Slice e elo ehi ecap -> do
            e' <- e
            elo' <- sequence elo
            ehi' <- sequence ehi
            ecap' <- sequence ecap

            pure $ \m ->
                text "take_slice" <> prettyParens True (
                    e' m <> comma
                    <+> hsep (
                        punctuate
                            comma
                            (map (maybe (text "-1") ($ m)) [elo', ehi', ecap'])
                    )
                )

        Call e Nothing args -> do
            e' <- e
            args' <- sequence args
            pure $ \m -> e' m <> prettyParens True (hsep (punctuate comma $ map ($ m) args'))

        Literal lit -> fmap const $ compileLiteral lit

        Variable v -> fmap const $ compileVariable v

        TypeAssertion _ _ -> error "type assertion not supported"

compileBinaryOp :: BinaryOp a -> Compiler Doc
compileBinaryOp bin = pure $ case bin of
    BitwiseAndNot -> text "& ~"
    _ -> pretty bin

compileUnaryOp :: UnaryOp a -> Compiler Doc
compileUnaryOp un = pure $ case un of
    BitwiseNot -> text "~"
    _ -> pretty un

compileTyType :: TySrcAnnType -> Compiler Doc
compileTyType = error "unimplemented: compileTyType"

compileType :: M.Map Type Doc -> Type -> Doc
compileType m ty = case unFix ty of
    VoidType -> text "void"
    IntType _ -> text "long"
    RuneType _ -> text "char"
    StringType _ -> text "string"
    FloatType _ -> text "double"
    BoolType _ -> text "bool"
    Array n ty' -> text "array<" <> getType m ty' <> text ", " <> int n <> text ">"
    T.Slice ty' -> text "vector<" <> getType m ty' <> text ">"
    Struct fields ->
        text "struct" $+$
        wrapBlock (vcat $ map
            (\(Ann _ sym, ty') ->
                maybe empty (\name -> getType m ty' <+> text name <> semi) (maybeSymbol sym)
            )
            fields
        )
    NilType -> error "can't compile nil type"
    BuiltinType _ -> error "can't compile builtin type"
    AliasType _  ty'-> getType m ty'
    UnknownType -> error "can't compile unknown type"
    FuncType _ _ -> error "can't compile function type"
    TypeSum _ -> error "can't compile type sum"

compileLiteral :: TySrcAnnLiteral -> Compiler Doc
compileLiteral (Ann _ lit) = pure $ case lit of
    IntLit i -> int i <> text "L"
    FloatLit f -> double f
    RuneLit r -> text (show r)
    StringLit s -> text (show s)

compileVariable :: GlobalId -> Compiler Doc
compileVariable = pure . makeVarName

getType :: M.Map Type Doc -> Type -> Doc
getType m ty = case M.lookup ty m of
    Nothing -> compileType m ty -- error $ "type lookup failed: " ++ show ty
    Just d -> d
