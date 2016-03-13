{-|
Module      : Language.GoLite.Typecheck
Description : Typechecking traversal logic
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Defines the functions for typechecking a source-annotated syntax tree. The
result is a typechecked syntax tree, as described in
"Language.GoLite.Syntax.Typecheck".
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.GoLite.Typecheck where

import Language.GoLite.Misc
import Language.GoLite.Monad.Traverse
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Syntax.Typecheck
import Language.GoLite.Syntax.Types
import Language.GoLite.Types
import Language.GoLite.Typecheck.Types

import Control.Applicative ( (<|>) )
import Data.Bifunctor ( first )
import qualified Data.Map as M
import Data.Functor.Foldable ( cata )
import Text.PrettyPrint

-- | Pushes a given scope onto the stack.
pushScope :: Scope -> Typecheck ()
pushScope scope = modify $ \s -> s { _scopes = scope : _scopes s }

-- | Pops the topmost scope off the stack.
--
-- Aborts the traversal with a fatal 'ScopeImbalance' error.
popScope :: Typecheck Scope
popScope = do
    scopes <- gets _scopes
    case scopes of
        [] -> throwError ScopeImbalance
        s:ss -> do
            modify $ \t -> t { _scopes = ss }
            pure s

-- | Pushes an empty scope onto the stack.
newScope :: Typecheck ()
newScope = pushScope $ Scope M.empty M.empty

-- | Pops a scope from the stack, discarding it.
dropScope :: Typecheck ()
dropScope = popScope $> ()

-- | Gets the top scope.
--
-- if the scope stack is empty, throws 'EmptyScopeStack'.
topScope :: Typecheck Scope
topScope = do
    scopes <- gets _scopes
    case scopes of
        [] -> throwError EmptyScopeStack
        top : _ -> pure top

-- | Runs a pure function on the top scope of the stack. If the stack is empty,
-- throws 'EmptyScopeStack'.
modifyTopScope :: (Scope -> Typecheck Scope) -> Typecheck ()
modifyTopScope f = do
    scopes <- gets _scopes
    case scopes of
        [] -> throwError EmptyScopeStack
        top:others -> do
            top' <- f top
            modify $ \s -> s { _scopes = top' : others }

-- | Adds a variable to the top scope.
--
-- If the scope stack is empty, throws 'EmptyScopeStack'.
--
-- If the variable already exists, then a non-fatal redeclaration error is
-- raised and the insertion is not performed.
declareVariable :: VariableName -> VariableInfo -> Typecheck ()
declareVariable name info = modifyTopScope $ \s -> do
    case M.lookup name (scopeVariables s) of
        Just info' -> do
            reportError $ Redeclaration
                { redeclOrigin = info'
                , redeclNew = info
                , redeclNamespace = VariableSpace
                }
            pure s -- return the scope unchanged
        Nothing -> pure $ s
            { scopeVariables = M.insert name info $ scopeVariables s
            }

-- | Adds a type to the current scope.
--
-- If the type already exists, then a non-fatal redeclaration error is raised
-- and the insertion is not performed.
declareType :: TypeName -> TypeInfo -> Typecheck ()
declareType name info = modifyTopScope $ \s -> do
    case M.lookup name (scopeTypes s) of
        Just info' -> do
            reportError $ Redeclaration
                { redeclOrigin = info'
                , redeclNew = info
                , redeclNamespace = TypeSpace
                }
            pure s
        Nothing -> pure $ s
            { scopeTypes = M.insert name info $ scopeTypes s
            }

-- | Looks up a variable in the scope stack.
--
-- This function does not throw exceptions. It pushes errors into the
-- typechecker /logic/ layer by using 'Maybe'. See 'lookupVariable\'' for a
-- variant that uses exceptions.
lookupVariable :: VariableName -> Typecheck (Maybe VariableInfo)
lookupVariable name = foldr (<|>) Nothing . map (M.lookup name . scopeVariables) <$> gets _scopes

-- | Looks up a variable in the current scope stack.
--
-- This function will throw an undeclared variable error if the identifier
-- cannot be found in the stack.
lookupVariable' :: VariableName -> Typecheck VariableInfo
lookupVariable' name = do
    m <- lookupVariable name
    case m of
        Just info -> pure info
        Nothing -> error "unimplemented: variable lookup error"

-- | Looks up a type in the current scope stack.
--
-- This function does not throw exceptions. It pushes errors into the
-- typechecker /logic/ layer by using 'Maybe'. See 'lookupType\'' for a variant
-- that uses exceptions.
lookupType :: TypeName -> Typecheck (Maybe TypeInfo)
lookupType = error "unimplemented: lookup variable"

-- | Looks up a type in the current scope stack.
--
-- This function will throw an undeclared type error if the identifier cannot
-- be found in the stack.
lookupType' :: TypeName -> Typecheck TypeInfo
lookupType' name = do
    m <- lookupType name
    case m of
        Just info -> pure info
        Nothing -> error "unimplemented: type lookup error"

-- | Computes the canonical type representation for a source-annotated type.
canonicalize :: SrcAnnType -> Typecheck Type
canonicalize = error "unimplemented: type canonicalization"

-- | Typechecks a source position-annotated 'Package'.
typecheckPackage :: SrcAnnPackage -> Typecheck TySrcAnnPackage
typecheckPackage (Package ident decls)
    = Package <$> pure ident <*> (mapM typecheckTopLevelDecl decls)

-- | Typechecks a source position-annotated 'TopLevelDecl'.
typecheckTopLevelDecl :: SrcAnnTopLevelDecl -> Typecheck TySrcAnnTopLevelDecl
typecheckTopLevelDecl d = case d of
    TopLevelDecl decl -> TopLevelDecl <$> typecheckDecl decl
    TopLevelFun decl -> TopLevelFun <$> typecheckFun decl

-- | Typechecks a source position-annotated 'Declaration'.
typecheckDecl :: SrcAnnDeclaration -> Typecheck TySrcAnnDeclaration
typecheckDecl d = case d of
    TypeDecl tyDeclBody -> TypeDecl <$> typecheckTypeDecl tyDeclBody
    VarDecl varDeclBody -> VarDecl <$> typecheckVarDecl varDeclBody

-- | Typechecks a source position-annotated 'TypeDecl'.
typecheckTypeDecl :: SrcAnnTypeDecl -> Typecheck SrcAnnTypeDecl
typecheckTypeDecl d = case d of
    TypeDeclBody (Ann a (Ident i)) ty -> do
        ty' <- canonicalize ty
        declareType i $ SymbolInfo { symLocation = SourcePosition a, symType = ty' }
        pure $ TypeDeclBody (Ann a (Ident i)) ty

-- | Typechecks a source position-annotated 'VarDecl'.
typecheckVarDecl :: SrcAnnVarDecl -> Typecheck TySrcAnnVarDecl
typecheckVarDecl d = case d of
    VarDeclBody idents mty exprs -> do
        let (ies, rest) = safeZip idents exprs
        case rest of
            Nothing -> pure ()
            Just e -> case e of
                Left extraIdents ->
                    error "unimplemented: too many identifiers error" extraIdents
                Right extraExprs ->
                    error "unimplemented: too many expressions error" extraExprs
        ies' <- forM ies $ \(Ann a (Ident i), expr) -> do
            expr' <- typecheckExpr expr
            let (ty, _) = topAnn expr'
            case mty of
                Nothing -> pure ()
                Just declTy -> do
                    ty' <- canonicalize declTy
                    -- TODO this is probably too strict; needs to check for
                    -- assignment-compatibility rather than type equality.
                    when (ty /= ty') $ do
                        error "unimplemented: type mismatch error"
            declareVariable i $ SymbolInfo
                { symLocation = SourcePosition a
                , symType = ty
                }
            pure $ (Ann a (Ident i), expr')
        let (idents', exprs') = unzip ies'
        pure $ VarDeclBody idents' mty exprs'

-- | Typechecks a source position-annotated 'FunDecl'.
typecheckFun :: SrcAnnFunDecl -> Typecheck TySrcAnnFunDecl
typecheckFun e = case e of
    FunDecl i@(Ann a (Ident name)) args mretty stmts -> do
        -- construct the canonical type of the function
        ty <- error "unimplemented: compute canonical declared function type" a name
        -- declare the function

        newScope
        -- declare the arguments
        stmts' <- typecheckFunctionBody e ty stmts
        dropScope

        pure $ FunDecl i args mretty stmts'

-- | Typechecks a source position-annotated fixed point of 'ExprF'.
typecheckExpr :: SrcAnnExpr -> Typecheck TySrcAnnExpr
typecheckExpr = cata f where
    -- the boilerplate for reconstructing the tree but in which the branches
    -- now contain type information along with source position information.
    wrap a = Fix . uncurry Ann . first (, a)

    -- the monadic F-algebra that typechecks expressions bottom-up.
    f :: SrcAnn SrcAnnExprF (Typecheck TySrcAnnExpr) -> Typecheck TySrcAnnExpr
    f (Ann a e) = fmap (wrap a) $ case e of
        BinaryOp o me1 me2 ->
            let subs = (,) <$> me1 <*> me2
                in (,)
                    <$> (uncurry (typecheckBinaryOp o) =<< subs)
                    <*> (uncurry (BinaryOp o) <$> subs)

        UnaryOp o me -> (,)
            <$> (typecheckUnaryOp o =<< me)
            <*> (UnaryOp o <$> me)

        Conversion ty me -> do
            ty' <- canonicalize ty
            e' <- me
            typecheckConversion ty' e'
            pure (ty', Conversion ty e')

        Selector me (Ann b i) -> do
            e' <- me
            let (ty, _) = topAnn e'

            -- check that ty is a struct type, perform the lookup of the
            -- identifier in the struct to get the component's type; that
            -- becomes the type of the selector expression.
            error "unimplemented: selector typecheck" b i ty

        Index mie miev -> do
            ie <- mie -- the expression to index *in*
            iev <- miev -- the expression to index *by*

            -- see the documentation of the Index constructor in
            -- Syntax/Types/Expr.hs for how to typecheck indexing expressions.
            -- Needs cross-referencing with the GoLite spec to ignore pointers,
            -- etc.
            error "unimplemented: index typecheck" ie iev

        Slice me melo mehi mebound -> do
            e' <- me

            -- check that the combination of Maybes is valid (essentially a
            -- weeder check)
            -- According to the type of the expression to slice in, determine
            -- the type of the computed slice.
            error "unimplemented: slice typecheck" melo mehi mebound e'

        TypeAssertion me ty -> do
            e' <- me
            ty' <- canonicalize ty
            let (ety, _) = topAnn e'

            when (ety /= ty') $ do
                error "unimplemented: type assertion failed error"

            pure (ty', TypeAssertion e' ty)

        Call me mty margs -> do
            e' <- me
            args <- sequence margs

            -- See whether the type of e' is the special built-in type of
            -- `make` and check that that coresponds with whether mty is Just
            -- or Nothing.
            -- Check that e' is a function type.
            -- Check that the type of each argument corresponds to the declared
            -- types of the function parameters.
            -- The type of the call is the declared return type of the
            -- function.
            error "unimplemented: typecheck call" mty e' args

        Literal (Ann la l) -> fmap (\(ty, l') -> (ty, Literal $ Ann (ty, la) l')) $ case l of
            IntLit x -> do
                -- get the built-in integer type
                error "unimplemented: typecheck integer literal" l x

            FloatLit x -> do
                -- get the built-in float type
                error "unimplemented: typecheck float literal" l x

            RuneLit x -> do
                -- get the built-in rune type
                error "unimplemented: typecheck rune literal" l x

            StringLit x -> do
                -- get the built-in string type
                error "unimplemented: typecheck string literal" l x

        Variable x@(Ann _ (Ident i)) -> do
            info <- lookupVariable' i
            pure (symType info, Variable x)

    -- | Computes the canonical type of a binary operator expression.
    typecheckBinaryOp
        :: SrcAnnBinaryOp
        -> TySrcAnnExpr
        -> TySrcAnnExpr
        -> Typecheck Type
    typecheckBinaryOp = error "unimplemented: typecheck binary operator"
    -- TODO use assignment compatibility and operator typing rules

    -- | Computes the canonical type of a unary operator expression.
    typecheckUnaryOp
        :: SrcAnnUnaryOp
        -> TySrcAnnExpr
        -> Typecheck Type
    typecheckUnaryOp = error "unimplemented: typecheck unary operator"
    -- TODO use assignment compatibility and operator typing rules

    -- | Checks that a conversion is valid.
    typecheckConversion
        :: Type
        -> TySrcAnnExpr
        -> Typecheck ()
    typecheckConversion = error "unimplemented: typecheck conversion"

-- | Typecheck the body of a function of a given type.
--
-- Also, this function verifies that for non-void functions, every branch
-- returns a value and that no statements follow a return statement.
typecheckFunctionBody
    :: SrcAnnFunDecl
    -> Type
    -> [SrcAnnStatement]
    -> Typecheck [TySrcAnnStatement]
typecheckFunctionBody fun fty stmts = case stmts of
    -- function has no body
    [] -> if fty == voidType
        then pure []
        else do
            reportError $ TypeMismatch
                { mismatchExpectedType = voidType
                , mismatchActualType = fty
                , mismatchCause = MismatchFunction fun
                , errorReason = text "the function has no body"
                }
            pure []
    _ -> error "unimplemented: typecheck function body step case"
