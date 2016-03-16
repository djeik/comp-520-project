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
{-# LANGUAGE ViewPatterns #-}

module Language.GoLite.Typecheck where

import Language.GoLite.Misc
import Language.GoLite.Monad.Traverse
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Syntax.Typecheck
import Language.GoLite.Syntax.Types as T
import Language.GoLite.Types as Ty
import Language.GoLite.Typecheck.Types

import Control.Applicative ( (<|>), Const(..) )
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
newScope = pushScope $ Scope { scopeMap = M.empty }

-- | Pops a scope from the stack, discarding it.
dropScope :: Typecheck ()
dropScope = popScope $> ()

withScope :: Typecheck a -> Typecheck a
withScope m = newScope *> m <* dropScope

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
declareSymbol :: SymbolName -> SymbolInfo -> Typecheck ()
declareSymbol name info = modifyTopScope $ \(Scope m) -> do
    case M.lookup name m of
        Just info' -> do
            reportError $ Redeclaration
                { redeclOrigin = info'
                , redeclNew = info
                }
            pure (Scope m) -- return the scope unchanged
        Nothing -> pure (Scope $ M.insert name info m)

-- | Looks up a variable in the scope stack.
--
-- This function does not report any errors.
lookupSymbol :: SymbolName -> Typecheck (Maybe SymbolInfo)
lookupSymbol name = foldr (<|>) Nothing . map (M.lookup name . scopeMap) <$> gets _scopes

-- | Computes the canonical type representation for a source-annotated type.
canonicalize :: SrcAnnType -> Typecheck Type
canonicalize = cata f where
    f :: SrcAnn SrcAnnTypeF (Typecheck Type) -> Typecheck Type
    f (Ann _ t) = case t of
        SliceType m -> fmap Fix $ Ty.Slice <$> m
        ArrayType (Ann _ (getConst -> n)) m -> fmap Fix $ Array <$> pure n <*> m
        StructType h -> fmap Fix $ Struct <$> traverse g h
        NamedType i@(Ann b (Ident name)) -> do
            minfo <- lookupSymbol name

            info <- case minfo of
                Just info -> case info of
                    VariableInfo _ _ -> do
                        reportError $ SymbolKindMismatch
                            { mismatchExpectedKind = typeKind
                            , mismatchActualInfo = info
                            , mismatchIdent = i
                            }
                        pure $ TypeInfo
                            { symLocation = SourcePosition b
                            , symType = unknownType
                            }

                    TypeInfo _ _ -> pure info

                Nothing -> do
                    reportError $ NotInScope { notInScopeIdent = i }
                    pure $ TypeInfo
                        { symLocation = SourcePosition b
                        , symType = unknownType
                        }

            pure $ symType info

    g :: (SrcAnnIdent, Typecheck Type) -> Typecheck (SrcAnn Symbol (), Type)
    g (i, m) = (,) <$> pure (annNat symbolFromIdent i) <*> m

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
        declareSymbol i $ TypeInfo { symLocation = SourcePosition a, symType = ty' }
        pure $ TypeDeclBody (Ann a (Ident i)) ty

-- | Typechecks a source position-annotated 'VarDecl'.
typecheckVarDecl :: SrcAnnVarDecl -> Typecheck TySrcAnnVarDecl
typecheckVarDecl d = case d of
    VarDeclBody idents mty exprs -> do
        let (ies, rest) = safeZip idents exprs

        unless (isNothing rest)
            (throwError $ WeederInvariantViolation
                        $ text "VarDecl with differing lengths on each side.")

        ies' <- forM ies $ \(Ann a (Ident i), expr) -> do
            (ty', expr') <- case mty of
                Nothing -> do
                    expr' <- typecheckExpr expr
                    let (ty, _) = topAnn expr'
                    pure (ty, expr')

                Just declTy -> do
                    declTy' <- canonicalize declTy
                    expr' <- requireExprType declTy' empty expr
                    pure (declTy', expr')

            declareSymbol i $ VariableInfo
                { symLocation = SourcePosition a
                , symType = ty'
                }

            pure $ (Ann a (Ident i), expr')

        let (idents', exprs') = unzip ies'

        pure $ VarDeclBody idents' mty exprs'

-- | Typechecks a source position-annotated 'FunDecl'.
typecheckFun :: SrcAnnFunDecl -> Typecheck TySrcAnnFunDecl
typecheckFun e = case e of
    FunDecl ident@(Ann a (Ident name)) args mretty stmts -> do
        -- construct the canonical type of the function
        ty <- funcType
            <$> forM args (\(i, t) -> (,)
                <$> pure (annNat symbolFromIdent i)
                <*> canonicalize t
            )
            <*> (case mretty of
                Nothing -> pure voidType
                Just t -> canonicalize t
            )

        declareSymbol name $ VariableInfo
            { symLocation = SourcePosition a
            , symType = ty
            }

        stmts' <- withScope $ do
            forM_ args $ \(Ann b (Ident argName), t) -> do
                argTy <- canonicalize t
                declareSymbol argName $ VariableInfo
                    { symLocation = SourcePosition b
                    , symType = argTy
                    }

            typecheckFunctionBody e ty stmts

        pure $ FunDecl ident args mretty stmts'

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

        Selector me i -> do
            e' <- me
            let (ty, b) = topAnn e'
            let sym = annNat symbolFromIdent i

            -- check that ty is a struct type, perform the lookup of the
            -- identifier in the struct to get the component's type; that
            -- becomes the type of the selector expression.
            ty' <- case unFix ty of
                Ty.Struct fs -> case lookup sym fs of
                    Nothing -> do
                        reportError $ NoSuchField
                            { fieldIdent = i
                            , fieldExpr = e'
                            }
                        pure unknownType
                    Just ty' -> pure ty'
                _ -> do
                    reportError $ TypeMismatch
                        { mismatchExpectedType
                            = structType [(sym, unknownType)]
                        , mismatchActualType
                            = ty
                        , mismatchCause = Ann b (Just e')
                        , errorReason = empty
                        }
                    pure unknownType

            pure (ty', Selector e' i)

        Index mie miev -> do
            ie <- mie -- the expression to index *in*
            iev <- miev -- the expression to index *by*

            let (iety, be) = topAnn ie
            let (defaultType -> ievty, bev) = topAnn iev
            -- see the documentation of the Index constructor in
            -- Syntax/Types/Expr.hs for how to typecheck indexing expressions.
            -- Needs cross-referencing with the GoLite spec to ignore pointers,
            -- etc.

            -- check that the expression to index in is indexable (is an array
            -- or a slice) and get the element type
            t <- case unFix iety of
                Ty.Slice t -> pure t
                Array _ t -> pure t
                _ -> do
                    reportError $ TypeMismatch
                        { mismatchExpectedType = arrayType 0 unknownType
                        , mismatchActualType = iety
                        , mismatchCause = Ann be (Just ie)
                        , errorReason = empty
                        }
                    pure unknownType

            -- check that the expression to index by is an integer
            case unFix ievty of
                IntType _ -> pure ()
                _ -> reportError $ TypeMismatch
                    { mismatchExpectedType = typedIntType
                    , mismatchActualType = ievty
                    , mismatchCause = Ann bev (Just iev)
                    , errorReason = empty
                    }

            pure (t, Index ie iev)

        T.Slice me melo mehi mebound -> do
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

        Literal (Ann la l) ->
            fmap (\ty -> (ty, Literal $ Ann (ty, la) l)) $ case l of
                IntLit _ -> pure untypedIntType
                FloatLit _ -> pure untypedFloatType
                RuneLit _ -> pure untypedRuneType
                StringLit _ -> pure untypedStringType

        Variable x@(Ann _ (Ident name)) -> do
            minfo <- lookupSymbol name
            case minfo of
                Just info -> pure (symType info, Variable x)
                Nothing -> do
                    reportError $ NotInScope { notInScopeIdent = x }
                    pure (unknownType, Variable x)

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
typecheckFunctionBody
    :: SrcAnnFunDecl
    -> Type
    -> [SrcAnnStatement]
    -> Typecheck [TySrcAnnStatement]
typecheckFunctionBody fun fty = mapM typecheckStmt where
    typecheckStmt :: SrcAnnStatement -> Typecheck TySrcAnnStatement
    typecheckStmt = cata f where
        f :: Ann SrcSpan SrcAnnStatementF (Typecheck TySrcAnnStatement)
          -> Typecheck TySrcAnnStatement
        f (Ann a s) = Fix . Ann a <$> case s of
            DeclStmt decl -> DeclStmt <$> typecheckDecl decl

            ExprStmt expr -> ExprStmt <$> typecheckExpr expr

            ShortVarDecl idents exprs -> do
                ies <- forM (zip idents exprs) $ \(Ann b (Ident i), e) -> do
                    e' <- typecheckExpr e
                    let (ty, _) = topAnn e'

                    declareSymbol i $ VariableInfo
                        { symLocation = SourcePosition b
                        , symType = ty
                        }

                    pure $ (Ann a (Ident i), e')

                pure $ uncurry ShortVarDecl $ unzip ies

            Assignment exprs1 assignOp exprs2 -> do
                es <- forM (zip exprs1 exprs2) $ \(e1, e2) -> do
                    typecheckAssignment e1 e2 assignOp

                let (exprs1', exprs2') = unzip es
                pure $ Assignment exprs1' assignOp exprs2'

            PrintStmt exprs -> PrintStmt <$> mapM typecheckExpr exprs

            ReturnStmt me -> case me of
                Just e -> do
                    e' <- typecheckExpr e
                    let (ty, b) = topAnn e'
                    -- TODO check that fty is not void, throw weeder invariant error otherwise
                    (fty, ty) <== TypeMismatch
                        { mismatchExpectedType = fty
                        , mismatchActualType = ty
                        , mismatchCause = Ann b (Just e')
                        , errorReason = text "the types are not assignment compatible"
                        }

                    when (fty == voidType)
                        (throwError $ WeederInvariantViolation
                                    $ text "Return with expr in void function")

                    pure $ ReturnStmt (Just e')

                Nothing -> do
                    when (fty /= voidType)
                        (throwError $ WeederInvariantViolation
                                    $ text "Return with no expr in non-void function")

                    pure $ ReturnStmt Nothing

            IfStmt minit cond thenBody melseBody -> IfStmt
                <$> sequence minit
                <*> requireExprType
                    typedBoolType
                    (text "the guard of an if statement must be a boolean")
                    cond
                <*> sequence thenBody
                <*> traverse sequence melseBody

            SwitchStmt minit mcond cases -> do
                minit' <- sequence minit
                mcond' <- traverse typecheckExpr mcond
                cases' <- forM cases $ \(hd, body) -> (,)
                    <$> typecheckCaseHead mcond' hd
                    <*> sequence body
                pure $ SwitchStmt minit' mcond' cases'

            ForStmt minit mcond mstep body -> ForStmt
                <$> sequence minit
                <*> sequence
                    (fmap
                        (requireExprType typedBoolType empty)
                        mcond)
                <*> sequence mstep
                <*> sequence body

            Block body -> Block <$> sequence body

            BreakStmt -> pure BreakStmt
            ContinueStmt -> pure ContinueStmt
            FallthroughStmt -> pure FallthroughStmt
            EmptyStmt -> pure EmptyStmt

        typecheckCaseHead
            :: Maybe TySrcAnnExpr -- ^ The switch-expression in context.
            -> SrcAnnCaseHead
            -> Typecheck TySrcAnnCaseHead
        typecheckCaseHead mcond hd
            = case hd of
                CaseDefault -> pure CaseDefault
                CaseExpr exprs -> case mcond of
                    Nothing -> CaseExpr
                        <$> mapM (requireExprType typedBoolType empty) exprs
                    Just e -> CaseExpr
                        <$> mapM (requireExprType (fst (topAnn e)) empty) exprs

typecheckAssignment
    :: SrcAnnExpr
    -> SrcAnnExpr
    -> SrcAnnAssignOp
    -> Typecheck (TySrcAnnExpr, TySrcAnnExpr)
typecheckAssignment e1 e2 (Ann _ op) = do
    e1' <- typecheckExpr e1
    let (ty, _) = topAnn e1'

    e2' <- case op of
        Assign -> checkE2 ty
        -- Special case for +=: allow typed string self-concat.
        PlusEq -> (case ty of
                    Fix (StringType True) -> checkE2 ty
                    _ -> checkAndSatisfy ty isArithmetic
                         $ text "operands to += must be string or arithmetic")
        _ ->    -- All assign-ops are arithmetic
                checkAndSatisfy ty isArithmetic
                         $ text "operands to assignment operations must be \
                                \arithmetic"


    pure (e1', e2')

    where
        -- Check that the second expression agrees in type with the first one.
        checkE2 ty = requireExprType ty empty e2
        -- Additionally check that the checked second type satisfies a predicate
        checkAndSatisfy ty p d = do
            checkE2 ty
            requireTypeToSatisfy p d e2

-- | Checks that an expression is assignment-compatible to a given type. If it
-- isn't a 'TypeMismatch' error is reported with the given reason.
requireExprType :: Type -> Doc -> SrcAnnExpr -> Typecheck TySrcAnnExpr
requireExprType t d e = do
    e' <- typecheckExpr e
    let (ty, b) = topAnn e'
    (t, ty) <== TypeMismatch
        { mismatchExpectedType = t
        , mismatchActualType = ty
        , mismatchCause = Ann b (Just e')
        , errorReason = d
        }
    pure e'

-- Checks that the type of an expression satisfies a given predicate. If not,
-- a 'UnsatisfyingType' error is reported with the given reason.
requireTypeToSatisfy :: (Type -> Bool) -> Doc -> SrcAnnExpr -> Typecheck TySrcAnnExpr
requireTypeToSatisfy p d e = do
    e' <- typecheckExpr e
    let (ty, b) = topAnn e'
    unless (p ty)
        (reportError UnsatisfyingType
                        { unsatOffender = ty
                        , unsatReason = d })
    pure e'

infixl 3 <==
-- | The assignment compatibility assertion operator, pronounced \"compat\"
-- verifies that the second type is assignment compatible to the first.
--
-- If it is compatible, then nothing happens.
-- If it isn't compatible, the provided error is reported.
-- Whether an error is reported or not is returned.
(<==) :: (Type, Type) -> TypeError -> Typecheck Bool
(<==) (t1, t2) e
    -- Special cases:
    -- Nothing can be assigned to nil.
    | isNilType t1 = e'
    -- Nothing can be assigned to untyped constants.
    | isUntyped t1 = e'
    -- Functions cannot assign or be assigned.
    | isFuncType t1 || isFuncType t2 = e'
    -- Builtins cannot assign or be assigned.
    | isBuiltinType t1 || isBuiltinType t2 = e'
    -- End of special cases.

    -- (1.)
    | t1 == t2 = pure False
    -- (2.)
    | not (isAliasType t1 && isAliasType t2) && unalias t1 == unalias t2
        = (unalias t1, unalias t2) <== e
    -- (3.)
    | isReferenceType t1 && isNilType t2 = pure False
    -- (4.)
    | isUntyped t2
        = (t1, defaultType t2) <== e
    | otherwise = e'
    where e' = reportError e *> pure True

{- Assignability
 - -------------

(Adapted from the Go specification.)

A value x is assignable to a variable of type T ("x is assignable to T") in any
of these cases:

 1. x's type is identical to T.
 2. at least one of x's type V and T is not an alias type, and both V and T
    have the same underlying type.
 3. x is the predeclared identifier nil and T is a pointer, function, slice,
    map, channel, or interface type.
 4. x is an untyped constant representable by a value of type T.
-}
