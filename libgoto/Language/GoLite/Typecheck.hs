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
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Language.GoLite.Typecheck
( runTypecheck
, _errors
, typecheckPackage
) where

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
canonicalize :: SrcAnnType -> Typecheck TySrcAnnType
canonicalize = annCata f where
    f :: SrcSpan -> SrcAnnTypeF (Typecheck TySrcAnnType) -> Typecheck TySrcAnnType
    f a t = case t of
        SliceType m -> do
            s@(Fix (Ann (t', _) _)) <- m
            pure $ Fix $ Ann (Fix $ Ty.Slice t', a) (SliceType s)
        ArrayType b@(Ann _ (getConst -> n)) m -> do
            s@(Fix (Ann (t', _) _)) <- m
            pure $ Fix $ Ann (Fix $ Array n t', a) (ArrayType b s)
        StructType h -> do
            h' <- forM h $ \(i, t') ->
                (,) <$> pure i <*> t'

            t' <- forM h $ \(i, m) ->
                (,) <$> pure (annNat symbolFromIdent i) <*> (fst . topAnn <$> m)

            pure $ Fix $ Ann (Fix $ Struct t', a) (StructType h')
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

            pure $ Fix $ Ann (symType info, a) (NamedType i)

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
        declareSymbol i $ TypeInfo
            { symLocation = SourcePosition a
            , symType = fst (topAnn ty')
            }
        pure $ TypeDeclBody (Ann a (Ident i)) ty

-- | Typechecks a source position-annotated 'VarDecl'.
typecheckVarDecl :: SrcAnnVarDecl -> Typecheck TySrcAnnVarDecl
typecheckVarDecl d = case d of
    VarDeclBody idents mty [] -> do
        declTy <- case mty of
            Nothing -> throwError ParserInvariantViolation
                { errorDescription
                    = text "at least one of the expression list and type must\
                            \be present in a declaration"
                }
            Just ty -> canonicalize ty

        forM_ idents $ \(Ann a (Ident i)) -> do
            declareSymbol i $ VariableInfo
                { symLocation = SourcePosition a
                , symType = defaultType (fst (topAnn declTy))
                }

        pure $ VarDeclBody idents (pure declTy) []

    VarDeclBody idents mty exprs -> do
        let (ies, rest) = safeZip idents exprs

        unless (isNothing rest) $ throwError WeederInvariantViolation
            { errorDescription
                = text "VarDecl with differing length on each side:"
                <+> text (show rest)
            }

        ty <- traverse canonicalize mty

        ies' <- forM ies $ \(Ann a (Ident i), expr) -> do
            (ty', expr') <- case ty of
                Nothing -> do
                    expr' <- typecheckExpr expr
                    let (tye, ae) = topAnn expr'
                    case tye of
                        Fix NilType -> do
                            reportError $ UntypedNil ae
                            pure (unknownType, expr')
                        _ ->
                            if not $ isValue tye then do
                                reportError $ IllegalNonvalueType tye ae
                                pure (unknownType, expr')
                            else pure (tye, expr')

                Just declTy -> do
                    let (t, _) = topAnn declTy
                    expr' <- requireExprType t empty expr
                    pure (t, expr')

            declareSymbol i $ VariableInfo
                { symLocation = SourcePosition a
                , symType = defaultType ty'
                }

            pure $ (Ann a (Ident i), expr')

        let (idents', exprs') = unzip ies'

        pure $ VarDeclBody idents' ty exprs'

-- | Typechecks a source position-annotated 'FunDecl'.
typecheckFun :: SrcAnnFunDecl -> Typecheck TySrcAnnFunDecl
typecheckFun e = case e of
    FunDecl ident@(Ann a (Ident name)) margs mretty stmts -> do
        rettyAnn <- traverse canonicalize mretty

        let retty = maybe voidType (fst . topAnn) rettyAnn

        args <- forM margs $ \(i, t) -> (,)
            <$> pure i
            <*> canonicalize t

        let ty = funcType
                (map (\(i, t) -> (annNat symbolFromIdent i, fst (topAnn t))) args)
                retty

        declareSymbol name $ VariableInfo
            { symLocation = SourcePosition a
            , symType = ty
            }

        stmts' <- withScope $ do
            forM_ args $ \(Ann b (Ident argName), argTy) -> do
                declareSymbol argName $ VariableInfo
                    { symLocation = SourcePosition b
                    , symType = fst (topAnn argTy)
                    }

            typecheckFunctionBody ty stmts

        FunDecl <$> pure ident <*> pure args <*> pure rettyAnn <*> pure stmts'

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
                    <$> (uncurry (typecheckBinaryOp o a) =<< subs)
                    <*> (uncurry (BinaryOp o) <$> subs)

        UnaryOp o me -> (,)
            <$> (typecheckUnaryOp o =<< me)
            <*> (UnaryOp o <$> me)

        Conversion ty me -> do
            ty' <- canonicalize ty
            e' <- me
            let cty = fst (topAnn ty')
            typecheckConversion cty e'
            pure (cty, Conversion ty' e')

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
            let (ty, b) = topAnn e'

            elemTy <- case unFix ty of
                Ty.Slice t -> pure t
                _ -> do
                    reportError $ TypeMismatch
                        { mismatchExpectedType = sliceType unknownType
                        , mismatchActualType = ty
                        , mismatchCause = Ann b (Just e')
                        , errorReason = empty
                        }
                    pure unknownType

            lo <- sequence melo
            hi <- sequence mehi
            bound <- sequence mebound

            let checkIndex i = do
                    let (ity, c) = topAnn i
                    (typedIntType, ity) <== TypeMismatch
                        { mismatchExpectedType = typedIntType
                        , mismatchActualType = ity
                        , mismatchCause = Ann c (Just i)
                        , errorReason = empty
                        }

            mapM_ (traverse checkIndex) [lo, hi, bound]

            pure (elemTy, T.Slice e' lo hi bound)

        TypeAssertion me ty -> do
            e' <- me
            ty' <- canonicalize ty

            throwError $ WeederInvariantViolation $ text "type assertion not supported"

            pure (fst $ topAnn ty', TypeAssertion e' ty')

        Call me mty margs -> do
            e' <- me
            args <- sequence margs
            ty <- forM mty canonicalize

            let normal = do
                    let (funTy, b) = topAnn e'
                    t <- case unFix funTy of
                        BuiltinType bu -> typecheckBuiltin a bu ty args
                        FuncType fargs fret -> do
                            typecheckCall a ty args fargs
                            pure fret
                        _ -> do
                            reportError $ TypeMismatch
                                { mismatchExpectedType = Fix $ FuncType
                                    { funcTypeArgs = map
                                        (\a' ->
                                            let (t, c) = topAnn a'
                                            in (Ann c Blank, t))
                                        args
                                    , funcTypeRet = unknownType
                                    }
                                , mismatchActualType = funTy
                                , mismatchCause = Ann b (Just e')
                                , errorReason = empty
                                }
                            pure unknownType

                    -- See whether the type of e' is the special built-in type
                    -- of `make` and check that that coresponds with whether
                    -- mty is Just or Nothing.
                    -- Check that e' is a function type.
                    -- Check that the type of each argument corresponds to the
                    -- declared types of the function parameters.
                    -- The type of the call is the declared return type of the
                    -- function.
                    pure (t, Call e' ty args)

            case bare (unFix e') of
                Variable (Ann b (Ident name)) -> do
                    minfo <- lookupSymbol name
                    case minfo of
                        Nothing -> throwError $ TypecheckerInvariantViolation
                            { errorDescription
                                = text "lookupSymbol is Nothing in Call"
                            }

                        Just info -> case info of
                            VariableInfo {} -> normal
                            TypeInfo {} -> do
                                let symTy = symType info
                                case ty of
                                    Just t -> do
                                        reportError $ TypeArgumentError
                                            { errorReason
                                                = text "only make can receive \
                                                \type arguments"
                                            , typeArgument
                                                = Just $ fst (topAnn t)
                                            , errorLocation = b
                                            }
                                    Nothing -> pure ()

                                let n = length args

                                case args of
                                    [] -> do
                                        reportError $ ArgumentLengthMismatch
                                            { argumentExpectedLength = 1
                                            , argumentActualLength = n
                                            , errorLocation = b
                                            }
                                        pure $ (unknownType, Call e' ty args)
                                    x:_ -> do
                                        typecheckConversion symTy x
                                        let at = Ann
                                                (symTy, b)
                                                (NamedType (Ann b $ Ident name))
                                        pure $
                                            ( symTy
                                            , Conversion (Fix at) x
                                            )

                _ -> normal

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

    -- | Computes the canonical type of a unary operator expression.
    typecheckUnaryOp
        :: SrcAnnUnaryOp
        -> TySrcAnnExpr
        -> Typecheck Type
    typecheckUnaryOp o e =
        let (ty, a) = topAnn e in
        case bare o of
            LogicalNot ->
                if isLogical $ unalias ty then
                    pure ty
                else do
                    reportError $ UnsatisfyingType
                        { unsatOffender = ty
                        , unsatReason = text "Expected logical type"
                        , errorLocation = a }
                    pure unknownType
            BitwiseNot ->
                if isIntegral $ unalias ty then
                    pure ty
                else do
                    reportError $ UnsatisfyingType
                        { unsatOffender = ty
                        , unsatReason = text "Expected integral type"
                        , errorLocation = a }
                    pure unknownType
            _ ->
                if isArithmetic $ unalias ty then
                    pure ty
                else do
                    reportError $ UnsatisfyingType
                        { unsatOffender = ty
                        , unsatReason = text "Expected numerical type"
                        , errorLocation = a }
                    pure unknownType


    -- | Checks that a conversion is valid.
    typecheckConversion
        :: Type
        -> TySrcAnnExpr
        -> Typecheck ()
    typecheckConversion ty e = do
        let (t', b) = topAnn e
        (ty, t') <== TypeMismatch
            { mismatchExpectedType = ty
            , mismatchActualType = t'
            , mismatchCause = Ann b (Just e)
            , errorReason = empty
            }
        pure ()

-- | Computes the canonical type of a binary operator expression.
typecheckBinaryOp
    :: SrcAnnBinaryOp
    -> SrcSpan -- ^ The span of the entire expression
    -> TySrcAnnExpr
    -> TySrcAnnExpr
    -> Typecheck Type
typecheckBinaryOp o a l r
    -- In general:
    -- Types must be indentical modulo typedness
    -- Expression is untyped iff both operands are untyped.
    | isArithmeticOp $ bare o = case bare o of
        -- Plus is defined on strings.
        Plus -> checkBinary (\ty -> isArithmetic ty || isString ty)
            (text "must be numerical or string")
        _ -> checkBinary isArithmetic (text "is not numerical")

    | isComparisonOp $ bare o =
        -- Special case: we check that the types are comparable, and
        -- not that they're equal. The resulting type is always a typed
        -- boolean, except when both operands are untyped booleans.
            let tyl = fst $ topAnn l in
            let tyr = fst $ topAnn r in do

            if isComparable tyl tyr then
                case (unFix tyl, unFix tyr) of
                    (BoolType False, BoolType False) -> pure untypedBoolType
                    (_, _) -> pure typedBoolType
            else do
                reportError $ BinaryTypeMismatch
                    { mismatchTypeL = tyl
                    , mismatchTypeR = tyr
                    , errorLocation = a }
                pure unknownType

    | isLogicalOp $ bare o =
        checkBinary isLogical (text "is not logical")

    | isOrderingOp $ bare o =
        -- Ordering operators produce booleans
        checkBinaryYieldingType
            isOrdered
            (text "cannot be ordered")
            (Just untypedBoolType)

    | isIntegralOp $ bare o =
        checkBinary isIntegral (text "is not integral")

    | otherwise = throwError UncategorizedOperator
    where
        -- Convenience alias.
        checkBinary p e = checkBinaryYieldingType p e Nothing

        -- Checks the type of the binary expression, yielding the given type if
        -- there is Just one, or the type of the left operand if it is Nothing.
        checkBinaryYieldingType p e mty =
            let (tyl, al) = topAnn l in
            let (tyr, ar) = topAnn r in do

            -- Check that the left type satisfies the predicate.
            when (not $ p tyl)
                (reportError $ UnsatisfyingType
                    { unsatOffender = tyl
                    , unsatReason = e
                    , errorLocation = al })

            -- Check that the right type satisfies the predicate.
            when (not $ p tyr)
                (reportError $ UnsatisfyingType
                    { unsatOffender = tyr
                    , unsatReason = e
                    , errorLocation = ar })

            if p tyr && p tyl then
                -- If they both satisfy the predicate, make sure they don't
                -- differ. We don't unalias them here.
                if defaultType tyr /= defaultType tyl then do
                    reportError $ BinaryTypeMismatch
                        { mismatchTypeL = tyl
                        , mismatchTypeR = tyr
                        , errorLocation = a }
                    pure unknownType
                else
                    -- If they are both untyped, the expression is untyped.
                    -- Otherwise, it is typed.
                    if isUntyped tyl && isUntyped tyr then
                        pure $ case mty of
                            Nothing -> tyl
                            Just ty -> ty
                    else
                        pure $ defaultType $ case mty of
                            Nothing -> tyl
                            Just ty -> ty
            else
                pure unknownType

-- | Typecheck the body of a function of a given type.
typecheckFunctionBody
    :: Type
    -> [SrcAnnStatement]
    -> Typecheck [TySrcAnnStatement]
typecheckFunctionBody fty = mapM typecheckStmt where
    typecheckStmt :: SrcAnnStatement -> Typecheck TySrcAnnStatement
    typecheckStmt = cata f where
        f :: Ann SrcSpan SrcAnnStatementF (Typecheck TySrcAnnStatement)
          -> Typecheck TySrcAnnStatement
        f (Ann a s) = Fix . Ann a <$> case s of
            DeclStmt decl -> DeclStmt <$> typecheckDecl decl

            ExprStmt expr -> do
                t <- typecheckExpr expr
                case bare $ unFix t of
                    (Call ee _ _) -> do
                        let (tyCall, _) = topAnn ee
                        when (not $ isAllowedInExprStmt tyCall)
                            (reportError $ UnsatisfyingType
                                { unsatOffender = tyCall
                                , unsatReason = text "cannot be used in \
                                  \expression statement context"
                                , errorLocation = a})
                    _ -> throwError $ ParserInvariantViolation
                                    $ text "ExprStmt should always be a call"
                pure $ ExprStmt t

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
                    typecheckAssignment a e1 e2 assignOp

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
                mcond' <- forM mcond $
                            (\e -> do
                                e' <- typecheckExpr e
                                let (ty, a) = topAnn e'
                                if not $ isValue ty then do
                                    reportError (IllegalNonvalueType
                                        { offendingType = ty
                                        , errorLocation = a })
                                    -- Reconstruct an expression annotated
                                    -- with unknown type.
                                    pure $ replaceTopAnn (\(_, a) -> (unknownType, a))
                                        e'
                                else pure $ e')

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
                CaseExpr exprs -> CaseExpr <$> case mcond of
                    Nothing -> mapM (requireExprType typedBoolType empty) exprs
                    Just e -> mapM (requireExprType (fst (topAnn e)) empty) exprs

typecheckAssignment
    :: SrcSpan -- ^ The source span of the assignment
    -> SrcAnnExpr
    -> SrcAnnExpr
    -> SrcAnnAssignOp
    -> Typecheck (TySrcAnnExpr, TySrcAnnExpr)
typecheckAssignment a e1 e2 (Ann aop op) = do
    e1' <- typecheckExpr e1
    let (ty, _) = topAnn e1'
    e2' <- requireExprType ty empty e2

    let bop = assignOpToBinOp op

    case bop of
        -- Occurs when we have a normal assignment. In this case we have nothing
        -- further to check.
        Nothing -> pure (e1', e2')
        -- Occurs when we have an assign-op. In this case we can check using the
        -- rules for the corresponding binary operation. The only difference
        -- between a = a `op` b and a `op=` b is that a is evaluated once in the
        -- second case - the same typing rules should apply.
        Just bop' -> do
            typecheckBinaryOp (Ann aop bop') a e1' e2'
            pure (e1', e2')

{- | Typechecks a built-in. Each built-in has special rules governing it.
    Below are the expected function signatures for them.

    * append([]T, T) -> []T
    * cap([]T) -> int
    * cap([x]T) -> int
    * copy([]T, []T) -> int
    * len(string) -> int
    * len([]T) -> int
    * len([x]T) -> int
    * make(<type literal []T>, int, [int]) -> []T

    Note that for array or slice types, aliases work equally well.
-}
typecheckBuiltin
    :: SrcSpan -- ^ The source position of the builtin call
    -> BuiltinType
    -> Maybe TySrcAnnType
    -> [TySrcAnnExpr]
    -> Typecheck Type
typecheckBuiltin a b mty exprs = do
    when (b /= MakeType)
        (case mty of
            Nothing -> pure ()
            Just ty ->
                reportError $ TypeArgumentError
                    { errorReason = text "only make can take type arguments"
                    , errorLocation = snd (topAnn ty)
                    , typeArgument = Just (fst (topAnn ty))
                    })
    case b of
        AppendType -> withArgLengthCheck 2 a exprs (\_ ->
            let x = head exprs in
            let y = exprs !! 1 in
            let (tyx, ax) = topAnn x in
            let (tyy, ay) = topAnn y in
            case unFix $ unalias tyx of
                Ty.Slice tyx' -> do
                    -- We have []T and U, check T <== U.
                    (tyx', tyy) <== TypeMismatch
                        { mismatchExpectedType = tyx'
                        , mismatchActualType = tyy
                        , mismatchCause = Ann ay (Just y)
                        , errorReason = empty }

                    pure tyx
                -- First argument is not a slice.
                _ -> do
                        reportError $ TypeMismatch
                            { mismatchExpectedType = sliceType tyy
                            , mismatchActualType = tyx
                            , mismatchCause = Ann ax (Just x)
                            , errorReason = empty }

                        pure unknownType)

        CapType -> withArgLengthCheck 1 a exprs (\_ ->
            let x = head exprs in
            let (ty, ax) = topAnn x in
            case unFix $ unalias ty of
                Ty.Array _ _ -> pure typedIntType
                Ty.Slice _ -> pure typedIntType
                _ -> do
                    reportError $ TypeMismatch
                        { mismatchExpectedType =
                            typeSum [sliceType unknownType,
                                    arrayType 0 unknownType]
                        , mismatchActualType = ty
                        , mismatchCause = Ann ax (Just x)
                        , errorReason = empty }

                    -- The return type is always int.
                    pure typedIntType)

        CopyType -> withArgLengthCheck 2 a exprs (\_ ->
            let x = head exprs in
            let y = exprs !! 1 in
            let (tyx, ax) = topAnn x in
            let (tyy, ay) = topAnn y in
            case (unFix $ unalias tyx, unFix $ unalias tyy) of

                -- Normal case: two slices
                (Ty.Slice tyx', Ty.Slice tyy') -> do

                    -- We have copy([]T, []U). Check that T <== U, since the
                    -- first argument is the destination.
                    (tyx', tyy') <== TypeMismatch
                        { mismatchExpectedType = tyx'
                        , mismatchActualType = tyy'
                        , mismatchCause = Ann ay (Just y)
                        , errorReason = empty }
                    pure typedIntType

                -- Try to have some better error reporting in case we have
                -- a slice somewhere.
                (Ty.Slice _, _) ->
                    mismatchWithInt tyx tyy (Ann ay $ Just y)

                (_, Ty.Slice _) ->
                    mismatchWithInt tyy tyx (Ann ax $ Just x)

                -- Finally if we have no good match, have an error for each
                -- argument.
                (_, _) -> do
                    mismatchWithInt tyx (sliceType unknownType) (Ann ax $ Just x)
                    mismatchWithInt tyy (sliceType unknownType) (Ann ay $ Just y))

        LenType -> withArgLengthCheck 1 a exprs (\_ ->
            let x = head exprs in
            let (ty, ax) = topAnn x in
            case unFix $ unalias ty of
                Ty.Array _ _ -> pure typedIntType
                Ty.Slice _ -> pure typedIntType
                Ty.StringType _ -> pure typedIntType
                _ -> mismatchWithInt ty
                        (typeSum [ arrayType 0 unknownType
                                , sliceType unknownType
                                , stringType True])
                            -- This expected typed string could as well be
                            -- untyped. For error reporting we don't care.
                        (Ann ax $ Just x))

        -- In Golang, make() can take either two or three arguments in the case
        -- of a slice. In the interest of making this a bit simpler, we're
        -- going to enforce three arguments. If we were to support maps or
        -- channels we'd need to change this.
        MakeType -> do
            case mty of
                Nothing -> do
                    reportError $ TypeArgumentError
                        { errorReason = text "make requires a type argument"
                        , errorLocation = a
                        , typeArgument = Nothing
                        }
                    pure unknownType
                Just ty ->
                    -- Note: this isn't really optimal for error messages, we'd
                    -- expect 3 arguments, but one will be a type.
                    withArgLengthCheck 2 a exprs $ const $
                        let x = head exprs in
                        let y = exprs !! 1 in
                        let (tyx, ax) = topAnn x in
                        let (tyy, ay) = topAnn y in
                        case (unFix $ unalias tyx, unFix $ unalias tyy) of
                            (Ty.IntType _, Ty.IntType _) -> pure (fst (topAnn ty))
                            (x', y') -> do
                                when (not $ isIntegral $ Fix y')
                                    (mismatchWithUnk tyy
                                        typedIntType (Ann ay $ Just y) $> ())
                                when (not $ isIntegral $ Fix x')
                                    (mismatchWithUnk tyx
                                        typedIntType (Ann ax $ Just x) $> ())
                                pure (fst (topAnn ty))

    where
        -- Checks that there are the specified number of arguments and, if yes,
        -- runs the given function, or reports an error if no.
        withArgLengthCheck n annot es f =
            if length es == n then
                f ()
            else do
                reportError $ ArgumentLengthMismatch
                    { argumentExpectedLength = n
                    , argumentActualLength = length es
                    , errorLocation = annot }
                pure unknownType

        -- Reports a type mismatch error, then returns the given type.
        mismatchWithTy expected actual cause ty = do
            reportError $ TypeMismatch
                { mismatchExpectedType = expected
                , mismatchActualType = actual
                , mismatchCause = cause
                , errorReason = empty }

            pure ty

        -- Reports a type mismatch error, then returns an unknown type
        mismatchWithUnk ex ac ca = mismatchWithTy ex ac ca unknownType

        -- Reports a type mismatch error, then returns a typed int type
        mismatchWithInt ex ac ca = mismatchWithTy ex ac ca typedIntType

-- | Typechecks a regular function call. See 'typecheckBuiltin' for
-- typechecking built-in functions.
--
-- Regular functions may not accept type arguments. The number of supplied
-- arguments must match the number of arguments in the function's signature.
-- Each expression's type must be assignment compatible to the declared type of
-- the formal parameter in the corresponding position of the function's
-- signature.
typecheckCall
    :: SrcSpan -- ^ The function call expression's source position
    -> Maybe TySrcAnnType -- ^ An optional type argument
    -> [TySrcAnnExpr] -- ^ The expression arguments
    -> [(SrcAnn Symbol (), Type)] -- ^ The arguments of the function
    -> Typecheck ()
typecheckCall pos tyArg exprs args = do
    case tyArg of
        Nothing -> pure ()
        Just (topAnn -> (t, a)) ->
            reportError $ TypeArgumentError
                { errorReason = text "regular functions cannot take type arguments"
                , errorLocation = a
                , typeArgument = Just t
                }

    matched <- case safeZip exprs args of
        (matched, excess) -> do
            case excess of
                Nothing -> pure ()
                Just _ ->
                    reportError $ ArgumentLengthMismatch
                        { argumentExpectedLength = length args
                        , argumentActualLength = length exprs
                        , errorLocation = pos
                        }

            pure matched

    forM_ (enumerate matched) $ \(i, (e, snd -> t)) -> do
        let (t', a) = topAnn e
        (t, t') <== CallTypeMismatch
            { mismatchExpectedType = t
            , mismatchActualType = t'
            , mismatchPosition = i
            , mismatchCause = Ann a (Just e)
            }

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
    -- Anything can be assigned to and from unknown type. This is because
    -- unknown type is generated only in the case of an error, so we don't
    -- want to generate extra errors on top of that.
    | t1 == Fix UnknownType || t2 == Fix UnknownType = pure False
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
