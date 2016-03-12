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

import qualified Data.Map as M
import Data.Bifunctor ( first )
import Data.Functor.Foldable ( cata )
import Text.PrettyPrint

-- | The typecheck monad tracks errors in its state. Fatal errors cause a true
-- exception to the thrown (in the 'ExceptT' sense) whereas non-fatal errors
-- merely causes new errors to be accumulated in the state. Hence, when
-- analyzing the 'Either' that results from running the @ExceptT@, 'Left'
-- indicates a fatal error and 'Right' indicates either success or non-fatal
-- errors.
newtype Typecheck a
    = Typecheck { runTypecheck :: Traversal TypecheckError TypecheckState a }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError TypecheckError
        , MonadState TypecheckState
        )

data TypecheckState
    = TypecheckState
        { _errors :: [TraversalError Typecheck]
        , _scopes :: [Scope]
        }
    deriving (Eq, Show)

-- | Regular typecheck/ing/ errors. (As opposed to typecheck/er/ errors.)
data TypeError
    = TypeMismatch
        { mismatchExpectedType :: Type
        -- ^ The expected type.
        , mismatchActualType :: Type
        -- ^ The actual type.
        , mismatchExpr :: SrcAnnExpr
        -- ^ The expression whose type is invalid.
        , exceptionReason :: Doc
        -- ^ A human-readable description of the error reason.
        }
    deriving (Eq, Show)

-- | All errors that can actually be thrown.
data TypecheckError
    = Abort
    -- ^ The typecheck is simply aborted.
    | ScopeImbalance
    -- ^ More scopes were popped than were pushed.

-- | An entry in the symbol table.
data SymbolInfo
    -- | A variable in scope.
    = SymbolInfo
        { symLocation :: !SrcSpan
        -- ^ The location of the symbol's definition.
        , symType :: !Type
        -- ^ The canonical type of the variable.
        }
    deriving (Eq, Ord, Show)

type VariableInfo = SymbolInfo
type TypeInfo = SymbolInfo

-- | Variables are identified by a string.
type VariableName = String

-- | Types are identified by a string.
type TypeName = String

-- | Scopes track definitions of symbols. In GoLite, there are two kinds of
-- symbols: variables and types. Since they live in separate namespaces, each
-- scope consists of two maps, once for each namespace.
data Scope
    = Scope
        { scopeVariables :: M.Map VariableName SymbolInfo
        , scopeTypes :: M.Map TypeName TypeInfo
        }
    deriving (Eq, Ord, Show)

instance MonadTraversal Typecheck where
    type TraversalError Typecheck = TypeError
    type TraversalException Typecheck = TypecheckError
    type TraversalState Typecheck = TypecheckState

    reportError e = modify $ \s -> s { _errors = e : _errors s }
    getErrors = _errors

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

-- | Adds a variable to the current scope.
--
-- If the variable already exists, then a non-fatal redeclaration error is
-- raised and the insertion is not performed.
declareVariable :: VariableName -> VariableInfo -> Typecheck ()
declareVariable = error "unimplemented: declare variable"

-- | Adds a type to the current scope.
--
-- If the type already exists, then a non-fatal redeclaration error is raised
-- and the insertion is not performed.
declareType :: TypeName -> TypeInfo -> Typecheck ()
declareType = error "unimplemented: declare type"

-- | Looks up a variable in the current scope stack.
--
-- This function does not throw exceptions. It pushes errors into the
-- typechecker /logic/ layer by using 'Maybe'. See 'lookupVariable\'' for a
-- variant that uses exceptions.
lookupVariable :: VariableName -> Typecheck (Maybe VariableInfo)
lookupVariable = error "unimplemented: lookup variable"

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

typecheckPackage :: SrcAnnPackage -> Typecheck TySrcAnnPackage
typecheckPackage (Package ident decls)
    = Package <$> pure ident <*> (mapM typecheckTopLevelDecl decls)

typecheckTopLevelDecl :: SrcAnnTopLevelDecl -> Typecheck TySrcAnnTopLevelDecl
typecheckTopLevelDecl d = case d of
    TopLevelDecl decl -> TopLevelDecl <$> typecheckDecl decl
    TopLevelFun decl -> TopLevelFun <$> typecheckFun decl

typecheckDecl :: SrcAnnDeclaration -> Typecheck TySrcAnnDeclaration
typecheckDecl d = case d of
    TypeDecl tyDeclBody -> TypeDecl <$> typecheckTypeDecl tyDeclBody
    VarDecl varDeclBody -> VarDecl <$> typecheckVarDecl varDeclBody

typecheckTypeDecl :: SrcAnnTypeDecl -> Typecheck SrcAnnTypeDecl
typecheckTypeDecl d = case d of
    TypeDeclBody (Ann a (Ident i)) ty -> do
        ty' <- canonicalize ty
        declareType i $ SymbolInfo { symLocation = a, symType = ty' }
        pure $ TypeDeclBody (Ann a (Ident i)) ty

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
            declareVariable i $ SymbolInfo a ty
            pure $ (Ann a (Ident i), expr')
        let (idents', exprs') = unzip ies'
        pure $ VarDeclBody idents' mty exprs'

typecheckFun :: SrcAnnFunDecl -> Typecheck TySrcAnnFunDecl
typecheckFun e = case e of
    FunDecl i@(Ann a (Ident name)) args mretty stmts -> do
        -- construct the canonical type of the function
        ty <- error "unimplemented: compute canonical declared function type" a name
        -- declare the function

        newScope
        -- declare the arguments
        stmts' <- typecheckFunctionBody ty stmts
        dropScope

        pure $ FunDecl i args mretty stmts'

typecheckExpr :: SrcAnnExpr -> Typecheck TySrcAnnExpr
typecheckExpr = cata f where
    wrap a = Fix . uncurry Ann . first (, a)

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
    :: Type
    -> [SrcAnnStatement]
    -> Typecheck [TySrcAnnStatement]
typecheckFunctionBody = error "unimplemented: typecheck function body"
