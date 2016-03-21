{-|
Module      : Language.GoLite.Typecheck.Types
Description : Definition of the Typecheck monad
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Defines the @Typecheck@ monad as an instance of
'Language.GoLite.Monad.Traverse.MonadTraversal'.
The purpose of the @Typecheck@ monad is to build type and source
position-annotated syntax trees from merely source-position annotated syntax
trees.

Source position-annotated trees are defined in "Language.GoLite.Syntax.SrcAnn".
Type- and source position-annotated trees are defined in
"Language.GoLite.Syntax.Typecheck".

This module simply defines the types however. The logic for performing that
transformation is in "Language.GoLite.Typecheck".
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.GoLite.Typecheck.Types where

import Language.GoLite.Monad.Traverse
import Language.GoLite.Pretty
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Syntax.Typecheck
import Language.GoLite.Types

import Text.PrettyPrint

-- | The typecheck monad tracks errors in its state. Fatal errors cause a true
-- exception to the thrown (in the 'ExceptT' sense) whereas non-fatal errors
-- merely causes new errors to be accumulated in the state. Hence, when
-- analyzing the 'Either' that results from running the @ExceptT@, 'Left'
-- indicates a fatal error and 'Right' indicates either success or non-fatal
-- errors.
newtype Typecheck a
    = Typecheck { unTypecheck :: Traversal TypecheckError TypecheckState a }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError TypecheckError
        , MonadState TypecheckState
        )

-- | Fully runs a computation in the 'Typecheck' monad using the default root
-- scope.
runTypecheck :: Typecheck a -> (Either TypecheckError a, TypecheckState)
runTypecheck t
    = runIdentity (
        runStateT (
            runExceptT (
                runTraversal (
                    unTypecheck t
                )
            )
        ) $
        TypecheckState
            { _errors = []
            , _scopes = [defaultRootScope]
            }
    )

-- | The state of the typechecker is the stack of scopes being traversed and
-- the list of accumulated non-fatal errors.
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
        , mismatchCause :: MismatchCause
        -- ^ The expression whose type is invalid.
        , errorReason :: Doc
        -- ^ A human-readable description of the error reason.
        }
    -- | The given symbol has already been declared in this scope.
    | Redeclaration
        { redeclOrigin :: SymbolInfo
        , redeclNew :: SymbolInfo
        }
    -- | The symbol used is not in scope.
    | NotInScope
        { notInScopeIdent :: SrcAnnIdent
        }
    -- | The symbol used is in scope, but is not of the proper kind
    -- (type or variable symbol)
    | SymbolKindMismatch
        { mismatchExpectedKind :: SymbolKind
        , mismatchActualInfo :: SymbolInfo
        , mismatchIdent :: SrcAnnIdent
        }
    -- | The given struct field does not exist.
    | NoSuchField
        { fieldIdent :: SrcAnnIdent
        , fieldExpr :: TySrcAnnExpr
        }
    -- | The type has been checked successfully, but cannot be used in the
    -- given expression.
    | UnsatisfyingType
        { unsatOffender :: Type
        , unsatReason :: Doc
        , errorLocation :: SrcSpan
        }
    -- | An error related to the type argument of a call occured (was present
    -- when not using the built-in make, or was not present when using it).
    | TypeArgumentError
        { errorReason :: Doc
        , typeArgument :: Maybe Type
        , errorLocation :: SrcSpan
        }
    -- | The number of arguments given to a call differ from the number of
    -- declared arguments to the function.
    | ArgumentLengthMismatch
        { argumentExpectedLength :: Int
        , argumentActualLength :: Int
        , errorLocation :: SrcSpan
        }
    -- | The types of a call expression do not match.
    | CallTypeMismatch
        { mismatchExpectedType :: Type
        , mismatchActualType :: Type
        , mismatchPosition :: Int
        , mismatchCause :: MismatchCause
        }
    -- | Two types involved in a binary operation could not be matched.
    | BinaryTypeMismatch
        { mismatchTypeL :: Type
        , mismatchTypeR :: Type
        , errorLocation :: SrcSpan
        }
    -- | Nil was used in a variable declaration without a type.
    | UntypedNil
        { errorLocation :: SrcSpan
        }
    -- | A variable was declared with a type that variables cannot have.
    | IllegalDeclType
        { offendingType :: Type
        , errorLocation :: SrcSpan
        }

    deriving (Eq, Show)

newtype ErrorPosition = ErrorPosition SymbolLocation

instance Pretty ErrorPosition where
    pretty (ErrorPosition loc) = case loc of
        SourcePosition s ->
            let start = srcStart s in
            let name = text (sourceName start) in
            let column = int (sourceColumn start) in
            let line = int (sourceLine start) in
            name <> colon <> line <> colon <> column <> colon
        Builtin ->
            text "builtin location:"

data ErrorSymbol = ErrorSymbol SymbolInfo

instance Pretty ErrorSymbol where
    pretty (ErrorSymbol sym) = case sym of
        VariableInfo {} ->
            text "variable (defined at"
            <+> pretty (ErrorPosition $ symLocation sym)
            <> text ")"
            <+> text "with declared type" $+$ nest indentLevel (pretty $ symType sym)
        TypeInfo {} ->
            text "type (defined at"
            <+> pretty (ErrorPosition $ symLocation sym)
            <> text ")"
            <+> text "with underlying type" $+$ nest indentLevel (pretty $ symType sym)

newtype ErrorSymbolKind = ErrorSymbolKind SymbolKind

instance Pretty ErrorSymbolKind where
    pretty (ErrorSymbolKind sym) = case sym of
        VariableInfo {} -> text "variable"
        TypeInfo {} -> text "type"

instance Pretty TypeError where
    pretty err = case err of
        TypeMismatch {} ->
            pretty (ErrorPosition (typeErrorLocation err)) $+$ nest indentLevel (
                text "cannot match expected type" $+$ nest indentLevel (
                    pretty (mismatchExpectedType err)
                ) $+$
                text "with actual type" $+$ nest indentLevel (
                    pretty (mismatchActualType err)
                ) $+$ (case mismatchCause err of
                    Ann _ Nothing -> empty
                    Ann _ (Just e) ->
                        text "in the expression" $+$ nest indentLevel (
                            pretty e
                        )
                )
            )
        _ ->
            pretty (ErrorPosition (typeErrorLocation err)) $+$ nest indentLevel (
                text "unknown type error"
            )

type MismatchCause = SrcAnn Maybe TySrcAnnExpr

-- | Determines the primary location of a type error.
typeErrorLocation :: TypeError -> SymbolLocation
typeErrorLocation e = case e of
    TypeMismatch { mismatchCause = Ann a _ } -> SourcePosition a
    Redeclaration { redeclNew = d } -> symLocation d
    NotInScope { notInScopeIdent = Ann a _ } -> SourcePosition a
    SymbolKindMismatch { mismatchIdent = Ann a _ } -> SourcePosition a
    NoSuchField { fieldIdent = Ann a _ } -> SourcePosition a
    UnsatisfyingType { errorLocation = a } -> SourcePosition a
    TypeArgumentError { errorLocation = a } -> SourcePosition a
    ArgumentLengthMismatch { errorLocation = a } -> SourcePosition a
    CallTypeMismatch { mismatchCause = Ann a _ } -> SourcePosition a
    BinaryTypeMismatch { errorLocation = a } -> SourcePosition a
    UntypedNil { errorLocation = a } -> SourcePosition a
    IllegalDeclType { errorLocation = a } -> SourcePosition a

-- | All errors that can actually be thrown.
data TypecheckError
    = ScopeImbalance
    -- ^ More scopes were popped than were pushed.
    | EmptyScopeStack
    -- ^ An attempt to modify the scope stack was made when the stack was
    -- empty.
    | WeederInvariantViolation
        { errorDescription :: Doc
        }
    -- ^ An illegal situation that should have been caught by a weeding pass
    -- arose during typechecking.
    | ParserInvariantViolation
        { errorDescription :: Doc
        }
    | UncategorizedOperator
    -- ^ An operator could not be categorized as either arithmetic, comparison,
    -- logical, or ordering.
    deriving (Eq, Show)

-- | Typechecking is a traversal requiring state and the possibility of fatal
-- errors.
instance MonadTraversal Typecheck where
    type TraversalError Typecheck = TypeError
    type TraversalException Typecheck = TypecheckError
    type TraversalState Typecheck = TypecheckState

    reportError e = modify $ \s -> s { _errors = e : _errors s }
    getErrors = _errors
