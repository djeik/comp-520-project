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

module Language.GoLite.Typecheck.Types where

import Language.GoLite.Monad.Traverse
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Syntax.Typecheck
import Language.GoLite.Types

import Text.PrettyPrint ( Doc )

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
    deriving (Eq, Show)

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

-- | All errors that can actually be thrown.
data TypecheckError
    = ScopeImbalance
    -- ^ More scopes were popped than were pushed.
    | EmptyScopeStack
    -- ^ An attempt to modify the scope stack was made when the stack was
    -- empty.
    | WeederInvariantViolation Doc
    -- ^ An illegal situation that should have been caught by a weeding pass
    -- arose during typechecking.
    | UncategorizedOperator
    -- ^ An operator could not be categorized as either arithmetic, comparison,
    -- logical, or ordering.

-- | Typechecking is a traversal requiring state and the possibility of fatal
-- errors.
instance MonadTraversal Typecheck where
    type TraversalError Typecheck = TypeError
    type TraversalException Typecheck = TypecheckError
    type TraversalState Typecheck = TypecheckState

    reportError e = modify $ \s -> s { _errors = e : _errors s }
    getErrors = _errors
