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

-- | Typechecking is a traversal requiring state and the possibility of fatal
-- errors.
instance MonadTraversal Typecheck where
    type TraversalError Typecheck = TypeError
    type TraversalException Typecheck = TypecheckError
    type TraversalState Typecheck = TypecheckState

    reportError e = modify $ \s -> s { _errors = e : _errors s }
    getErrors = _errors