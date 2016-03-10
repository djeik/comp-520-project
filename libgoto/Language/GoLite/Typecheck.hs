{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.GoLite.Typecheck where

import Language.GoLite.Monad.Traverse
import Language.GoLite.Syntax.Typecheck

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State

newtype Typecheck a
    = Typecheck
        { runTypecheck
            :: ExceptT [TypecheckException] (
                StateT TypecheckState
                    Identity
            ) a
        }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError [TypecheckException]
        , MonadState TypecheckState
        )

-- TODO
type TypecheckException = String

-- TODO
type TypecheckState = String

instance MonadTraversal Typecheck where
    type TraversalError Typecheck = TypecheckException

    reportError = undefined -- TODO add to list of errors in state

    getErrors = undefined -- TODO extract list of errors from state

    abortTraversal = throwError =<< getErrors
