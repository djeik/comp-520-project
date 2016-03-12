{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.GoLite.Typecheck where

import Language.GoLite.Monad.Traverse
import Language.GoLite.Syntax.Typecheck

-- TODO
type TypecheckException = String

-- TODO
type TypecheckState = String

newtype Typecheck a
    = Typecheck { runTypecheck :: Traversal TypecheckException TypecheckState a }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState TypecheckState
        , MonadError [TypecheckException]
        )

instance MonadTraversal Typecheck where
    type TraversalError Typecheck = TypecheckException

    reportError = undefined -- TODO add to list of errors in state

    getErrors = undefined -- TODO extract list of errors from state

    abortTraversal = throwError =<< getErrors
