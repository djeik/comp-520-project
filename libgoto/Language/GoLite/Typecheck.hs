{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.GoLite.Typecheck where

import Language.GoLite.Monad.Traverse
import Language.GoLite.Syntax.Typecheck

-- TODO
type TypecheckException = String

-- TODO
type TypecheckState = String

type Typecheck = Traversal TypecheckException TypecheckState

instance MonadTraversal Typecheck where
    type TraversalError Typecheck = TypecheckException

    reportError = undefined -- TODO add to list of errors in state

    getErrors = undefined -- TODO extract list of errors from state

    abortTraversal = throwError =<< getErrors
