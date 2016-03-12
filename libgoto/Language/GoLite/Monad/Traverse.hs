{-|
Module      : Language.GoLite.Monad.Traverse
Description : Traversing annotated syntax trees with class
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Defines a type family based approach for traversing general annotated syntax
trees.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.GoLite.Monad.Traverse (
  module Control.Monad.Except
, module Control.Monad.Identity
, module Control.Monad.State
, MonadTraversal (..)
, Traversal
) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State

-- | Describes a syntax tree traversal.
class Monad m => MonadTraversal m where
    type TraversalError m :: *

    -- | Issues an error, but continues the traversal.
    reportError :: TraversalError m -> m ()

    -- | Gets all errors that have been issued so far.
    getErrors :: m [TraversalError m]

    -- | Aborts the traversal.
    abortTraversal :: m a

newtype Traversal e s a
    = Traversal
        { runTraversal
            :: ExceptT [e] (
                StateT s
                    Identity
            ) a
        }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError [e]
        , MonadState s
        )