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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.GoLite.Monad.Traverse (
  module Control.Monad.Except
, module Control.Monad.Identity
, module Control.Monad.State
, MonadTraversal (..)
, Traversal (..)
) where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State

-- | Describes a syntax tree traversal.
class
    ( Monad m
    , MonadState (TraversalState m) m
    , MonadError (TraversalException m) m
    ) => MonadTraversal m where

    -- | Fatal errors that can occur during the traversal.
    type TraversalException m :: *

    -- | Non-fatal errors that can occur during the traversal.
    type TraversalError m :: *

    -- | The state of the traversal.
    type TraversalState m :: *

    -- | Issues a non-fatal error, but continues the traversal.
    --
    -- The non-fatal errors are accumulated in the state.
    reportError :: TraversalError m -> m ()

    -- | Extracts the non-fatal errors from the traversal state.
    getErrors :: TraversalState m -> [TraversalError m]

newtype Traversal e s a
    = Traversal
        { runTraversal
            :: ExceptT e (
                StateT s
                    Identity
            ) a
        }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadError e
        , MonadState s
        )
