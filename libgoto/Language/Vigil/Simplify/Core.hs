{-|
Module      : Language.Vigil.Simplify.Core
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Convenience re-exports
-}

module Language.Vigil.Simplify.Core
( module Control.Monad
, module Control.Monad.Except
, module Data.Functor.Foldable
, module Language.Common.Annotation
, module Language.GoLite.Syntax.Typecheck
, module Language.Vigil.Simplify
, reinterpretGlobalIdEx
) where

-- Reexported modules

import Control.Monad ( forM )
import Control.Monad.Except
import Data.Functor.Foldable ( cata )

import Language.Common.Annotation
import Language.GoLite.Syntax.Typecheck
import Language.Vigil.Simplify

-- Modules needed here

import Language.GoLite.Types as G
import Language.Vigil.Types as V

-- | Reinterprets a GoLite global ID into a Vigil global ID, throwing an error
-- when the inner type is unrepresentable
reinterpretGlobalIdEx :: G.GlobalId -> Simplify (Maybe V.GlobalId)
reinterpretGlobalIdEx x = do
    case maybeSymbol $ bare $ gidOrigName x of
        Nothing -> pure Nothing
        Just _ -> case reinterpretGlobalId x of
            Nothing -> throwError $ UnrepresentableType ("reinterpretGlobalIdEx: " ++ show x)
            Just gid -> pure $ Just gid
