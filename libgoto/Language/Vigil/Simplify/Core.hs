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
, gToVIdent
, gIdToVIdent
) where

import Control.Monad ( forM )
import Control.Monad.Except
import Data.Functor.Foldable ( cata )


import Language.Common.Annotation
import Language.GoLite.Syntax.Typecheck
import Language.GoLite.Types
import Language.GoLite.Syntax.Basic as G
import Language.GoLite.Syntax.Types as G
import Language.Vigil.Simplify
import Language.Vigil.Syntax as V
import Language.Vigil.Syntax.Basic as V

-- | Converts a GoLite global identifier to a Vigil identifier
gIdToVIdent :: GlobalId -> V.BasicIdent
gIdToVIdent = gToVIdent . bare . gidOrigName

-- | Converts a GoLite identifier to a Vigil identifier
gToVIdent :: G.BasicIdent -> V.BasicIdent
gToVIdent = V.Ident . G.unIdent
