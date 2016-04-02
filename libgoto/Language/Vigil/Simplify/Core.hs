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
( module Data.Functor.Foldable
, module Language.Common.Annotation
, module Language.GoLite.Syntax.Typecheck
, module Language.Vigil.Simplify
) where

import Data.Functor.Foldable ( cata )
import Language.Common.Annotation
import Language.GoLite.Syntax.Typecheck
import Language.Vigil.Simplify
