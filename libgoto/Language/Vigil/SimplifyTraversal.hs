{-|
Module      : Language.Vigil.Simplifier
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Contains the transformation functions from a typechecked GoLite AST to a Vigil
AST.
-}

module Language.Vigil.SimplifyTraversal where

import Language.Common.Annotation
import Language.GoLite.Syntax.Typecheck
import Language.Vigil.Simplify
import Language.Vigil.Syntax.Basic


simplifyExpr :: TySrcAnnExpr -> Simplify BasicExpr
simplifyExpr = annCata phi where
    phi :: TySrcSpan -> TySrcAnnExprF (Simplify BasicExpr) -> Simplify BasicExpr
    phi = undefined