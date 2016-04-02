{-|
Module      : Language.Vigil.Simplify.Stmt
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Simplifications for statements
-}

module Language.Vigil.Simplify.Stmt where

import Language.GoLite.Syntax.SrcAnn
import Language.Vigil.Simplify.Core
import Language.Vigil.Simplify.Expr
import Language.Vigil.Syntax.TyAnn as V

simplifyStmt :: TySrcAnnStatement -> Simplify [TyAnnStatement]
simplifyStmt = annCata phi where
    phi :: SrcSpan
        -> TySrcAnnStatementF (Simplify [TyAnnStatement])
        -> Simplify [TyAnnStatement]
    phi = undefined
