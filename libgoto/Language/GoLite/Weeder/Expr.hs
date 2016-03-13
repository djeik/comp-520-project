{-|
Module      : Language.GoLite.Weeder.Expr
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

"Weeder" traversal definition for expressions.
-}

module Language.GoLite.Weeder.Expr
( weedExpr
) where

import Language.GoLite.Weeder.Core

weedExpr :: SrcAnnExpr -> Weeder ()
weedExpr = undefined