{-|
Module      : Language.GoLite.Syntax.Sugar
Description : GoLite syntax tree helpers
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

module Language.GoLite.Syntax.Sugar.Misc where

import Language.GoLite.Syntax.Types

package :: ident -> [topLevelDecl] -> Package ident topLevelDecl
package = Package

caseDefault :: CaseHead expr
caseDefault = CaseDefault

caseExpr :: [expr] -> CaseHead expr
caseExpr = CaseExpr
