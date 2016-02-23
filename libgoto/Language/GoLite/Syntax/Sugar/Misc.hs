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

-- | Constructs a package declaration.
package :: ident -> [topLevelDecl] -> Package ident topLevelDecl
package = Package

-- | Constructs a 'CaseHead' for the default case of a switch.
caseDefault :: CaseHead expr
caseDefault = CaseDefault

-- | Constructs a 'CaseHead' for a case in a switch that matches on
-- one or more expressions.
caseExpr :: [expr] -> CaseHead expr
caseExpr = CaseExpr
