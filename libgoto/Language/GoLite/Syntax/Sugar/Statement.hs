{-|
Module      : Language.GoLite.Syntax.Sugar.Statement
Description : Syntax sugar for synthesizing statements
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE TypeFamilies #-}

module Language.GoLite.Syntax.Sugar.Statement where

import Language.GoLite.Syntax.Types

assignment
    :: [expr]
    -> assignOp
    -> [expr]
    -> Fix (StatementF d expr i assignOp c)
assignment lhss op rhss = Fix $ Assignment lhss op rhss

printStmt :: [expr] -> Fix (StatementF d expr i a c)
printStmt = Fix . PrintStmt

returnStmt :: (Maybe expr) -> Fix (StatementF d expr i a c)
returnStmt = Fix . ReturnStmt

ifStmt
    :: f ~ Fix (StatementF d e i a c)
    => Maybe f
    -> e
    -> [f]
    -> Maybe [f]
    -> f
ifStmt ini c body ebody = Fix $ IfStmt ini c body ebody

switchStmt
    :: Maybe (Fix (StatementF d e i a c))
    -> Maybe e
    -> [(c, [Fix (StatementF d e i a c)])]
    -> Fix (StatementF d e i a c)
switchStmt ini expr cases = Fix $ SwitchStmt ini expr cases

forStmt
    :: f ~ Fix (StatementF d e i a c)
    => Maybe f
    -> Maybe e
    -> Maybe f
    -> [f]
    -> f
forStmt ini ex step body = Fix $ ForStmt ini ex step body

breakStmt :: Fix (StatementF d e i a c)
breakStmt = Fix BreakStmt

continueStmt :: Fix (StatementF d e i a c)
continueStmt = Fix ContinueStmt

fallthroughStmt :: Fix (StatementF d e i a c)
fallthroughStmt = Fix FallthroughStmt

declStmt :: decl -> Fix (StatementF decl e i a c)
declStmt = Fix . DeclStmt

exprStmt :: expr -> Fix (StatementF d expr i a c)
exprStmt = Fix . ExprStmt

shortVarDecl :: [ident] -> [expr] -> Fix (StatementF d expr ident a c)
shortVarDecl is es = Fix $ ShortVarDecl is es

block :: f ~ Fix (StatementF d e i a c) => [f] -> f
block ss = Fix $ Block ss

