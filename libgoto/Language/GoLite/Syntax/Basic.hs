{-|
Module      : Language.GoLite.Syntax.Basic
Description : Type synonyms for unannotated syntax trees
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

module Language.GoLite.Syntax.Basic where

import Language.GoLite.Syntax.Types

import Data.Functor.Foldable

type BasicPackage
    = Package BasicIdent BasicTopLevelDecl

type BasicTopLevelDecl
    = TopLevelDecl BasicDeclaration BasicFunDecl

type BasicVarDecl
    = VarDecl BasicIdent BasicType BasicExpr

type BasicTypeDecl
    = TypeDecl BasicIdent BasicType

type BasicFunDecl
    = FunDecl BasicIdent BasicType BasicStatement

type BasicTypeF
    = TypeF BasicIdent (Identity GoInt)

type BasicType
    = Fix BasicTypeF

type BasicStatementF
    = StatementF
        BasicDeclaration
        BasicExpr
        BasicIdent
        BasicAssignOp
        BasicCaseHead

type BasicStatement
    = Fix BasicStatementF

type BasicCaseHead
    = CaseHead BasicExpr

type BasicDeclaration = Declaration BasicTypeDecl BasicVarDecl

type BasicExprF
    = ExprF BasicIdent BasicBinaryOp BasicUnaryOp BasicLiteral BasicType

type BasicExpr
    = Fix BasicExprF

type BasicBinaryOp
    = BinaryOp ()

type BasicAssignOp
    = AssignOp ()

type BasicUnaryOp
    = UnaryOp ()

type BasicLiteral
    = Literal ()

type BasicIdent = Ident ()
