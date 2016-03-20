{-|
Module      : Language.GoLite.Syntax.Basic
Description : Type synonyms for unannotated syntax trees
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

This module provides type synonyms for practically all the syntax elements from
'Language.GoLite.Syntax.Types' in which data is unannotated.
-}

module Language.GoLite.Syntax.Basic
( BasicPackage
, BasicTopLevelDecl
, BasicVarDecl
, BasicTypeDecl
, BasicFunDecl
, BasicTypeF
, BasicType
, BasicStatementF
, BasicStatement
, BasicCaseHead
, BasicDeclaration
, BasicExprF
, BasicExpr
, BasicBinaryOp
, BasicAssignOp
, BasicUnaryOp
, BasicLiteral
, BasicIdent
) where

import Language.GoLite.Syntax.Types

-- | 'Package' without annotations.
type BasicPackage
    = Package BasicIdent BasicTopLevelDecl

-- | 'TopLevelDecl' without annotations.
type BasicTopLevelDecl
    = TopLevelDecl BasicDeclaration BasicFunDecl

-- | 'VarDecl' without annotations.
type BasicVarDecl
    = VarDecl BasicIdent BasicType BasicExpr

-- | 'TypeDecl' without annotations.
type BasicTypeDecl
    = TypeDecl BasicIdent BasicType

-- | 'FunDecl' without annotations.
type BasicFunDecl
    = FunDecl BasicIdent BasicType (Maybe BasicType) BasicStatement

-- | 'TypeF' without annotations.
type BasicTypeF
    = TypeF BasicIdent (Identity GoInt)

-- | 'Type' without annotations.
type BasicType
    = Fix BasicTypeF

-- | 'StatementF' without annotations.
type BasicStatementF
    = StatementF
        BasicDeclaration
        BasicExpr
        BasicIdent
        BasicAssignOp
        BasicCaseHead

-- | 'Statement' without annotations.
type BasicStatement
    = Fix BasicStatementF

-- | 'CaseHead' without annotations.
type BasicCaseHead
    = CaseHead BasicExpr

-- | 'Declaration' without annotations.
type BasicDeclaration
    = Declaration BasicTypeDecl BasicVarDecl

-- | 'ExprF' without annotations.
type BasicExprF
    = ExprF BasicIdent BasicBinaryOp BasicUnaryOp BasicLiteral BasicType

-- | 'Expr' without annotations.
type BasicExpr
    = Fix BasicExprF

-- | 'BinaryOp' without annotations.
type BasicBinaryOp
    = BinaryOp ()

-- | 'AssignOp' without annotations.
type BasicAssignOp
    = AssignOp ()

-- | 'UnaryOp' without annotations.
type BasicUnaryOp
    = UnaryOp ()

-- | 'Literal' without annotations.
type BasicLiteral
    = Literal ()

-- | 'Ident' without annotations.
type BasicIdent
    = Ident ()
