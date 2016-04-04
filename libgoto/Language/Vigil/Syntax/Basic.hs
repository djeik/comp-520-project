{-|
Module      : Language.GoLite.Syntax.Basic
Description : Type synonyms for unannotated Vigil syntax trees
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

-}

module Language.Vigil.Syntax.Basic where

import Language.Vigil.Syntax
import Language.Vigil.Types

-- | 'Package' without annotations.
type BasicProgram
    = Program BasicVarDecl BasicFunDecl

-- | 'TopLevelDecl' without annotations.
type BasicVarDecl
    = VarDecl BasicIdent BasicType

-- | 'FunDecl' without annotations.
type BasicFunDecl
    = FunDecl BasicIdent BasicType (Maybe BasicType) BasicVarDecl BasicStatement

-- | 'StatementF' without annotations.
type BasicStatementF
    = StatementF BasicExpr BasicCondExpr BasicRef

-- | 'Statement' without annotations.
type BasicStatement
    = Fix BasicStatementF

-- | 'Expr' without annotations.
type BasicExpr
    = Expr
        BasicType
        BasicRef
        BasicIdent
        BasicVal
        BasicBinOp
        BasicUnOp
        BasicCondExpr

-- | 'CondExpr' without annotations.
type BasicCondExpr = CondExpr BasicVal BasicRef BasicBinCondOp BasicUnCondOp

-- | 'Ref' without annotations.
type BasicRef = Ref BasicIdent BasicVal

-- | 'Val' without annotations.
type BasicVal = Val BasicIdent BasicLiteral

-- | 'BinaryOp' without annotations.
type BasicBinOp
    = BinaryOp ()

-- | 'BinaryOp' without annotations.
type BasicBinCondOp
    = BinaryCondOp ()

-- | 'UnaryOp' without annotations.
type BasicUnOp
    = UnaryOp ()

-- | 'UnaryCondOp' without annotations.
type BasicUnCondOp
    = UnaryCondOp ()

-- | 'Literal' without annotations.
type BasicLiteral
    = Literal ()

type BasicType = Type

-- | 'Ident' without annotations.
type BasicIdent
    = Ident ()
