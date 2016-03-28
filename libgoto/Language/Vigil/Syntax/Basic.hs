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

-- | 'TypeF' without annotations.
type BasicTypeF = TypeF BasicIdent (Identity VigilInt)

-- | 'Type' without annotations.
type BasicType = Fix BasicTypeF

-- | 'CondExpr' without annotations.
type BasicCondExpr = CondExpr BasicVal BasicRelOp

-- | 'Ref' without annotations.
type BasicRef = Ref BasicIdent BasicVal

-- | 'Val' without annotations.
type BasicVal = Ref BasicIdent BasicLiteral

-- | 'BinaryOp' without annotations.
type BasicBinOp
    = BinaryOp ()

-- | 'UnaryOp' without annotations.
type BasicUnOp
    = UnaryOp ()

-- | 'RelOp' without annotations.
type BasicRelOp
    = RelOp ()

-- | 'Literal' without annotations.
type BasicLiteral
    = Literal ()

-- | 'Ident' without annotations.
type BasicIdent
    = Ident ()
