{-|
Module      : Language.GoLite.Syntax.Basic
Description : Type synonyms for type-annotated Vigil syntax trees
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

-}

module Language.Vigil.Syntax.TyAnn where

import Language.Common.Annotation
import Language.GoLite.Types
import Language.Vigil.Syntax
import Language.Vigil.Syntax.Basic

import Control.Applicative ( Const(..) )

type TyAnn f a = Ann Type f a
type TyAnnFix f = Fix (Ann Type f)

type TyAnnProgram
    = Program BasicVarDecl TyAnnFunDecl

type TyAnnFunDecl
    = FunDecl BasicIdent TyAnnType (Maybe TyAnnType) BasicVarDecl TyAnnStatement

type TyAnnStatementF
    = StatementF TyAnnExpr TyAnnCondExpr TyAnnRef

type TyAnnStatement
    = TyAnnFix TyAnnStatementF

type TyAnnExpr
    = Expr
        TyAnnType
        TyAnnRef
        BasicIdent
        TyAnnVal
        BasicBinOp
        BasicUnOp
        TyAnnCondExpr

type TyAnnType = TyAnnFix BasicTypeF

type TyAnnCondExpr = CondExpr TyAnnVal BasicRelOp BasicLogOp

type TyAnnRef = Ref BasicIdent TyAnnVal

type TyAnnVal = Val BasicIdent TyAnnLiteral

type TyAnnLiteral = TyAnn Literal ()

type TyAnnVigilInt = TyAnn (Const VigilInt) ()
type TyAnnVigilFloat = TyAnn (Const VigilFloat) ()
type TyAnnVigilRune = TyAnn (Const VigilRune) ()
type TyAnnVigilString = TyAnn (Const VigilString) ()