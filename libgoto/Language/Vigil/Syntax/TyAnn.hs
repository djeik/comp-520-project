{-|
Module      : Language.GoLite.Syntax.Basic
Description : Type synonyms for type-annotated Vigil syntax trees
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Vigil.Syntax.TyAnn where

import Language.Common.Annotation
import Language.Common.Pretty
import Language.Vigil.Types
import Language.Vigil.Syntax
import Language.Vigil.Syntax.Basic

import Control.Applicative ( Const(..) )

type TyAnn f a = Ann Type f a
type TyAnnFix f = Fix (Ann Type f)

type TyAnnProgram
    = Program BasicVarDecl TyAnnFunDecl

type TyAnnFunDecl
    = FunDecl BasicIdent BasicVarDecl TyAnnStatement

type TyAnnStatementF
    = StatementF TyAnnExpr TyAnnCondExpr TyAnnRef

type TyAnnStatement
    = Fix TyAnnStatementF

type TyAnnExpr
    = TyAnn (Expr
        BasicType
        TyAnnRef
        BasicIdent
        TyAnnVal
        TyAnnBinOp
        TyAnnUnOp
        TyAnnCondExpr) ()

type TyAnnBinOp = BasicBinOp

type TyAnnUnOp = BasicUnOp

type TyAnnCondExpr = CondExpr TyAnnVal TyAnnRef TyAnnBinCondOp TyAnnUnCondOp

type TyAnnBinCondOp = BasicBinCondOp

type TyAnnUnCondOp = BasicUnCondOp

type TyAnnRef = TyAnn (Ref BasicIdent (Ident ()) TyAnnVal) ()

type TyAnnVal = Val BasicIdent TyAnnLiteral

type TyAnnLiteral = TyAnn Literal ()

type TyAnnVigilInt = TyAnn (Const VigilInt) ()
type TyAnnVigilFloat = TyAnn (Const VigilFloat) ()
type TyAnnVigilRune = TyAnn (Const VigilRune) ()
type TyAnnVigilString = TyAnn (Const VigilString) ()

instance Pretty (f a) => Pretty (TyAnn f a) where
    pretty = pretty . bare