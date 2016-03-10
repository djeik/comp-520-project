{-|
Module      : Language.GoLite.Monad.Traverse
Description : GoLite syntax definitions with type and source annotations
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Defines type synonyms for typechecked syntax trees.
-}

module Language.GoLite.Syntax.Typecheck where

import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Syntax.Types

import Control.Applicative ( Const(..) )

-- The canonical form of a type.
type Type = String

-- | An annotation that consists of both type and source position information.
type TySrcSpan = (Type, SrcSpan)

-- | A type and source position annotated functor value.
type TySrcAnn f a = Ann TySrcSpan f a

-- | A fixed point of a type and source position annotated functor.
type TySrcAnnFix f = Fix (Ann TySrcSpan f)

-- | 'Package' with type and source position annotations.
type TySrcAnnPackage
    = Package SrcAnnIdent TySrcAnnTopLevelDecl

-- | 'TopLevelDecl' with type and source position annotations.
type TySrcAnnTopLevelDecl
    = TopLevelDecl TySrcAnnDeclaration TySrcAnnFunDecl

-- | 'VarDecl' with type and source position annotations.
type TySrcAnnVarDecl
    = VarDecl SrcAnnIdent SrcAnnType TySrcAnnExpr

-- | 'FunDecl' with type and source position annotations.
type TySrcAnnFunDecl
    = FunDecl SrcAnnIdent SrcAnnType TySrcAnnStatement

-- | 'Statement' with type and source position annotations.
type TySrcAnnStatementF
    = StatementF
        TySrcAnnDeclaration
        TySrcAnnExpr
        SrcAnnIdent
        SrcAnnAssignOp
        TySrcAnnCaseHead

-- | The fixed point of 'TySrcAnnStatementF' with type and source position
-- annotations.
type TySrcAnnStatement
    = TySrcAnnFix TySrcAnnStatementF

-- | 'CaseHead' with type and source position annotations.
type TySrcAnnCaseHead
    = CaseHead TySrcAnnExpr

-- | 'Declaration' with type and source position annotations.
type TySrcAnnDeclaration
    = Declaration SrcAnnTypeDecl TySrcAnnVarDecl

-- | 'ExprF' with type and source position annotations.
type TySrcAnnExprF
    = ExprF SrcAnnIdent SrcAnnBinaryOp SrcAnnUnaryOp TySrcAnnLiteral SrcAnnType

-- | The fixed point of 'TySrcAnnExprF' with type and source position
-- annotations.
type TySrcAnnExpr
    = TySrcAnnFix TySrcAnnExprF

-- | 'Literal' with type and source position annotations.
type TySrcAnnLiteral
    = TySrcAnn Literal ()

-- | '' with type and source position annotations.
type TySrcAnnGoInt
    = TySrcAnn (Const GoInt) ()

-- | 'GoFloat' with type and source position annotations.
type TySrcAnnGoFloat
    = TySrcAnn (Const GoFloat) ()

-- | 'GoRune' with type and source position annotations.
type TySrcAnnGoRune
    = TySrcAnn (Const GoRune) ()

-- | 'GoString' with type and source position annotations.
type TySrcAnnGoString
    = TySrcAnn (Const GoString) ()
