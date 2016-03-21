{-|
Module      : Language.GoLite.Syntax.SrcAnn
Description : GoLite syntax definitions with source annotations
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

This module provides type synonyms for practically all the types available in
"Language.GoLite.Syntax.Types" in which all the data is 'SrcSpan'-annotated
using 'Language.GoLite.Annotation.Ann'.

Also provided are parser combinators to facilitate capturing source position
information during parsing as well as stripping functions for removing
annotations from entire syntax trees.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.GoLite.Syntax.SrcAnn
( -- * General source-annotated functors
  SrcSpan(..)
, SrcAnn
, SrcAnnFix
, module Language.GoLite.Annotation
  -- * Parser combinators
, withSrcAnn
  -- ** Useful specialized parser combinators
, withSrcAnnF
, withSrcAnnFix
, withSrcAnnId
, withSrcAnnConst
, withPushSrcAnnF
, withPushSrcAnnFix
  -- * Source-annotated syntax definitions
, SrcAnnPackage
, SrcAnnTopLevelDecl
, SrcAnnVarDecl
, SrcAnnTypeDecl
, SrcAnnFunDecl
, SrcAnnTypeF
, SrcAnnType
, SrcAnnStatementF
, SrcAnnStatement
, SrcAnnCaseHead
, SrcAnnDeclaration
, SrcAnnExprF
, SrcAnnExpr
, SrcAnnBinaryOp
, SrcAnnAssignOp
, SrcAnnUnaryOp
, SrcAnnLiteral
, SrcAnnIdent
, SrcAnnGoInt
, SrcAnnGoFloat
, SrcAnnGoRune
, SrcAnnGoString
  -- * Annotation stripping functions
, barePackage
, bareTopLevelDecl
, bareFunDecl
, bareType
, bareStmt
, bareExpr
  -- * Reexports for dealing with @SourcePos@
, sourceName
, sourceLine
, sourceColumn
) where

import Language.GoLite.Annotation
import Language.GoLite.Lexer.Core
import Language.GoLite.Pretty
import Language.GoLite.Syntax.Basic
import Language.GoLite.Syntax.Types

import Control.Applicative
import Data.Functor.Foldable

-- | A source span has a beginning and an end that refer to locations in a
-- source file.
data SrcSpan
    = SrcSpan
        { srcStart :: !SourcePos
        , srcEnd :: !SourcePos
        }
    deriving (Eq, Ord, Show)

-- | General source-annotated functor value.
type SrcAnn f a = Ann SrcSpan f a

-- | General source-annotated functor fixed point.
type SrcAnnFix f = Fix (Ann SrcSpan f)

-- | Run a parser and annotate its result after applying a given wrapping
-- strategy with source position information.
withSrcAnn
    :: (a -> f b) -- ^ A wrapping strategy for the parsed data
    -> Parser a -- ^ The parser to annotate
    -> Parser (SrcAnn f b) -- ^ A parser that produces an annotated result.
withSrcAnn f p = do
    p1 <- getPosition
    x <- p
    p2 <- getPosition
    pure (Ann (SrcSpan p1 p2) (f x))

-- | Runs a parser producing a functor value and annotates it with source
-- position information.
--
-- > withSrcAnnF = withSrcAnn id
withSrcAnnF :: Parser (f a) -> Parser (SrcAnn f a)
withSrcAnnF = withSrcAnn id

-- | Combines 'withSrcAnnF' and 'annPush' to create a source-span annotation of
-- data wrapped within two functors.
withPushSrcAnnF :: Functor f => Parser (f (g a)) -> Parser (f (SrcAnn g a))
withPushSrcAnnF = fmap annPush . withSrcAnnF

-- | Runs a parser that produces a source-annotated syntax tree and wraps it in
-- another layer of source annotation.
withSrcAnnFix
    :: Parser (f (Fix (Ann SrcSpan f)))
    -> Parser (Fix (Ann SrcSpan f))
withSrcAnnFix = fmap Fix . withSrcAnnF

-- | Combines 'withPushSrcAnnF' and 'Fix' to add one more layer of annotated
-- fixed-point structure.
withPushSrcAnnFix
    :: Functor f
    => Parser (f (g (Fix (Ann SrcSpan g))))
    -> Parser (f (Fix (Ann SrcSpan g)))
withPushSrcAnnFix = fmap (fmap Fix) . withPushSrcAnnF

-- | Run a parser and annotate its result in the identity functor with source
-- position information.
withSrcAnnId :: Parser a -> Parser (SrcAnn Identity a)
withSrcAnnId = withSrcAnn Identity

-- | Runs a parser, packaging its result into a constant functor annotated with
-- source position information.
withSrcAnnConst :: Parser a -> Parser (SrcAnn (Const a) b)
withSrcAnnConst = withSrcAnn Const

-- | Removes source annotations from a package.
barePackage :: SrcAnnPackage -> BasicPackage
barePackage (Package i ds) = Package (bare i) (map bareTopLevelDecl ds)

-- | Removes source annotations from a top-level declaration (regular
-- type/variable or function).
bareTopLevelDecl :: SrcAnnTopLevelDecl -> BasicTopLevelDecl
bareTopLevelDecl (TopLevelDecl d) = TopLevelDecl (bareDecl d)
bareTopLevelDecl (TopLevelFun f) = TopLevelFun (bareFunDecl f)

-- | Removes source annotations from a function declaration.
bareFunDecl :: SrcAnnFunDecl -> BasicFunDecl
bareFunDecl (FunDecl fn args rty bod) =
    FunDecl
        (bare fn)
        (map (\(ident, ty) -> (bare ident, bareType ty)) args)
        (bareType <$> rty)
        (map bareStmt bod)

-- | Removes source annotations from a type/variable declaration.
bareDecl :: SrcAnnDeclaration -> BasicDeclaration
bareDecl (TypeDecl (TypeDeclBody i ty)) =
        TypeDecl (TypeDeclBody (bare i) (bareType ty))
bareDecl (VarDecl (VarDeclBody is ty es)) =
        VarDecl (VarDeclBody (map bare is) (bareType <$> ty) (map bareExpr es))

-- | Removes source annotations from a statement and all its inner statements.
bareStmt :: SrcAnnStatement -> BasicStatement
bareStmt = cata phi where
    phi (Ann _ (DeclStmt d)) = Fix (DeclStmt (bareDecl d))
    phi (Ann _ (ExprStmt e)) = Fix (ExprStmt (bareExpr e))
    phi (Ann _ (ShortVarDecl ids es)) = Fix (ShortVarDecl (map bare ids)
                                                (map bareExpr es))
    phi (Ann _ (Assignment es op es')) = Fix (Assignment (map bareExpr es)
                                                (bare op)
                                                (map bareExpr es'))
    phi (Ann _ (PrintStmt es)) = Fix (PrintStmt (map bareExpr es))
    phi (Ann _ (ReturnStmt e)) = Fix (ReturnStmt (bareExpr <$> e))
    phi (Ann _ (IfStmt ini e thens elses)) = Fix (IfStmt ini (bareExpr e)
                                                thens
                                                elses)
    phi (Ann _ (SwitchStmt ini e clauses)) = Fix (SwitchStmt ini
                                                    (bareExpr <$> e)
                        (map (\cl -> (bareCaseHead (fst cl), snd cl)) clauses))
    phi (Ann _ (ForStmt i e p d)) = Fix (ForStmt i (bareExpr <$> e) p d)
    phi (Ann _ BreakStmt) = Fix BreakStmt
    phi (Ann _ ContinueStmt) = Fix ContinueStmt
    phi (Ann _ FallthroughStmt) = Fix FallthroughStmt
    phi (Ann _ (Block ss)) = Fix (Block ss)
    phi (Ann _ EmptyStmt) = Fix EmptyStmt

-- | Removes source annotations from a case head.
bareCaseHead :: SrcAnnCaseHead -> BasicCaseHead
bareCaseHead CaseDefault = CaseDefault
bareCaseHead (CaseExpr es) = CaseExpr (map bareExpr es)

-- | Removes source annotations from an expression and all its inner expressions.
bareExpr :: SrcAnnExpr -> BasicExpr
bareExpr = cata phi where
    phi (Ann _ (BinaryOp op e e')) = Fix (BinaryOp (bare op) e e')
    phi (Ann _ (UnaryOp op e)) = Fix (UnaryOp (bare op) e)
    phi (Ann _ (Conversion ty e)) = Fix (Conversion (bareType ty) e)
    phi (Ann _ (Selector e i)) = Fix (Selector e (bare i))
    phi (Ann _ (Index e e')) = Fix (Index e e')
    phi (Ann _ (Slice e0 e1 e2 e3)) = Fix (Slice e0 e1 e2 e3)
    phi (Ann _ (TypeAssertion e ty)) = Fix (TypeAssertion e (bareType ty))
    phi (Ann _ (Call e ty es)) = Fix (Call e (bareType <$> ty) es)
    phi (Ann _ (Literal lit)) = Fix (Literal (bare lit))
    phi (Ann _ (Variable i)) = Fix (Variable (bare i))

-- | Removes source annotations from a type and all its inner types.
bareType :: SrcAnnType -> BasicType
bareType = cata phi where
    phi (Ann _ (SliceType ty)) = Fix (SliceType ty)
    phi (Ann _ (ArrayType i ty)) = Fix (ArrayType
                                            (Identity (getConst (bare i)))
                                            ty)
    phi (Ann _ (NamedType i)) = Fix (NamedType (bare i))
    phi (Ann _ (StructType fields)) = Fix (StructType (map
                                    (\(i, ty) -> (bare i, ty)) fields))

-- | 'Package' with source annotations.
type SrcAnnPackage
    = Package SrcAnnIdent SrcAnnTopLevelDecl

-- | 'TopLevelDecl' with source annotations.
type SrcAnnTopLevelDecl
    = TopLevelDecl SrcAnnDeclaration SrcAnnFunDecl

-- | 'VarDecl' with source annotations.
type SrcAnnVarDecl
    = VarDecl SrcAnnIdent SrcAnnType SrcAnnExpr

-- | 'TypeDecl' with source annotations.
type SrcAnnTypeDecl
    = TypeDecl SrcAnnIdent SrcAnnType

-- | 'FunDecl' with source annotations.
type SrcAnnFunDecl
    = FunDecl SrcAnnIdent SrcAnnType (Maybe SrcAnnType) SrcAnnStatement

-- | 'TypeF' with source annotations.
type SrcAnnTypeF
    = TypeF SrcAnnIdent SrcAnnGoInt

-- | 'SrcAnnFix' with source annotations.
type SrcAnnType
    = SrcAnnFix SrcAnnTypeF

-- | 'Statement' with source annotations.
type SrcAnnStatementF
    = StatementF
        SrcAnnDeclaration
        SrcAnnExpr
        SrcAnnIdent
        SrcAnnAssignOp
        SrcAnnCaseHead

-- | 'SrcAnnFix' with source annotations.
type SrcAnnStatement
    = SrcAnnFix SrcAnnStatementF

-- | 'CaseHead' with source annotations.
type SrcAnnCaseHead
    = CaseHead SrcAnnExpr

-- | 'Declaration' with source annotations.
type SrcAnnDeclaration
    = Declaration SrcAnnTypeDecl SrcAnnVarDecl

-- | 'ExprF' with source annotations.
type SrcAnnExprF
    = ExprF SrcAnnIdent SrcAnnBinaryOp SrcAnnUnaryOp SrcAnnLiteral SrcAnnType

-- | 'SrcAnnFix' with source annotations.
type SrcAnnExpr
    = SrcAnnFix SrcAnnExprF

-- | 'BinaryOp' with source annotations.
type SrcAnnBinaryOp
    = SrcAnn BinaryOp ()

-- | 'AssignOp' with source annotations.
type SrcAnnAssignOp
    = SrcAnn AssignOp ()

-- | 'UnaryOp' with source annotations.
type SrcAnnUnaryOp
    = SrcAnn UnaryOp ()

-- | 'Literal' with source annotations.
type SrcAnnLiteral
    = SrcAnn Literal ()

-- | 'Ident' with source annotations.
type SrcAnnIdent
    = SrcAnn Ident ()

-- | '' with source annotations.
type SrcAnnGoInt
    = SrcAnn (Const GoInt) ()

-- | 'GoFloat' with source annotations.
type SrcAnnGoFloat
    = SrcAnn (Const GoFloat) ()

-- | 'GoRune' with source annotations.
type SrcAnnGoRune
    = SrcAnn (Const GoRune) ()

-- | 'GoString' with source annotations.
type SrcAnnGoString
    = SrcAnn (Const GoString) ()

-- | Annotated functors can be pretty-printed by stripping the annotations and
-- pretty-printing the inner syntax tree.
instance (Functor f, Pretty (Fix f)) => Pretty (SrcAnnFix f) where
    pretty = pretty . bareF

-- | Annotated data can be pretty-printed by stripping the annotation and
-- pretty-printing the inner data.
instance Pretty (f a) => Pretty (SrcAnn f a) where
    pretty = pretty . bare
