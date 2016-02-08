{-|
Module      : Language.GoLite.SrcAnn
Description : GoLite syntax definitions with source annotations
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

module Language.GoLite.SrcAnn
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
) where

import Language.GoLite.Annotation
import Language.GoLite.Lexer.Core
import Language.GoLite.Syntax

import Control.Applicative
import Data.Functor.Foldable
import Data.Functor.Identity

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

withPushSrcAnnF :: Functor f => Parser (f (g a)) -> Parser (f (SrcAnn g a))
withPushSrcAnnF = fmap annPush . withSrcAnnF

-- | Runs a parser that produces a source-annotated syntax tree and wraps it in
-- another layer of source annotation.
withSrcAnnFix
    :: Parser (f (Fix (Ann SrcSpan f)))
    -> Parser (Fix (Ann SrcSpan f))
withSrcAnnFix = fmap Fix . withSrcAnnF

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

type SrcAnnPackage
    = Package SrcAnnIdent SrcAnnTopLevelDecl

type SrcAnnTopLevelDecl
    = TopLevelDecl SrcAnnDeclaration SrcAnnFunDecl

type SrcAnnVarDecl
    = VarDecl SrcAnnIdent SrcAnnType SrcAnnExpr

type SrcAnnTypeDecl
    = TypeDecl SrcAnnIdent SrcAnnType

type SrcAnnFunDecl
    = FunDecl SrcAnnIdent SrcAnnType SrcAnnStatement

type SrcAnnTypeF
    = TypeF SrcAnnIdent SrcAnnGoInt

type SrcAnnType
    = SrcAnnFix SrcAnnTypeF

type SrcAnnStatementF
    = StatementF
        SrcAnnDeclaration
        SrcAnnExpr
        SrcAnnIdent
        SrcAnnAssignOp
        SrcAnnCaseHead

type SrcAnnStatement
    = SrcAnnFix SrcAnnStatementF

type SrcAnnCaseHead
    = CaseHead SrcAnnExpr

type SrcAnnDeclaration
    = Declaration SrcAnnTypeDecl SrcAnnVarDecl

type SrcAnnExprF
    = ExprF SrcAnnIdent SrcAnnBinaryOp SrcAnnUnaryOp SrcAnnLiteral SrcAnnType

type SrcAnnExpr
    = SrcAnnFix SrcAnnExprF

type SrcAnnBinaryOp
    = SrcAnn BinaryOp ()

type SrcAnnAssignOp
    = SrcAnn AssignOp ()

type SrcAnnUnaryOp
    = SrcAnn UnaryOp ()

type SrcAnnLiteral
    = SrcAnn Literal ()

type SrcAnnIdent
    = SrcAnn Ident ()

type SrcAnnGoInt
    = SrcAnn (Const GoInt) ()

type SrcAnnGoFloat
    = SrcAnn (Const GoFloat) ()

type SrcAnnGoRune
    = SrcAnn (Const GoRune) ()

type SrcAnnGoString
    = SrcAnn (Const GoString) ()
