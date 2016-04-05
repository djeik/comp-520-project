{-|
Module      : Language.GoLite.Syntax.Typecheck
Description : GoLite syntax definitions with type and source annotations
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Defines type synonyms for typechecked syntax trees.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Language.GoLite.Syntax.Typecheck where

import Language.GoLite.Pretty
import Language.GoLite.Syntax.Precedence
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Syntax.Types as Ty
import Language.GoLite.Types

import Control.Applicative ( Const(..) )
import Data.Functor.Foldable

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
    = VarDecl GlobalId TySrcAnnType TySrcAnnExpr

-- | 'FunDecl' with type and source position annotations.
type TySrcAnnFunDecl
    = FunDecl GlobalId TySrcAnnType (Maybe TySrcAnnType) TySrcAnnStatement

-- | A fixed point of 'SrcAnnTypeF' with type and source position annotations.
type TySrcAnnType
    = TySrcAnnFix SrcAnnTypeF

-- | 'StatementF' with type and source position annotations.
type TySrcAnnStatementF
    = StatementF
        TySrcAnnDeclaration
        TySrcAnnExpr
        GlobalId
        SrcAnnAssignOp
        TySrcAnnCaseHead

-- | The fixed point of 'TySrcAnnStatementF' with type and source position
-- annotations.
type TySrcAnnStatement
    = SrcAnnFix TySrcAnnStatementF

-- | 'CaseHead' with type and source position annotations.
type TySrcAnnCaseHead
    = CaseHead TySrcAnnExpr

-- | 'Declaration' with type and source position annotations.
type TySrcAnnDeclaration
    = Declaration SrcAnnTypeDecl TySrcAnnVarDecl

-- | 'ExprF' with type and source position annotations.
type TySrcAnnExprF
    = ExprF SrcAnnIdent GlobalId SrcAnnBinaryOp SrcAnnUnaryOp TySrcAnnLiteral TySrcAnnType

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

instance Pretty TySrcAnnType where
    pretty = annCata phi where
        phi :: (Type, SrcSpan) -> SrcAnnTypeF Doc -> Doc
        phi (t, _) e = case e of
            SliceType d -> text "[]" <> d <+> pretty (Comment t)
            ArrayType n d -> prettyBrackets True (pretty n) <> d <+> pretty (Comment t)
            NamedType (bare -> name) -> pretty name <+> pretty (Comment t)
            StructType fields ->
                text "struct {" $+$ nest indentLevel (
                    vcat (map (\(bare -> i, d) -> pretty i <+> d) fields)
                ) $+$
                text "}" <+> pretty (Comment t)

instance Pretty TySrcAnnLiteral where
    pretty (Ann (ty, _) lit) = case lit of
        IntLit x -> pretty x <+> pretty (Comment ty)
        FloatLit x -> pretty x <+> pretty (Comment ty)
        RuneLit x -> pretty (text $ show x) <+> pretty (Comment ty)
        StringLit x -> doubleQuotes (text $ show x) <+> pretty (Comment ty)

instance Pretty TySrcAnnExpr where
    pretty = snd . cata f where
        f ::
            ( Pretty selId
            , Pretty id
            , Pretty bin
            , Pretty un
            , Pretty lit
            , Pretty ty
            , HasPrecedence bin
            , HasPrecedence un
            )
            => TySrcAnn (ExprF selId id bin un lit ty) (Int, Doc)
            -> (Int, Doc)
        f (Ann (ty, _) e) = case e of
            BinaryOp op (dl, l) (dr, r) -> (precedence op,) $
                prettyParens (dl <= precedence op) l
                <+>
                pretty op
                <+>
                pretty (Comment ty)
                <+>
                prettyParens (dr <= precedence op) r

            UnaryOp op (dp, p) -> (precedence op,) $
                pretty op
                <>
                prettyParens (dp <= precedence op) p
                <+>
                pretty (Comment ty)

            Literal l -> (6,) $ pretty l

            Variable x -> (6,) $ pretty x <+> pretty (Comment ty)

            Ty.Slice (ep, ex) lo hi up ->
                let p q = case q of
                        Nothing -> empty
                        Just (_, q') -> q'
                    in (6,) $
                        prettyParens (ep <= 6) ex <+>
                        prettyBrackets True (
                            p lo <> text ":" <> p hi <> case up of
                                Nothing -> empty
                                Just (_, u) ->
                                    text ":" <> u
                        ) <+>
                        pretty (Comment ty)
            Conversion t (_, e') -> (6,) $
                pretty t <> prettyParens True e' <+> pretty (Comment ty)
            Selector (ps, e') name -> (6,) $
                prettyParens (ps <= 6) e' <+> text "." <+> pretty name <+>
                pretty (Comment ty)
            Index (di, ei) (_, ej) -> (6,) $
                prettyParens (di <= 6) ei <+> prettyBrackets True ej <+> pretty (Comment ty)
            TypeAssertion (dex, ex) ty' -> (6,) $ cat
                [ prettyParens (dex <= 6) ex
                , text "."
                , prettyParens True (pretty ty')
                ] <+>
                pretty (Comment ty)
            Call (fp, fb) mty args -> (6,) $
                prettyParens (fp <= 6) (prettyPrec 6 fb) <>
                prettyParens True (
                    case args of
                        [] -> pretty ty
                        s -> case mty of
                            Nothing ->
                                sep $
                                punctuate comma $
                                map (pretty . snd) s
                            Just t ->
                                sep $
                                punctuate comma $
                                pretty t : map (pretty . snd) s
                )
