{-|
Module      : Language.GoLite.Pretty
Description : Pretty-printer definition and combinators.
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

The "Pretty" class is essentially a clone of the "Text.Show.Show" class, but
instead of aiming to output valid Haskell code, "Pretty" aims to output valid
GoLite code.
-}

module Language.GoLite.Pretty 
( Pretty(..)
, prettyPrefix
, prettyInfix
, prettyParens
, prettyBrackets
, prettyBraces
) where

import Language.GoLite.Precedence

import Control.Applicative ( Const(..) )
import Text.PrettyPrint

-- | Essentially a clone of 'Text.Show.Show'.
class Pretty a where
    prettyPrec :: Int -> a -> Doc
    pretty :: a -> Doc

    pretty = prettyPrec 0
    prettyPrec _ = pretty

instance Pretty Char where
    pretty = char

instance Pretty a => Pretty (Maybe a) where
    prettyPrec d e = case e of
        Just x -> prettyPrec d x
        Nothing -> empty

instance Pretty Doc where
    pretty = id

instance Pretty Int where
    pretty = int

instance Pretty Double where
    pretty = double

instance Pretty a => Pretty (Const a b) where
    pretty = pretty . getConst

-- | Pretty-prints an infix operator and its two operands.
prettyInfix :: (HasPrecedence sym, Pretty sym, Pretty l, Pretty r)
            => sym -> l -> r -> Doc
prettyInfix sym l r
    = prettyPrec (precedence sym) l
    <+> pretty sym
    <+> prettyPrec (precedence sym) r

-- | Pretty-prints a prefix operator and its operand.
prettyPrefix :: (HasPrecedence sym, Pretty sym, Pretty p)
             => sym -> p -> Doc
prettyPrefix sym p
    = pretty sym
    <> prettyPrec (precedence sym) p

prettyIf :: Bool -> (Doc -> Doc) -> (Doc -> Doc)
prettyIf b f = case b of
    True -> f
    False -> id

prettyParens :: Bool -> Doc -> Doc
prettyParens = flip prettyIf parens

prettyBrackets :: Bool -> Doc -> Doc
prettyBrackets = flip prettyIf brackets

prettyBraces :: Bool -> Doc -> Doc
prettyBraces = flip prettyIf braces
