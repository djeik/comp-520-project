{-|
Module      : Language.GoLite.Syntax.Types
Description : Definitions and instances for type syntax
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.GoLite.Syntax.Types.Type where

import Language.GoLite.Pretty

import Data.Functor.Foldable

-- | A type.
data TypeF ident int f
    = SliceType f
    -- ^ A slice is a compound type, and represent a resizable array of
    -- elements of some other type.
    | ArrayType int f
    -- ^ An arrays is a compound type, and represents a statically-sized array
    -- of elements of some other type.
    | NamedType ident
    -- ^ A named type is the category into which all other types fall. It is
    -- simply an identifier.
    | StructType [(ident, f)]
    -- ^ A struct type represents a struct: a list of typed identifiers.
    deriving (Eq, Functor, Ord, Read, Show)

-- | Prints a recursive type structure bottom-up.
instance
    ( Pretty ident
    , Pretty int
    ) => Pretty (Fix (TypeF ident int)) where
    prettyPrec _ = cata f where -- TODO investigate precedence rules for types
        f :: (Pretty ident, Pretty int)
          => TypeF ident int Doc -> Doc
        f e = case e of
            SliceType t -> text "[]" <> t
            ArrayType i t -> prettyBrackets True (pretty i) <> t
            NamedType n -> pretty n
            StructType t ->
                text "struct" <+> prettyBraces True (
                    sep $ map (\(i, ty) ->
                        (pretty i) <+> ty <> semi
                    ) t
                )
