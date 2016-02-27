{-|
Module      : Language.GoLite.Syntax.Types.Expr
Description : Definitions and instances for expression syntax
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Language.GoLite.Syntax.Types.Expr where

import Language.GoLite.Syntax.Precedence
import Language.GoLite.Pretty

import Data.Functor.Foldable
import Data.String
import Text.PrettyPrint

-- | Integers in GoLite.
type GoInt = Int

-- | Floats in GoLite.
type GoFloat = Double

-- | Chars in GoLite.
type GoRune = Char

-- | Strings in GoLite.
type GoString = String

-- | Base functor for expressions, with holes for all the contained data.
data ExprF id bin un lit ty f
    = BinaryOp bin f f
    | UnaryOp un f
    | Conversion ty f
    | Selector f id
    {-|
    \"An expression of the form

    > a[x]

    denotes the element of the array, pointer to array, slice, string or map a
    indexed by @x@. The value @x@ is called the /index/ or /map key/,
    respectively.

    The following rules apply:

    * If @a@ is not a map

        * The index x must be of integer type or untyped;
        * It is in range if @0 <= x < len(a)@;
        * Otherwise it is out of range a constant index must be non-negative
        and representable by a value of type int.

    * For @a@ of array type A

        A constant index must be in range if x is out of range at run time, a
        run-time panic occurs @a[x]@ is the array element at index x and the type
        of @a[x]@ is the element type of A

    * For @a@ of pointer to array type

        @a[x]@ is shorthand for @(*a)[x]@

    * For @a@ of slice type /S/

        * If @x@ is out of range at run time, a run-time panic occurs.
        * @a[x]@ is the slice element at index @x@ and the type of @a[x]@ is the
        element type of /S/

    * For @a@ of string type

        * A constant index must be in range if the string @a@ is also constant;
        * If @x@ is out of range at run time, a run-time panic occurs;
        * @a[x]@ is the non-constant byte value at index @x@ and the type of @a[x]@
        is byte;
        * @a[x]@ may not be assigned to.

    * For a of map type M

        * @x@'s type must be assignable to the key type of /M/;
        * If the map contains an entry with key @x@, @a[x]@ is the map value with
        key @x@ and the type of @a[x]@ is the value type of /M/;
        * If the map is @nil@ or does not contain such an entry, @a[x]@ is the zero
        value for the value type of /M/;
        * Otherwise @a[x]@ is illegal.
    -}
    | Index
        { indexExpr :: f
        , indexExprValue :: f
        }
    -- | \"Slice expressions construct a substring or slice from a string,
    -- array, pointer to array, or slice. There are two variants: a simple form
    -- that specifies a low and high bound, and a full form that also specifies
    -- a bound on the capacity.\"
    | Slice
        { sliceExpr :: f
        -- ^ The expression to take a slice of.
        , sliceExprLow :: Maybe f
        -- ^ The low index to select from.
        , sliceExprHigh :: Maybe f
        -- ^ The high index to select up to.
        , sliceExprBound :: Maybe f
        -- ^ An upper bound on the capacity of the resulting slice object. No
        -- more than this many elements will be selected.
        }
    | TypeAssertion f ty
    | Call f (Maybe ty) [f]
    | Literal lit
    | Variable id
    deriving (Eq, Read, Show, Functor)

-- | Prints a recursive expression structure bottom up by dispatching to the
-- pretty-printers for any contained data. Internally, the pretty-printer
-- tracks the precedence of pretty-printed subexpressions in order to
-- parenthesize expressions only as much as is necessary to achieve the correct
-- parse.
instance
    ( Pretty id
    , Pretty bin
    , Pretty un
    , Pretty lit
    , Pretty ty
    , HasPrecedence bin
    , HasPrecedence un
    ) => Pretty (Fix (ExprF id bin un lit ty)) where
    pretty = snd . cata f where

        -- the F-algebra that we use here keeps track of the precendence levels
        -- of subexpressions so that we can properly parenthesize them in
        -- higher branches of the recursion.
        f ::
            ( Pretty id
            , Pretty bin
            , Pretty un
            , Pretty lit
            , Pretty ty
            , HasPrecedence bin
            , HasPrecedence un
            ) => ExprF id bin un lit ty (Int, Doc) -> (Int, Doc)
        f e = case e of
            BinaryOp op (dl, l) (dr, r) -> (precedence op,) $
                prettyParens (dl <= precedence op) l <+>
                pretty op <+>
                prettyParens (dr <= precedence op) r
            UnaryOp op (dp, p) -> (precedence op,) $
                pretty op <>
                prettyParens (dp <= precedence op) p
            Literal l -> (6, pretty l)
            Variable x -> (6, pretty x)
            Slice (ep, ex) lo hi up -> (6,) $
                prettyParens (ep <= 6) ex <>
                prettyBrackets True (
                    pretty (snd <$> lo) <>
                    text ":" <>
                    pretty (snd <$> hi) <>
                    case up of
                        Just u -> text ":" <> pretty (snd u)
                        Nothing -> empty
                )
            Call (fp, fb) ty args -> (6,) $
                prettyParens (fp <= 6) (prettyPrec 6 fb) <>
                prettyParens True (
                    case args of
                        [] -> pretty ty
                        s -> case ty of
                            Nothing ->
                                sep $
                                punctuate comma $
                                map (pretty . snd) s
                            Just t ->
                                sep $
                                punctuate comma $
                                pretty t : map (pretty . snd) s
                )
            Conversion ty (_, p) -> (6,) $
                pretty ty <> prettyParens True p
            Selector (ds, s) i -> (6,) $
                prettyParens (ds <= 6) s <> text "." <> pretty i
            TypeAssertion (dex, ex) ty -> (6,) $ cat
                [ prettyParens (dex <= 6) ex
                , text "."
                , prettyParens True (pretty ty)
                ]
            Index (dex, ex) (_, i) -> (6,) $ cat
                [ prettyParens (dex <= 6) ex
                , prettyBrackets True i
                ]


-- | A literal.
--
-- Derives functor so that we can painlessly use 'Language.GoLite.Annotation'.
data Literal a
    = IntLit GoInt
    | FloatLit GoFloat
    | RuneLit GoRune
    | StringLit GoString
    deriving (Eq, Read, Show, Functor)

-- | Pretty-prints a literal by using the 'Show' instance for the underlying
-- data.
instance Pretty (Literal a) where
    pretty l = case l of
        IntLit x -> pretty x
        FloatLit x -> pretty x
        StringLit x -> text $ show x
        RuneLit x -> text $ show x

-- | Identifiers are just wrapped strings.
data Ident a = Ident String deriving (Eq, Read, Show, Functor)

-- | Unwraps the string from the 'Ident' as displays it.
instance Pretty (Ident a) where
    pretty (Ident s) = text s

-- | String literals can be used instead of 'Ident' with @-XOverloadedStrings@.
instance IsString (Ident a) where
    fromString = Ident
