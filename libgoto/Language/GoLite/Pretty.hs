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
, prettys
, prettyPrefix
, prettyInfix
, showBrackets
, prettysParen
, prettyBrackets
, prettysBrackets
) where

import Language.GoLite.Precedence

-- | Essentially a clone of 'Text.Show.Show'.
class Pretty a where
    pretty :: a -> String
    prettysPrec :: Int -> a -> ShowS
    prettyList :: [a] -> ShowS

    prettysPrec _ x s = pretty x ++ s
    pretty x = prettys x ""
    prettyList ls s = prettyList__ prettys ls s

instance Pretty Char where
    pretty c = show c
    prettyList s = showString s

instance Pretty a => Pretty (Maybe a) where
    prettysPrec d e = case e of
        Just x -> prettysPrec d x
        Nothing -> id

instance Pretty Int where
    pretty i = show i

-- | Pretty-prints something (as a difference list) with the lowest precedence.
prettys :: Pretty a => a -> ShowS
prettys = prettysPrec 0

prettyList__ :: (a -> ShowS) ->  [a] -> ShowS
prettyList__ _     []     s = "[]" ++ s
prettyList__ prettyx (x:xs) s = '[' : prettyx x (prettyl xs) where
    prettyl []     = ']' : s
    prettyl (y:ys) = ',' : prettyx y (prettyl ys)

-- | Pretty-prints an infix operator and its two operands.
prettyInfix :: (HasPrecedence sym, Pretty sym, Pretty l, Pretty r)
            => sym -> l -> r -> ShowS
prettyInfix sym l r
    = prettysPrec (precedence sym) l
    . showChar ' '
    . showString (pretty sym)
    . showChar ' '
    . prettysPrec (precedence sym) r

-- | Pretty-prints a prefix operator and its operand.
prettyPrefix :: (HasPrecedence sym, Pretty sym, Pretty p)
             => sym -> p -> ShowS
prettyPrefix sym p
    = showString (pretty sym)
    . prettysPrec (precedence sym) p

prettysParen :: Bool -> ShowS -> ShowS
prettysParen b s = case b of
    True -> showString "(" . s . showString ")"
    False -> s

showBrackets :: Show a => Bool -> a -> ShowS
showBrackets True s = showString "[" . shows s . showString "]"
showBrackets False s = shows s

prettyBrackets :: Pretty a => Bool -> a -> ShowS
prettyBrackets True s = showString "[" . prettys s . showString "]"
prettyBrackets False s = prettys s

prettysBrackets :: Bool -> ShowS -> ShowS
prettysBrackets b s = case b of
    True -> showString "[" . s . showString "]"
    False -> s
