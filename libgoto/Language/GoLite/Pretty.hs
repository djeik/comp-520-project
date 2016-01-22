module Language.GoLite.Pretty 
( Pretty(..)
, prettys
, prettyPrefix
, prettyInfix
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

prettys :: Pretty a => a -> ShowS
prettys = prettysPrec 0

prettyList__ :: (a -> ShowS) ->  [a] -> ShowS
prettyList__ _     []     s = "[]" ++ s
prettyList__ prettyx (x:xs) s = '[' : prettyx x (prettyl xs) where
    prettyl []     = ']' : s
    prettyl (y:ys) = ',' : prettyx y (prettyl ys)

prettyInfix :: (HasPrecedence sym, Pretty sym, Pretty l, Pretty r)
            => sym -> l -> r -> ShowS
prettyInfix sym l r
    = prettysPrec (precedence sym) l
    . showChar ' '
    . showString (pretty sym)
    . showChar ' '
    . prettysPrec (precedence sym) r

prettyPrefix :: (HasPrecedence sym, Pretty sym, Pretty p)
             => sym -> p -> ShowS
prettyPrefix sym p
    = showString (pretty sym)
    . prettysPrec (precedence sym) p
