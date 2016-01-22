module Language.GoLite.Precedence where

class HasPrecedence a where
    precedence :: a -> Int
