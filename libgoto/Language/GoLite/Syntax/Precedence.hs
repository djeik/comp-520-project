{-|
Module      : Language.GoLite.Syntax.Precedence
Description : Precedence class definition
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

module Language.GoLite.Syntax.Precedence where

-- | Class of operators having an associated precedence.
--
-- Allows us to abstract over operator types during pretty-printing.
class HasPrecedence a where
    precedence :: a -> Int
