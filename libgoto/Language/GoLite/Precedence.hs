{-|
Module      : Language.GoLite.Precedence
Description : Precedence class definition
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

module Language.GoLite.Precedence where

class HasPrecedence a where
    precedence :: a -> Int
