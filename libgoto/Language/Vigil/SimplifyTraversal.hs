{-|
Module      : Language.Vigil.SimplifyTraversal
Description : Convert a typechecked GoLite program to a Vigil program
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Contains the transformation functions from a typechecked GoLite AST to a Vigil
AST.
-}

module Language.Vigil.SimplifyTraversal
( module Language.Vigil.Simplify.Top
) where

import Language.Vigil.Simplify.Top
