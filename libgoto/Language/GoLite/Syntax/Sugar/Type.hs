{-|
Module      : Language.GoLite.Syntax.Sugar.Type
Description : Syntax sugar for synthesizing types
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

module Language.GoLite.Syntax.Sugar.Type where

import Language.GoLite.Syntax.Types

sliceType :: Fix (TypeF ident int) -> Fix (TypeF ident int)
sliceType = Fix . SliceType

arrayType :: int -> Fix (TypeF ident int) -> Fix (TypeF ident int)
arrayType i t = Fix $ ArrayType i t

namedType :: ident -> Fix (TypeF ident int)
namedType = Fix . NamedType

structType :: [([ident], Fix (TypeF ident int))] -> Fix (TypeF ident int)
structType = Fix . StructType

