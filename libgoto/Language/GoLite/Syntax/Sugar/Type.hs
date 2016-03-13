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

-- | Constructs a type tree for a 'SliceType'.
sliceType :: Fix (TypeF ident int) -> Fix (TypeF ident int)
sliceType = Fix . SliceType

-- | Constructs a type tree for an 'ArrayType'.
arrayType :: int -> Fix (TypeF ident int) -> Fix (TypeF ident int)
arrayType i t = Fix $ ArrayType i t

-- | Constructs a type tree for a 'StructType'.
structType :: [(ident, Fix (TypeF ident int))] -> Fix (TypeF ident int)
structType = Fix . StructType

-- | Constructs a type tree for a 'NamedType'.
namedType :: ident -> Fix (TypeF ident int)
namedType = Fix . NamedType

