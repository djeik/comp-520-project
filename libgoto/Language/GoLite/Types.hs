{-|
Module      : Language.GoLite.Types
Description : Type definitions for the internal representation of GoLite code
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Defines the core types used in the internal representation of GoLite code.
-}

{-# LANGUAGE DeriveFunctor #-}

module Language.GoLite.Types where

import Language.GoLite.Syntax.SrcAnn

import Data.Functor.Foldable
import qualified Data.Map as M

-- | An entry in the symbol table.
data SymbolInfo
    -- | A symbol in scope.
    = VariableInfo
        { symLocation :: !SymbolLocation
        -- ^ The location of the symbol's definition.
        , symType :: !Type
        -- ^ The canonical type of the symbol.
        }
    | TypeInfo
        { symLocation :: !SymbolLocation
        -- ^ The location of the symbol's definition.
        , symType :: !Type
        -- ^ The canonical type of the symbol.
        }
    deriving (Eq, Ord, Show)

-- | The location of a symbol.
data SymbolLocation
    -- | The symbol is built-in to the compiler.
    = Builtin
    -- | The symbol is in a source file.
    | SourcePosition !SrcSpan
    deriving (Eq, Ord, Show)

-- | The base functor for a canonical representation of a GoLite type.
data GoTypeF f
    -- | The built-in void type.
    = VoidType
    -- | The built-in integer number type.
    | IntType
    -- | The built-in unicode character type.
    | RuneType
    -- | The built-in string type.
    | StringType
    -- | The built-in floating point number type.
    | FloatType
    -- | A statically-sized array of some type.
    | ArrayType Int f
    -- | A slice of some type.
    | SliceType f
    -- | A struct type.
    | StructType
        { structTypeFields :: M.Map SrcAnnIdent f }
    -- | The type for the predeclared identifier "nil". No other expression has
    -- this type.
    | NilType
    -- | Types for built-in functions, which are unrepresentable in the go
    -- typesystem.
    | BuiltinType BuiltinType
    | FuncType
        { funcTypeArgs :: [(SrcAnnIdent, f)]
        , funcTypeRet :: f
        }
    deriving (Eq, Functor, Ord, Show)

-- | The types of builtins.
data BuiltinType
    -- | The type of the @append@ builtin.
    = AppendType
    -- | The type of the @cap@ builtin.
    | CapType
    -- | The type of the @copy@ builtin.
    | CopyType
    -- | The type of the @delete@ builtin.
    | DeleteType
    -- | The type of the @len@ builtin.
    | LenType
    -- | The type of the @make@ builtin.
    | MakeType
    -- | The type of the @panic@ builtin.
    | PanicType
    deriving (Eq, Ord, Read, Show)

-- | A canonical representation of a GoLite type.
type Type = Fix GoTypeF

voidType :: Type
voidType = Fix VoidType

intType :: Type
intType = Fix IntType

runeType :: Type
runeType = Fix RuneType

stringType :: Type
stringType = Fix StringType

floatType :: Type
floatType = Fix FloatType

arrayType :: Int -> Type -> Type
arrayType n t = Fix $ ArrayType n t

sliceType :: Type -> Type
sliceType = Fix . SliceType

nilType :: Type
nilType = Fix NilType

builtin :: BuiltinType -> Type
builtin = Fix . BuiltinType

funcType :: [(SrcAnnIdent, Type)] -> Type -> Type
funcType args ret = Fix $ FuncType
    { funcTypeArgs = args
    , funcTypeRet = ret
    }

-- | The name of a symbol is simply the string assigned to it by the
-- programmer.
type SymbolName = String

-- | Scopes track definitions of symbols.
data Scope
    = Scope
        { scopeMap :: M.Map SymbolName SymbolInfo
        }
    deriving (Eq, Ord, Show)
