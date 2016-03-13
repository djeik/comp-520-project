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
{-# LANGUAGE StandaloneDeriving #-}

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
        { typeNamed :: M.Map SrcAnnIdent f }
    -- | The type for the predeclared identifier "nil". No other expression has
    -- this type.
    | NilType
    -- | An artificial type used for built-ins. It contains the actual type of
    -- the built-in, but is not assignment-compatible with anything.
    | BuiltinType f
    -- | A function type.
    | FuncType
        { args :: [(SrcAnnIdent, f)]
        , ret :: f
        }
    deriving (Functor)

deriving instance Eq f => Eq (GoTypeF f)
deriving instance Show f => Show (GoTypeF f)
deriving instance Ord f => Ord (GoTypeF f)

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

-- | The name of a symbol is simply the string assigned to it by the
-- programmer.
type SymbolName = String

-- | Scopes track definitions of symbols.
data Scope
    = Scope
        { scopeMap :: M.Map SymbolName SymbolInfo
        }
    deriving (Eq, Ord, Show)
