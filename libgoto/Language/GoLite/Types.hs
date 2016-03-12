{-|
Module      : Language.GoLite.Types
Description : Type definitions for the internal representation of GoLite code
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental


-}

module Language.GoLite.Types where

import Language.GoLite.Syntax.SrcAnn

import qualified Data.Map as M

-- | An entry in the symbol table.
data SymbolInfo
    -- | A symbol in scope.
    = SymbolInfo
        { symLocation :: !SrcSpan
        -- ^ The location of the symbol's definition.
        , symType :: !Type
        -- ^ The canonical type of the symbol.
        }
    deriving (Eq, Ord, Show)

-- | A canonical representation of a GoLite type.
--
-- TODO
type Type = String

-- | The information for a variable is the same as for any symbol.
-- This synonym is meant only as a reminder.
type VariableInfo = SymbolInfo

-- | The information for a type is the same as for any symbol.
-- This synonym is meant only as a reminder.
type TypeInfo = SymbolInfo

-- | Variables are identified by a string.
type VariableName = String

-- | Types are identified by a string.
type TypeName = String

-- | Scopes track definitions of symbols. In GoLite, there are two kinds of
-- symbols: variables and types. Since they live in separate namespaces, each
-- scope consists of two maps, once for each namespace.
data Scope
    = Scope
        { scopeVariables :: M.Map VariableName SymbolInfo
        -- ^ The map of variables declared in this scope.
        , scopeTypes :: M.Map TypeName TypeInfo
        -- ^ The map of types declared in this scope.
        }
    deriving (Eq, Ord, Show)
