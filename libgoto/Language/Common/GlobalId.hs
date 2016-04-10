{-|
Module      : Language.Common.GlobalId
Description : Global numbering
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

module Language.Common.GlobalId where

-- | The origin of a piece of data within a function.
data DataOrigin
    = Local
    | Argument Int
    deriving (Eq, Ord, Read, Show)

-- | A global identifier is just a number, unique within a package, along with
-- a type and information about its origin.
data GlobalId ty orig
    = GlobalId
        { gidNum :: !Int
        , gidTy :: !ty
        , gidOrigName :: !orig
        , gidOrigin :: !DataOrigin
        }
    deriving (Eq, Ord, Read, Show)
