{-|
Module      : Language.Common.Storage
Description : Storage size definition
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

module Language.Common.Storage where

-- | Types having an associated storage requirement as a number of bytes needed
-- to represent the data.
class StorageSize a where
    -- | Compute the number of bytes needed to store a value of this type.
    storageSize :: a -> Int