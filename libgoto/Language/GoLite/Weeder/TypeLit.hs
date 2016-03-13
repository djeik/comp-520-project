{-|
Module      : Language.GoLite.Weeder.TypeLit
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

"Weeder" traversal definition for types.
-}

module Language.GoLite.Weeder.TypeLit
( weedType
, weedFields
) where

import Language.GoLite.Weeder.Core
import qualified Data.Set as S

{- | Weeds a type.

    * Field declarations in structs must be unique (except for the blank
      identifier)
-}
weedType :: SrcAnnType -> Weeder ()

-- Slice type: weed the inner type.
weedType (Fix (Ann _ (SliceType t))) = weedType t
-- Array type: weed the inner type.
weedType (Fix (Ann _ (ArrayType _ t))) = weedType t
-- Named type: nothing.
weedType (Fix (Ann _ (NamedType _))) = pure ()
-- Struct type: check that identifiers are unique, and weed inner types.
weedType (Fix (Ann _ (StructType fields))) = weedFields fields

-- | Weeds a field. Checks that the field names are unique, and weeds the types.
weedFields :: [(SrcAnnIdent, SrcAnnType)] -> Weeder ()
weedFields fs = do
    -- This is pretty ugly, but lets us pinpoint exactly where the duplicate
    -- identifiers are.
    let s = S.empty
    forM_ fs (\f -> do
        weedType $ snd f
        let (Ann a (Ident n)) = fst f
        if n /= "_" && n `S.member` s then
            reportError (a, "duplicate name " ++ n)
        else
            void $ pure $ S.insert n s)