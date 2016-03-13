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
weedType (Fix (Ann _ (StructType fields))) = do
    let s = S.empty
    forM_ fields (\f -> do
        weedType (snd f)
        forM_ (fst f)
            (\(Ann a (Ident n)) -> do
                if n /= "_" && n `S.member` s then
                    reportError (a, "duplicate struct field " ++ n)
                else
                    void $ pure $ S.insert n s))