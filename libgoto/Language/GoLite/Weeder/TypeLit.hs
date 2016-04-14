{-|
Module      : Language.GoLite.Weeder.TypeLit
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

"Weeder" traversal definition for types.
-}

{-# LANGUAGE TypeFamilies #-}

module Language.GoLite.Weeder.TypeLit
( weedType
, weedFields
) where

import Language.Common.Misc
import Language.GoLite.Weeder.Core

{- | Weeds a type.

    * Field declarations in structs must be unique (except for the blank
      identifier)
    * A named type may not be the blank identifier.
-}
weedType :: SrcAnnType -> Weeder ()
weedType (Fix (Ann _ (NamedType i))) =
        errorOnBlankIdentifier i "Cannot name a type with the blank identifier"
weedType (Fix (Ann _ (StructType fields))) = weedFields fields
weedType (Fix (Ann _ (ArrayType _ ty))) = weedType ty
weedType (Fix (Ann _ (SliceType ty))) = weedType ty


-- | Weeds a field. Checks that the field names are unique.
weedFields :: [(SrcAnnIdent, SrcAnnType)] -> Weeder ()
weedFields fs = do
    forM_ fs (\f -> weedType $ snd f)
    let dupes = filter notBlank $ bun' containsId (map fst fs)
    forM_ dupes (\(Ann a (Ident n)) ->
        reportError $ WeederException a ("duplicate name " ++ n))

    where
        notBlank (Ann _ (Ident n)) = n /= "_"
        containsId x@(Ann _ (Ident n)) ((Ann _ (Ident n')):ids) =
            if n == n' then True else containsId x ids
        containsId _ [] = False
