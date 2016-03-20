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

import Data.Functor.Foldable ( cata )
import qualified Data.Set as S

import Language.GoLite.Weeder.Core

{- | Weeds a type.

    * Field declarations in structs must be unique (except for the blank
      identifier)
    * A named type may not be the blank identifier.
-}
weedType :: SrcAnnType -> Weeder ()
weedType = cata phi where

    phi (Ann _ (NamedType i)) =
        errorOnBlankIdentifier i "Cannot name a type with the blank identifier"

    phi (Ann _ (StructType fields)) = weedFields fields

    phi _ = pure ()

-- | Weeds a field. Checks that the field names are unique.
weedFields :: [(SrcAnnIdent, b)] -> Weeder ()
weedFields fs = do
    -- This is pretty ugly, but lets us pinpoint exactly where the duplicate
    -- identifiers are.
    let s = S.empty
    forM_ fs (\f ->
        let (Ann a (Ident n)) = fst f in
        if n /= "_" && n `S.member` s then
            reportError $ WeederException a ("duplicate name " ++ n)
        else
            void $ pure $ S.insert n s)