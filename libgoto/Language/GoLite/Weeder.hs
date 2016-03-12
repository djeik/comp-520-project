{-|
Module      : Language.GoLite.Weeder
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

This module contains the Weeder traversal, which throws errors on semantically
invalid programs.
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.GoLite.Weeder where

import Control.Monad (void)

import Language.GoLite.Monad.Traverse
import Language.GoLite.Syntax
import Language.GoLite.Syntax.SrcAnn

-- TODO
type WeederException = (SrcSpan, String)

-- TODO
type WeederState = String

type Weeder = Traversal WeederException WeederState

instance MonadTraversal Weeder where
    type TraversalError Weeder = WeederException

    reportError = undefined -- TODO add to list of errors in state

    getErrors = undefined -- TODO extract list of errors from state

    abortTraversal = throwError =<< getErrors


weedPackage :: SrcAnnPackage -> Weeder ()
weedPackage (Package (Ann a (Ident n)) tlds) = do
    case n of
        "_" -> reportError (a, "Package name may not be the blank identifier.")
        _ -> pure ()
    pure $ (void . map) weedTopLevelDecl tlds

weedTopLevelDecl :: SrcAnnTopLevelDecl -> Weeder ()
weedTopLevelDecl = undefined