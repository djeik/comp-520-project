{-|
Module      : Language.GoLite.Annotation
Description : Generalized functor annotating framework
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

This module defines a generalized functor annotating framework and some
combinators for working with annotations.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Language.GoLite.Annotation where

import Data.Functor.Foldable
import Prelude as P

import Language.GoLite.Pretty
import Language.GoLite.Syntax.Precedence

-- | A functor value with an annotation.
--
-- Annotated functors are themselves functors whose implementation of @fmap@
-- simply dispatches to the inner functor's @fmap@.
data Ann x f a
    = Ann
        { ann :: !x
        -- ^ Extract only the annotation, discarding the data.
        , bare :: f a
        -- ^ Extract only the data, discarding the annotation.
        }
    deriving
        ( Eq
        , Functor
        , Ord
        , P.Foldable
        , Read
        , Show
        , Traversable
        )

annNat :: (forall b. f b -> g b) -> Ann x f a -> Ann x g a
annNat p a = case a of
    Ann x f -> Ann x (p f)

-- | The fixed point of an annotated functor is a syntax tree annotated along
-- its spine.
type AnnFix x f = Fix (Ann x f)

-- | Extract the base functor of annotated data.
type instance Base (Ann x f a) = f

-- | Annotated functors can be pretty-printed by stripping the annotations and
-- pretty-printing the inner syntax tree.
instance (Functor f, Pretty (Fix f)) => Pretty (Fix (Ann x f)) where
    pretty = pretty . bareF

-- | Annotated data can be pretty-printed by stripping the annotation and
-- pretty-printing the inner data.
instance Pretty (f a) => Pretty (Ann x f a) where
    pretty = pretty . bare

-- | Annotated operators with associated precedences have the same precedence
-- as the inner operator.
instance HasPrecedence (f a) => HasPrecedence (Ann x f a) where
    precedence = precedence . bare

-- | Pushes an annotation deeper into layered functors.
annPush :: Functor f => Ann x f (g a) -> f (Ann x g a)
annPush (Ann x fga) = fmap (Ann x) fga

-- | Duplicates a layer of annotation.
dupAnn :: Ann x f a -> Ann x (Ann x f) a
dupAnn f@(Ann x _) = Ann x f

-- | Extracts the annotation from the root of an annotated tree.
topAnn :: AnnFix x f -> x
topAnn (Fix (Ann x _)) = x

-- | Strip all annotations from a syntax tree.
--
-- /Warning:/ this strips annotations only from the *primary* recursion in the
-- tree! If the datatype you want to strip has many other annotated types
-- nested in it (e.g. 'Language.GoLite.Syntax.Types.StatementF'), then you will
-- need specialized stripping functions.
bareF :: Functor f => Fix (Ann x f) -> Fix f
bareF = cata (Fix . bare)
