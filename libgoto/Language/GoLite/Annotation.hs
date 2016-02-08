{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.GoLite.Annotation where

import Data.Functor.Foldable
import Prelude as P

import Language.GoLite.Pretty
import Language.GoLite.Precedence

-- | A functor value with an annotation.
data Ann x f a
    = Ann
        { ann :: !x
        -- ^ Extract only the annotation, discarding the data.
        , bare :: f a
        -- ^ Extract only the data, discarding the annotation.
        }
    deriving (Eq, Read, Show, Functor, P.Foldable, Traversable)

-- | Pushes an annotation deeper into layered functors.
annPush :: Functor f => Ann x f (g a) -> f (Ann x g a)
annPush (Ann x fga) = fmap (Ann x) fga

-- | Duplicates a layer of annotation.
dupAnn :: Ann x f a -> Ann x (Ann x f) a
dupAnn f@(Ann x _) = Ann x f

-- | Extracts the annotation from the root of an annotated tree.
topAnn :: AnnFix x f -> x
topAnn (Fix (Ann x _)) = x

type AnnFix x f = Fix (Ann x f)

-- | Extract the base functor of annotated data.
type instance Base (Ann x f a) = f

-- | Strip all annotations from a syntax tree.
bareF :: Functor f => Fix (Ann x f) -> Fix f
bareF = cata (Fix . bare)

instance (Functor f, Pretty (Fix f)) => Pretty (Fix (Ann x f)) where
    pretty = pretty . bareF

instance Pretty (f a) => Pretty (Ann x f a) where
    pretty = pretty . bare

instance (Functor f, HasPrecedence (f a)) => HasPrecedence (Ann x f a) where
    precedence = precedence . bare
