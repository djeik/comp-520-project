{-|
Module      : Language.GoLite.Misc
Description : Stuff that fits nowhere else.
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Defines miscellaneous useful functions. We won't kid ourselves by calling this
module \"Util\".
-}

{-# LANGUAGE TupleSections #-}

module Language.GoLite.Misc
( ($>)
, safeZip
, distTuple
, unFix
  -- * Convenience re-exports
, isJust
, isNothing
) where

import Data.Functor.Foldable ( Fix(..) )
import Data.Maybe ( isJust, isNothing )

-- | The \"fmap const\" operator replaces the contents of a "Functor" with a
-- given value.
($>) :: Functor f => f a -> b -> f b
f $> c = fmap (const c) f

-- | Zips two lists and returns the part that would have been truncated.
safeZip :: [a] -> [b] -> ([(a, b)], Maybe (Either [a] [b]))
safeZip [] ys = ([], Just $ Right ys)
safeZip xs [] = ([], Just $ Left xs)
safeZip (x:xs) (y:ys) = ((x, y) : xys, z) where
    (xys, z) = safeZip xs ys

distTuple :: [a] -> b -> [(a, b)]
distTuple = flip $ \x -> map (, x)

unFix :: Fix f -> f (Fix f)
unFix (Fix x) = x
