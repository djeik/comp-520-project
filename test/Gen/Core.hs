module Gen.Core
( surroundWith
, smallArbitrary
, maybeGen
, genNothing
, module Language.GoLite.Syntax
, module Control.Monad
, module Test.Hspec
, module Test.Hspec.QuickCheck
, module Test.QuickCheck
) where

import Language.GoLite.Syntax

import Control.Monad
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ( Positive ) -- Conflicts with Types.Positive
import Test.QuickCheck.Gen ( Gen(MkGen) )

-- | Creates a function which will surround a string with the given string.
surroundWith :: String -> (String -> String)
surroundWith s = (\x -> s ++ x ++ s)

-- | Generates a size-1 arbitrary value.
smallArbitrary :: Arbitrary a => Gen a
smallArbitrary = (resize 1 arbitrary)

-- | Generates either just a value from the given generator, or nothing.
-- The body is the same as the instance for Arbitrary (Maybe a)
-- (https://goo.gl/zimSLs), -- but available as a standalone function.
maybeGen :: Gen a -> Gen (Maybe a)
maybeGen g = frequency [(1, pure Nothing), (3, liftM Just g)]

-- | Generates Nothing.
genNothing :: Gen (Maybe a)
genNothing = (MkGen $ \_ _ -> Nothing)