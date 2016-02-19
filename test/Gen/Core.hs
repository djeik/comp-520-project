module Gen.Core
( surroundWith
, module Control.Monad
, module Test.Hspec
, module Test.Hspec.QuickCheck
, module Test.QuickCheck
) where

import Control.Monad
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- | Creates a function which will surround a string with the given string.
surroundWith :: String -> (String -> String)
surroundWith s = (\x -> s ++ x ++ s)