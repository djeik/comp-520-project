module Core
( isRight
, isLeft
, parseOnly
, module Control.Monad
, module Test.Hspec
, module Test.Hspec.QuickCheck
, module Test.QuickCheck
, module Gen.Literal
, module Gen.Expr
) where

import Control.Monad
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Text.Megaparsec
import Text.Megaparsec.String

import Gen.Literal
import Gen.Expr

-- | Checks whether an instance of `Either` is `Right` or not
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- | Checks whether an instance of `Either` is `Left` or not
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | Runs an abitrary parser followed immediately by the end-of-file parser.
parseOnly :: Parser a -> String -> Either ParseError a
parseOnly m = parse (m <* eof) "test"
