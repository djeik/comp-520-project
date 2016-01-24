module TestUtil
(
  isRight
, isLeft
, parseOnly
, hexGenLower
, hexGenMixedCase
, intGen
, floatGen
, module Control.Monad
, module Test.Hspec
, module Test.Hspec.QuickCheck
, module Test.QuickCheck
) where

import Control.Monad
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Text.Megaparsec
import Text.Megaparsec.String

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

parseOnly :: Parser a -> String -> Either ParseError a
parseOnly m = parse (m <* eof) "test"

hexGenLower :: Gen String
hexGenLower = ("0x" ++) <$> mkDigits where
    mkDigits
        = sized
        . flip replicateM
        . elements
        $ ['0'..'9'] ++ ['a'..'f']

hexGenMixedCase :: Gen String
hexGenMixedCase = (++) <$> prefixes <*> mkDigits where
    prefixes = elements ["0x", "0X"]
    mkDigits
        = sized
        . flip replicateM
        . elements
        $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

intGen :: Gen String
intGen = sized . flip replicateM . elements $ ['0'..'9']

floatGen :: Gen String
floatGen = (\x y -> x ++ "." ++ y) <$> intGen <*> intGen