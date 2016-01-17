module Main where

import Language.GoLite

import Control.Monad
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.String

main :: IO ()
main = hspec goLite

goLite :: SpecWith ()
goLite = describe "Language.GoLite" $ do
    lexer

lexer :: SpecWith ()
lexer = describe "Lexer" $ do
    describe "octalLiteral" $ do
        it "parses an octal integer literal" $ do
            parseOnly octalLiteral "054321" `shouldBe` Right 0o54321

        prop "parses an arbitrary number of zeroes" $ do
            forAll (choose (1, 100)) $ \n ->
               isRight . parseOnly octalLiteral . replicate n $ '0'

        prop "cannot parse strings beginning with a nonzero digit" $ do
            forAll (choose ('1', '7'))
                   (isLeft . parseOnly octalLiteral . (:"53421"))

    describe "hexLiteral" $ do
        prop "parses hexadecimal integer literals" $ do
            forAll (resize 8 hexGenLower) $ \s ->
                parseOnly hexLiteral s == Right (read s :: Int)

        prop "doesn't care about letter-digit case"$ do
            forAll (resize 8 hexGenMixedCase) $ \s ->
                parseOnly hexLiteral s == Right (read s :: Int)

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

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

parseOnly :: Parser a -> String -> Either ParseError a
parseOnly m = parse (m <* eof) "test"
