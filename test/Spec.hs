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
    describe "decimalLiteral" $ do
        it "parses a decimal integer literal" $ do
            parseOnly decimalLiteral "12" `shouldBe` Right 12

        prop "cannot parse a literal starting with 0 of length > 1" $ do
            isLeft $ parseOnly decimalLiteral "01"

    describe "octalLiteral" $ do
        it "parses an octal integer literal" $ do
            parseOnly octalLiteral "054321" `shouldBe` Right 0o54321

        prop "parses an arbitrary number of zeroes" $ do
            forAll (choose (1, 100)) $ \n ->
               isRight . parseOnly octalLiteral . replicate n $ '0'

        prop "cannot parse strings beginning with a nonzero digit" $ do
            forAll (choose ('1', '7'))
               (isLeft . parseOnly octalLiteral . (:"53421"))

        it "cannot parse numbers containing non-octal digits" $ do
            parseOnly octalLiteral "08" `shouldSatisfy` isLeft
            parseOnly octalLiteral "09" `shouldSatisfy` isLeft

    describe "hexLiteral" $ do
        prop "parses hexadecimal integer literals" $ do
            forAll (resize 8 hexGenLower) $ \s ->
                parseOnly hexLiteral s == Right (read s :: Int)

        prop "doesn't care about letter-digit case" $ do
            forAll (resize 8 hexGenMixedCase) $ \s ->
                parseOnly hexLiteral s == Right (read s :: Int)

        it "cannot parse just the string \"0x\" or \"0X\"" $ do
            parseOnly hexLiteral "0x" `shouldSatisfy` isLeft
            parseOnly hexLiteral "0X" `shouldSatisfy` isLeft

        prop "cannot parse numbers without the hex prefix" $ do
            forAll (resize 8 intGen) $ \s ->
                isLeft $ parseOnly hexLiteral s

    describe "floatLiteral" $ do
        it "parses \"0.0\", \"0.\" and \".0\"" $ do
            parseOnly floatLiteral "0.0" `shouldBe` Right 0
            parseOnly floatLiteral "0." `shouldBe` Right 0
            parseOnly floatLiteral ".0" `shouldBe` Right 0

        prop "parses float literals with an integral and decimal part" $ do
            forAll (resize 8 floatGen) $ \s ->
                parseOnly floatLiteral s == Right (read s :: Double)

        prop "parses float literals with only an integral part" $ do
            forAll (resize 8 intGen) $ \s ->
                parseOnly floatLiteral (s ++ ".") == Right (read s :: Double)

        prop "parses float literals with only a decimal part" $ do
            forAll (resize 8 intGen) $ \s ->
                parseOnly floatLiteral ('.':s) == Right (read ("0."++s)::Double)

        prop "cannot parse numbers without a decimal point" $ do
            forAll (resize 8 intGen) $ \s ->
                isLeft $ parseOnly floatLiteral s

intGen :: Gen String
intGen = sized . flip replicateM . elements $ ['0'..'9']

floatGen :: Gen String
floatGen = (\x y -> x ++ "." ++ y) <$> intGen <*> intGen

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
