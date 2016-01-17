module Main where

import Language.GoLite

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Megaparsec

main = hspec goLite

goLite = describe "Language.GoLite" $ do
    lexer

lexer = describe "Lexer" $ do
    describe "octal_lit" $ do
        it "parses an octal integer literal" $ do
            parseOnly octal_lit "054321" `shouldBe` Right 0o54321

        it "can parse an arbitrary number of zeroes" $ do
            parseOnly octal_lit "00000" `shouldBe` Right 0

        prop "cannot begin with a nonzero digit" $ do
            forAll (choose ('1', '7')) (isLeft . parseOnly octal_lit . (:"53421"))

isRight (Right _) = True
isRight _ = False

isLeft (Left _) = True
isLeft _ = False

parseOnly m = parse (m <* eof) "test"
