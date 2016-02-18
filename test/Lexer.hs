{-# LANGUAGE OverloadedStrings #-}

module Lexer
(
  lexer
) where

import qualified Text.Megaparsec.Lexer as L (charLiteral)

import Language.GoLite
import Language.GoLite.Annotation ( bare )
import TestUtil

lexer :: SpecWith ()
lexer = describe "Lexer" $ do
    testDecimalLiteral
    testOctalLiteral
    testHexLiteral
    testIntLiteral
    testFloatLiteral
    testEscapeCode
    testRuneLiteral
    testRawString
    testInterpretedString
    testIdentifier

testDecimalLiteral :: SpecWith ()
testDecimalLiteral = describe "decimalLiteral" $ do
    it "parses a decimal integer literal" $ do
        parseOnly decimalLiteral "12" `shouldBe` Right 12

    it "cannot parse a literal starting with 0" $ do
        parseOnly decimalLiteral "012" `shouldSatisfy` isLeft
        parseOnly decimalLiteral "0" `shouldSatisfy` isLeft

    it "cannot parse a number containing spaces" $ do
        parseOnly decimalLiteral "12 34" `shouldSatisfy` isLeft

testOctalLiteral :: SpecWith ()
testOctalLiteral = describe "octalLiteral" $ do
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

    it "cannot parse `0` followed by a space then octal digits" $ do
        parseOnly octalLiteral "0 1" `shouldSatisfy` isLeft

testHexLiteral :: SpecWith ()
testHexLiteral = describe "hexLiteral" $ do
    prop "parses hexadecimal integer literals" $ do
        forAll (resize 8 hexGenLower) $ \s ->
            parseOnly hexLiteral s == Right (read s :: Int)

    prop "doesn't care about letter-digit case" $ do
        forAll (resize 8 hexGenMixedCase) $ \s ->
            parseOnly hexLiteral s == Right (read s :: Int)

    it "cannot parse just the string `0x` or `0X`" $ do
        parseOnly hexLiteral "0x" `shouldSatisfy` isLeft
        parseOnly hexLiteral "0X" `shouldSatisfy` isLeft

    it "cannot parse `0x` followed by a space and a number`" $ do
        parseOnly hexLiteral "0x 12" `shouldSatisfy` isLeft
        parseOnly hexLiteral "0X 12" `shouldSatisfy` isLeft

    prop "cannot parse numbers without the hex prefix" $ do
        forAll (resize 8 intGen) $ \s ->
            isLeft $ parseOnly hexLiteral s

testIntLiteral :: SpecWith ()
testIntLiteral = describe "integerLiteral" $ do
    it "parses all kinds of integer literals (decimal, octal, hex)" $ do
        parseOnly decimalLiteral "123" `shouldBe` Right 123
        parseOnly octalLiteral "012" `shouldBe` Right 0o12
        parseOnly hexLiteral "0x8F" `shouldBe` Right  0x8F

testFloatLiteral :: SpecWith ()
testFloatLiteral = describe "floatLiteral" $ do
    it "parses `0.0`, `0.` and `.0`" $ do
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

    it "cannot parse the string `.`" $ do
        parseOnly floatLiteral "." `shouldSatisfy` isLeft

    it "cannot parse string with spaces between the dot and numbers" $ do
        parseOnly floatLiteral "1. 1" `shouldSatisfy` isLeft
        parseOnly floatLiteral "1 .1" `shouldSatisfy` isLeft
        parseOnly floatLiteral "1 . 1" `shouldSatisfy` isLeft

testEscapeCode :: SpecWith ()
testEscapeCode = describe "escapeCode" $ do
    it "parses legal escape codes (e.g. `\\b`)" $ do
        forAll (elements commonEscapes) $ \c ->
            -- Use charLiteral as our reference
            parseOnly (escapeCode commonEscapes) ('\\':[c])
            ==  parseOnly L.charLiteral ('\\':[c])

    it "cannot parse illegal escape codes (e.g. `\\x`)" $ do
        parseOnly (escapeCode commonEscapes) "\\x" `shouldSatisfy` isLeft

    it "cannot parse a character that is not an escape code (e.g. `a`)" $ do
        -- This test doesn't depend on which escapes are legal, hence []
        parseOnly (escapeCode []) "a" `shouldSatisfy` isLeft

    it "cannot parse just a backslash (i.e. `\\`)" $ do
        parseOnly (escapeCode []) "\\" `shouldSatisfy` isLeft

testRuneLiteral :: SpecWith ()
testRuneLiteral = describe "runeLiteral" $ do
    it "parses a single character or escape code in single quotes" $ do
        parseOnly runeLiteral "'a'" `shouldBe` Right 'a'
        parseOnly runeLiteral "'\\a'" `shouldBe` Right '\a'

    it "cannot parse an empty pair of single quotes" $ do
        parseOnly runeLiteral "''" `shouldSatisfy` isLeft

    it "cannot parse a rune containing a newline, `\\\"`, or `'`" $ do
        parseOnly runeLiteral "'\\\"'" `shouldSatisfy` isLeft
        parseOnly runeLiteral "'\n'" `shouldSatisfy` isLeft
        parseOnly runeLiteral "'''" `shouldSatisfy` isLeft

    it "cannot parse a rune containing more than one character" $ do
        parseOnly runeLiteral "'ab'" `shouldSatisfy` isLeft
        parseOnly runeLiteral "'a '" `shouldSatisfy` isLeft
        parseOnly runeLiteral "'  '" `shouldSatisfy` isLeft

testRawString :: SpecWith ()
testRawString = describe "rawString" $ do
    it "parses a string of characters enclosed in backticks (`)" $ do
        parseOnly rawStringLiteral "`abcd`" `shouldBe` Right "abcd"
        parseOnly rawStringLiteral "``" `shouldBe` Right ""

    it "does not perform escape code interpretation" $ do
        parseOnly rawStringLiteral "`a\\tb`" `shouldBe` Right "a\\tb"

    it "parses a literal containing newlines" $ do
        parseOnly rawStringLiteral "`a\nb`" `shouldBe` Right "a\nb"

    it "gobbles carriage return characters (`\\r`)" $ do
        parseOnly rawStringLiteral "`ab\rcd`" `shouldBe` Right "abcd"

    it "cannot parse a raw string literal containing a backtick" $ do
        parseOnly rawStringLiteral "`ab`cd`" `shouldSatisfy` isLeft

testInterpretedString :: SpecWith ()
testInterpretedString = describe "interpretedString" $ do
    it "parses a string of characters in double-quotes" $ do
        parseOnly interpStringLiteral "\"abc\"" `shouldBe` Right "abc"
        parseOnly interpStringLiteral "\"\"" `shouldBe` Right ""

    it "performs escape code interpretation" $ do
        parseOnly interpStringLiteral "\"\\t\"" `shouldBe` Right "\t"

    it "cannot parse a literal containing newlines, `\\\'` or `\"`" $ do
        parseOnly interpStringLiteral "\"\a\n\"" `shouldSatisfy` isLeft
        parseOnly interpStringLiteral "\"\\\'\"" `shouldSatisfy` isLeft
        parseOnly interpStringLiteral "\"\"\"" `shouldSatisfy` isLeft

testIdentifier :: SpecWith ()
testIdentifier = describe "identifier" $ do
    let identifier' = fmap bare $ identifier >>= unSemiP

    it "parses an alphanumeric string starting with a letter" $ do
        parseOnly (identifier') "abc12" `shouldBe` Right "abc12"
        parseOnly (identifier') "a" `shouldBe` Right "a"

    it "parses a string containing or starting with underscores" $ do
        parseOnly (identifier') "a_b" `shouldBe` Right "a_b"
        parseOnly (identifier') "_ab" `shouldBe` Right "_ab"
        parseOnly (identifier') "___" `shouldBe` Right "___"

    it "does not parse a string starting with a number" $ do
        parseOnly (identifier') "0_or_1" `shouldSatisfy` isLeft
        parseOnly (identifier') "0xAF" `shouldSatisfy` isLeft
