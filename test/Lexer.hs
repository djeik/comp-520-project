{-# LANGUAGE OverloadedStrings #-}

module Lexer
(
  lexer
) where

import qualified Text.Megaparsec.Lexer as L (charLiteral)

import Language.GoLite hiding (structType) -- conflicts with sugar structType
import Language.GoLite.Syntax.Sugar
import Language.GoLite.Annotation ( bare )
import Language.GoLite.SrcAnn ( bareType )
import Text.PrettyPrint ( render )
import Language.GoLite.Pretty
import Core

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
    testType

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

    prop "parses arbitrary octal literals" $ do
        forAll octalGen $ \s -> isRight $ parseOnly octalLiteral s

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
        forAll (resize 8 decimalGen) $ \s ->
            isLeft $ parseOnly hexLiteral s

testIntLiteral :: SpecWith ()
testIntLiteral = describe "integerLiteral" $ do
    it "parses all kinds of integer literals (decimal, octal, hex)" $ do
        forAll (resize 8 (oneof [decimalGen, octalGen, hexGenLower])) $ \ s ->
                isRight $ parseOnly integerLiteral s

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
        forAll (resize 8 decimalGen) $ \s ->
            parseOnly floatLiteral (s ++ ".") == Right (read s :: Double)

    prop "parses float literals with only a decimal part" $ do
        forAll (resize 8 decimalGen) $ \s ->
            parseOnly floatLiteral ('.':s) == Right (read ("0."++s)::Double)

    prop "cannot parse numbers without a decimal point" $ do
        forAll (resize 8 decimalGen) $ \s ->
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

    it "has the proper parse result" $ do
        parseOnly runeLiteral "'a'" `shouldBe` Right 'a'
        parseOnly runeLiteral "'\\a'" `shouldBe` Right '\a'

    prop "parses arbitrary valid rune literals" $ do
        forAll runeGen $ \s -> isRight (parseOnly runeLiteral s)

    it "cannot parse an empty pair of single quotes" $ do
        parseOnly runeLiteral "''" `shouldSatisfy` isLeft

    it "cannot parse a rune containing a newline, `\\\"`, `\\` or `'`" $ do
        parseOnly runeLiteral "'\\\"'" `shouldSatisfy` isLeft
        parseOnly runeLiteral "'\\'" `shouldSatisfy` isLeft
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

    prop "parses arbitrary valid raw string literals" $ do
        forAll rawStringGen $ \s -> isRight (parseOnly rawStringLiteral s)

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

    prop "parses arbitrary valid interpreted string literals" $ do
        forAll interpStringGen $ \s -> isRight (parseOnly interpStringLiteral s)

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

    prop "parses arbitrary identifiers" $ do
        forAll identGen $ \s -> isRight (parseOnly identifier' s)

    it "does not parse a string starting with a number" $ do
        parseOnly (identifier') "0_or_1" `shouldSatisfy` isLeft
        parseOnly (identifier') "0xAF" `shouldSatisfy` isLeft


testType :: SpecWith ()
testType = describe "type_" $ do
    let type' = fmap bareType $ type_ >>= unSemiP
    let isNotSlice x = case x of
                        Right (Fix (SliceType _)) -> False
                        _ -> True
    let isNotArray x = case x of
                        Right (Fix (ArrayType _ _)) -> False
                        _ -> True
    let idf = Identity

    it "parses slice types containing arbitrary types" $ do
        parseOnly type' "[]a" `shouldBe` Right (sliceType (namedType "a"))
        parseOnly type' "[][]a" `shouldBe`
                    Right (sliceType $ sliceType $ namedType "a")

        parseOnly type' "[][1]a" `shouldBe`
                    Right (sliceType $ arrayType (idf 1) (namedType "a"))

        parseOnly type' "[]struct{}" `shouldBe`
                    Right (sliceType $ structType [])

    it "does not parse invalid slice types" $ do
        -- Invalid brackets
        parseOnly type' "a" `shouldSatisfy` isNotSlice
        parseOnly type' "[a" `shouldSatisfy` isLeft
        parseOnly type' "]a" `shouldSatisfy` isLeft
        -- Improper semi
        parseOnly type' "[];a" `shouldSatisfy` isLeft
        parseOnly type' "[]\na" `shouldSatisfy` isLeft
        -- No inner type
        parseOnly type' "[]" `shouldSatisfy` isLeft
        -- Improper inner type
        parseOnly type' "[]struct{" `shouldSatisfy` isLeft

    it "parses slice types with superfluous whitespace" $ do
        parseOnly type' "[]  \t  a" `shouldBe` Right (sliceType (namedType "a"))
        parseOnly type' "[  ]a" `shouldBe` Right (sliceType (namedType "a"))
        parseOnly type' "[\t]\ta" `shouldBe` Right (sliceType (namedType "a"))

    it "parses array types containing arbitrary types" $ do
        parseOnly type' "[1]a" `shouldBe`
            Right (arrayType (idf 1) (namedType "a"))

        parseOnly type' "[1][]a" `shouldBe`
            Right (arrayType (idf 1) (sliceType $ namedType "a"))

        parseOnly type' "[1][1]a" `shouldBe`
            Right (arrayType (idf 1) (arrayType (idf 1) (namedType "a")))

        parseOnly type' "[1]struct{}" `shouldBe`
            Right (arrayType (idf 1) (structType []))

    it "parses 0-sized array types" $
        parseOnly type' "[0]a" `shouldBe`
            Right (arrayType (idf 0) (namedType "a"))

    it "parses array types with superfluous whitespace" $ do
        parseOnly type' "[ 0 ] a" `shouldBe`
            Right (arrayType (idf 0) (namedType "a"))

        parseOnly type' "[ 0] a" `shouldBe`
            Right (arrayType (idf 0) (namedType "a"))

        parseOnly type' "[0 ]a" `shouldBe`
            Right (arrayType (idf 0) (namedType "a"))

    it "does not parse invalid array types" $ do
        -- No brackets
        parseOnly type' "a" `shouldSatisfy` isNotArray
        parseOnly type' "1 a" `shouldSatisfy` isLeft
        -- Improper semi
        parseOnly type' "[1];a" `shouldSatisfy` isLeft
        parseOnly type' "[1]\na" `shouldSatisfy` isLeft
        parseOnly type' "[1;]a" `shouldSatisfy` isLeft
        parseOnly type' "[1\n]a" `shouldSatisfy` isLeft
        -- Missing dimension
        parseOnly type' "[]a" `shouldSatisfy` isNotArray
        -- Negative dimension
        parseOnly type' "[-1]a" `shouldSatisfy` isLeft
        -- Float dimension
        parseOnly type' "[1.5]a" `shouldSatisfy` isLeft
        -- No inner type
        parseOnly type' "[1]" `shouldSatisfy` isLeft
        -- Improper inner type
        parseOnly type' "[]struct{" `shouldSatisfy` isLeft

    it "parses named types" $ do
        parseOnly type' "typ" `shouldBe` Right (namedType "typ")
        parseOnly type' "_" `shouldBe` Right (namedType "_")

    it "parses struct types" $ do
        -- No fields
        parseOnly type' "struct {}" `shouldBe` Right (structType [])
        -- This is fine, the struct keyword doesn't trigger semicolon insertion
        parseOnly type' "struct\n{}" `shouldSatisfy` isRight
        -- One field, one type
        parseOnly type' "struct { foo int32; }" `shouldBe`
            Right (structType [(["foo"], namedType "int32")])
        -- Multiple fields, one type
        parseOnly type' "struct { foo, bar int32; }" `shouldBe`
            Right (structType [(["foo", "bar"], namedType "int32")])
        -- Same, with some whitespace before the comma
        parseOnly type' "struct { foo , bar int32; }" `shouldBe`
            Right (structType [(["foo", "bar"], namedType "int32")])
        parseOnly type' "struct { foo,\nbar int32; }" `shouldBe`
            Right (structType [(["foo", "bar"], namedType "int32")])
        -- One field, multiple types
        parseOnly type' "struct { foo int32; bar int64; }" `shouldBe`
            Right (structType [ (["foo"], namedType "int32"),
                                (["bar"], namedType "int64")])
        -- Multiple fields, multiple types
        parseOnly type' "struct { foo, bar int32; baz, quux int64; }" `shouldBe`
            Right (structType [ (["foo", "bar"], namedType "int32"),
                                (["baz", "quux"], namedType "int64")])
        -- Fields with various inner types
        parseOnly type' "struct { foo []ty; bar [4]ki; }" `shouldBe`
            Right (structType [ (["foo"], sliceType $ namedType "ty"),
                        (["bar"], arrayType (idf 4) (namedType "ki"))])
        -- Nested structs
        parseOnly type' "struct { in struct { inn struct {}}}" `shouldBe`
            Right (structType [ (["in"],
                    (structType [ (["inn"],
                        structType [])]))])

    it "does not parse structs with invalid fields" $ do
        -- Missing identifier
        parseOnly type' "struct { []int32 }" `shouldSatisfy` isLeft
        -- Missing type
        parseOnly type' "struct { foo }" `shouldSatisfy` isLeft
        parseOnly type' "struct { foo, bar }" `shouldSatisfy` isLeft
        -- Invalid type
        parseOnly type' "struct { foo [-1]int32 }" `shouldSatisfy` isLeft
        -- Identifier with semi
        parseOnly type' "struct { foo; int32 }" `shouldSatisfy` isLeft
        parseOnly type' "struct { foo\n int32 }" `shouldSatisfy` isLeft
        -- Spurious comma
        parseOnly type' "struct { foo, bar, []int32 }" `shouldSatisfy` isLeft
        -- Missing comma
        parseOnly type' "struct { foo bar []int32 }" `shouldSatisfy` isLeft
        parseOnly type' "struct { foo bar, []int32 }" `shouldSatisfy` isLeft

    it "does not parse invalid structs" $ do
        -- Missing struct
        parseOnly type' "{ foo []int32 }" `shouldSatisfy` isLeft
        -- Missing braces
        parseOnly type' "struct {" `shouldSatisfy` isLeft
        parseOnly type' "struct }" `shouldSatisfy` isLeft
        -- Spurious semi
        parseOnly type' "struct; {}" `shouldSatisfy` isLeft
        -- Missing body
        parseOnly type' "struct" `shouldSatisfy` isLeft

    prop "parses any kind of valid type" $ do
        forAll typeGen $
            \t -> parseOnly type' (render $ pretty t) `shouldBe` Right t
