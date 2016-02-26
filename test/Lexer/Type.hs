{-# LANGUAGE OverloadedStrings #-}

module Lexer.Type (
  testType
) where

import Core

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
        parseOnly type' "struct { in struct { inn struct {};};}" `shouldBe`
            Right (structType [ (["in"],
                    (structType [ (["inn"],
                        structType [])]))])

    it "does not parse structs with invalid fields" $ do
        -- Missing identifier
        parseOnly type' "struct { []int32; }" `shouldSatisfy` isLeft
        -- Missing type
        parseOnly type' "struct { foo; }" `shouldSatisfy` isLeft
        parseOnly type' "struct { foo, bar; }" `shouldSatisfy` isLeft
        -- Invalid type
        parseOnly type' "struct { foo [-1]int32; }" `shouldSatisfy` isLeft
        -- Identifier with semi
        parseOnly type' "struct { foo; int32; }" `shouldSatisfy` isLeft
        parseOnly type' "struct { foo\n int32; }" `shouldSatisfy` isLeft
        -- Spurious comma
        parseOnly type' "struct { foo, bar, []int32; }" `shouldSatisfy` isLeft
        -- Missing comma
        parseOnly type' "struct { foo bar []int32; }" `shouldSatisfy` isLeft
        parseOnly type' "struct { foo bar, []int32; }" `shouldSatisfy` isLeft

    it "does not parse invalid structs" $ do
        -- Missing struct
        parseOnly type' "{ foo []int32; }" `shouldSatisfy` isLeft
        -- Missing braces
        parseOnly type' "struct {" `shouldSatisfy` isLeft
        parseOnly type' "struct }" `shouldSatisfy` isLeft
        -- Spurious semi
        parseOnly type' "struct; {}" `shouldSatisfy` isLeft
        -- Missing body
        parseOnly type' "struct" `shouldSatisfy` isLeft

    prop "parses any kind of valid type" $ do
        forAll typeGen $
            \t -> parseOnly type' (renderGoLite $ pretty t) `shouldBe` Right t
