{-# LANGUAGE OverloadedStrings #-}

module Stmt
(
  statement
) where

import Language.GoLite
import Language.GoLite.SrcAnn
import Language.GoLite.Parser.SimpleStmts
import Language.GoLite.Syntax.Sugar
import Core

statement :: SpecWith ()
statement = describe "stmt" $ do
                describe "assignStmt" $ assign

r = Right

assign :: SpecWith ()
assign = do
    let parseAssign = parseOnly (fmap bareStmt $ assignStmt >>= unSemiP)
    let parseStmt = parseOnly (fmap bareStmt $ fmap head stmt)
    let assign e o e' = Fix $ Assignment e o e'
    let var = Fix . Variable
    let int = Fix. Literal . IntLit

    it "parses increments and decrements" $ do
        parseAssign "x++;" `shouldBe` r (assign [var "x"] PlusEq [int 1])
        parseAssign "x--;" `shouldBe` r (assign [var "x"] MinusEq [int 1])

        parseAssign "x ++" `shouldSatisfy` isRight

    it "parses e +=/-= 1 equivalently to e++/--" $ do
        parseAssign "x++;" `shouldBe` parseAssign "x += 1;"
        parseAssign "x--" `shouldBe` parseAssign "x -= 1;"

    it "does not parse mangled/missing increment or decrement operators" $ do
        parseAssign "x+ +" `shouldSatisfy` isLeft
        parseAssign "x+" `shouldSatisfy` isLeft
        parseAssign "x- -" `shouldSatisfy` isLeft
        parseAssign "x-" `shouldSatisfy` isLeft
        parseAssign "x" `shouldSatisfy` isLeft

    it "does not parse increment/decrements missing an expression" $ do
        parseAssign "++" `shouldSatisfy` isLeft
        parseAssign "--" `shouldSatisfy` isLeft

    it "does not parse an increment/decrement with a semi expression" $ do
        parseAssign "x\n++" `shouldSatisfy` isLeft
        parseAssign "x;++" `shouldSatisfy` isLeft

    it "parses a single or a multiple assignment" $ do
        parseAssign "a = a" `shouldBe` r (assign [var "a"] Assign [var "a"])
        parseAssign "a, b = b, a" `shouldBe`
            r (assign [var "a", var "b"] Assign [var "b", var "a"])

    it "parses an assignment op" $ do
        parseAssign "a += a" `shouldBe` r (assign [var "a"] PlusEq [var "a"])

    it "does not parse an assignment with no expression on either side" $ do
        parseAssign "= a" `shouldSatisfy` isLeft
        parseAssign "= a, b" `shouldSatisfy` isLeft
        parseAssign "+= a" `shouldSatisfy` isLeft
        parseAssign "+= a, b" `shouldSatisfy` isLeft
        parseAssign "a =" `shouldSatisfy` isLeft
        parseAssign "a, b =" `shouldSatisfy` isLeft
        parseAssign "a +=" `shouldSatisfy` isLeft
        parseAssign "a, b +=" `shouldSatisfy` isLeft

    it "does not parse an assignment with a missing or non-assign operator" $ do
        parseAssign "a a" `shouldSatisfy` isLeft
        parseAssign "a << a" `shouldSatisfy` isLeft

    it "does not parse when any expression but the rightmost has a semi" $ do
        parseAssign "a; = a" `shouldSatisfy` isLeft
        parseAssign "a\n = a" `shouldSatisfy` isLeft
        parseAssign "a;, b = a" `shouldSatisfy` isLeft
        parseAssign "a\n, b = a" `shouldSatisfy` isLeft
        parseAssign "a = a;, b" `shouldSatisfy` isLeft
        parseAssign "a += a;, b" `shouldSatisfy` isLeft

    it "does not parse when the rightmost expression does not have a semi" $ do
        parseAssign "a = a {}" `shouldSatisfy` isLeft
        parseAssign "a += a {}" `shouldSatisfy` isLeft