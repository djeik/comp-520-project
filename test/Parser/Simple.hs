{-# LANGUAGE OverloadedStrings #-}

module Parser.Simple where

import Core
import Language.GoLite.Parser.SimpleStmts

assign :: SpecWith ()
assign = do
    let parseAssign = parseOnly (fmap bareStmt $ assignStmt >>= unSemiP)

    it "parses increments and decrements" $ do
        parseAssign "x++;" `shouldBe` r (assignment [variable "x"] PlusEq [int 1])
        parseAssign "x--;" `shouldBe` r (assignment [variable "x"] MinusEq [int 1])

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
        parseAssign "a = a" `shouldBe`
            r (assignment   [variable "a"]
                            Assign
                            [variable "a"])
        parseAssign "a, b = b, a" `shouldBe`
            r (assignment   [variable "a", variable "b"]
                            Assign
                            [variable "b", variable "a"])

    it "parses an assignment op" $ do
        parseAssign "a += a" `shouldBe`
            r (assignment [variable "a"] PlusEq [variable "a"])

    it "parses assignment on addressable operand" $ do
        parseAssign "struc.selector++" `shouldBe`
            r (assignment
                [selector (variable "struc") "selector"]
                PlusEq
                [int 1])

        parseAssign "arrayz[0]++" `shouldBe`
            r (assignment
                [index (variable "arrayz") (int 0)]
                PlusEq
                [int 1])

        parseAssign "*p++" `shouldBe`
            r (assignment
                [Fix $ UnaryOp Dereference (variable "p")]
                PlusEq
                [int 1])

    it "does not parse an assignment if the operand is not addressable" $ do
        parseAssign "(a + a) >>= 3" `shouldSatisfy` isLeft
        parseAssign "-a /= 3" `shouldSatisfy` isLeft
        parseAssign "6.nothing++" `shouldSatisfy` isLeft
        parseAssign "[]int(nope) *= 2" `shouldSatisfy` isLeft
        parseAssign "([]int(nope))[2]--" `shouldSatisfy` isLeft
        parseAssign "a.([]int) &= 127" `shouldSatisfy` isLeft
        parseAssign "a[:]++" `shouldSatisfy` isLeft
        parseAssign "g()++" `shouldSatisfy` isLeft
        parseAssign "`literally`--" `shouldSatisfy` isLeft

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

    it "does not parse when the operator has an explicit semi" $ do
        parseAssign "a =; a" `shouldSatisfy` isLeft
        parseAssign "a +=; a" `shouldSatisfy` isLeft
        parseAssign "a =\n a" `shouldSatisfy` isRight
        parseAssign "a +=\n a" `shouldSatisfy` isRight

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

    it "does not parse when the number of operands differs on each side" $ do
        parseAssign "a, b = 1" `shouldSatisfy` isLeft
        parseAssign "a = 1, 2" `shouldSatisfy` isLeft
        parseAssign "a, b += 1" `shouldSatisfy` isLeft
        parseAssign "a += 1, 2" `shouldSatisfy` isLeft

    it "does not parse when there is more than one operand in an assign-op" $ do
        parseAssign "a, b += 3, 4" `shouldSatisfy` isLeft
        parseAssign "a, b = 3, 4" `shouldSatisfy` isRight

shortVariableDeclaration :: SpecWith ()
shortVariableDeclaration = do
    let parseShortVarDecl = parseOnly (fmap bareStmt $ shortVarDeclP >>= unSemiP)

    it "parses one or multiple variable declarations" $ do
        parseShortVarDecl "x := 2" `shouldBe` r (shortVarDecl ["x"] [int 2])
        parseShortVarDecl "x, y := 2, 3" `shouldBe`
            r (shortVarDecl ["x", "y"] [int 2, int 3])

    it "does not parse when the := operator is not present" $ do
        parseShortVarDecl "x 2" `shouldSatisfy` isLeft
        parseShortVarDecl "x, y 2, 3" `shouldSatisfy` isLeft

    it "does not parse when elements on the left-hand side are not idents" $ do
        parseShortVarDecl "1 := 2" `shouldSatisfy` isLeft
        parseShortVarDecl "x, 1 := 2, 3" `shouldSatisfy` isLeft

    it "does not parse when either side has no elements" $ do
        parseShortVarDecl "x :=" `shouldSatisfy` isLeft
        parseShortVarDecl ":= 2" `shouldSatisfy` isLeft

    it "does not parse when sides have differing lengths" $ do
        parseShortVarDecl "x, y := 1" `shouldSatisfy` isLeft
        parseShortVarDecl "x := 1, 2" `shouldSatisfy` isLeft

    it "does not parse when any identifier has a semi" $ do
        parseShortVarDecl "x; := 1" `shouldSatisfy` isLeft
        parseShortVarDecl "x\n := 1" `shouldSatisfy` isLeft
        parseShortVarDecl "x, y;, z := 1, 2, 3" `shouldSatisfy` isLeft
        parseShortVarDecl "x, y\n, z := 1, 2, 3" `shouldSatisfy` isLeft
        parseShortVarDecl "x, y; := 1, 2" `shouldSatisfy` isLeft
        parseShortVarDecl "x, y\n:= 1, 2" `shouldSatisfy` isLeft

    it "does not parse when any expression but the last has a semi" $ do
        parseShortVarDecl "x, y := 1;, 2" `shouldSatisfy` isLeft
        parseShortVarDecl "x, y:= 1\n, 2" `shouldSatisfy` isLeft
        parseShortVarDecl "x, y, z := 1, 2;, 3" `shouldSatisfy` isLeft
        parseShortVarDecl "x, y, z := 1, 2\n, 3" `shouldSatisfy` isLeft

    it "does not parse when the last expression doesn't have a semi" $ do
        parseShortVarDecl "x := 1 {}" `shouldSatisfy` isLeft
        parseShortVarDecl "x, y := 1, 2 {}" `shouldSatisfy` isLeft

expressionStatement :: SpecWith()
expressionStatement = do
    let parseExprStmt = parseOnly (fmap bareStmt $ exprStmtP >>= unSemiP)

    it "parses function calls as statements" $ do
        parseExprStmt "f();" `shouldBe`
            (r (exprStmt (call (variable "f") Nothing [])))

    it "requires a semi on the expression" $ do
        parseExprStmt "f() {}" `shouldSatisfy` isLeft

    it "does not parse other expressions as statements" $ do
        parseExprStmt "aVariable;" `shouldSatisfy` isLeft
        parseExprStmt "2;" `shouldSatisfy` isLeft
        parseExprStmt "1.1;" `shouldSatisfy` isLeft
        parseExprStmt "\"StringLit\"" `shouldSatisfy` isLeft
        parseExprStmt "aStruct.aField;" `shouldSatisfy` isLeft
        parseExprStmt "[]typ(convert);" `shouldSatisfy` isLeft
        parseExprStmt "indexing[2];" `shouldSatisfy` isLeft
        parseExprStmt "binary + operator" `shouldSatisfy` isLeft
        parseExprStmt "-unaryMinus" `shouldSatisfy` isLeft
        parseExprStmt "slice[low:high]" `shouldSatisfy` isLeft
        parseExprStmt "slice[low:high:max]" `shouldSatisfy` isLeft
        parseExprStmt "assertion.([]typ)" `shouldSatisfy` isLeft

simpleStatement :: SpecWith ()
simpleStatement = do
    let parseSimpleStmt = parseOnly (fmap bareStmt $ simpleStmt >>= unSemiP)

    it "parses any kind of simple statement" $ do
        parseSimpleStmt "x := 2" `shouldSatisfy` isRight
        parseSimpleStmt "f()" `shouldSatisfy` isRight
        parseSimpleStmt "x = 2" `shouldSatisfy` isRight
        parseSimpleStmt "x += 2" `shouldSatisfy` isRight
        parseSimpleStmt ";" `shouldSatisfy` isRight
        parseSimpleStmt "" `shouldSatisfy` isLeft