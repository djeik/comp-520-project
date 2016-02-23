{-# LANGUAGE OverloadedStrings #-}

module Stmt
(
  statement
) where

import Language.GoLite
import Language.GoLite.SrcAnn
import Language.GoLite.Parser.SimpleStmts
import Language.GoLite.Syntax.Sugar as Sugar
import Core

statement :: SpecWith ()
statement = describe "stmt" $ do
                describe "assignStmt" assign
                describe "shortVarDecl" shortVariableDeclaration
                describe "simpleStmt" simpleStatement
                describe "varDecl" variableDeclaration
                describe "typeDecl" typeDeclaration
                describe "break/continue/fallthrough" simpleKeywordStmts
                describe "block" blockStatement

r :: b -> Either a b
r = Right
<<<<<<< HEAD
int = Fix. Literal . IntLit
varDeclStmt i t e = Fix $ DeclStmt $ VarDecl $ VarDeclBody i t e
=======

int :: GoInt -> Fix (ExprF id bin un (Literal a) ty)
int = Sugar.literal . IntLit
>>>>>>> 5834fdb3c223531fac0051abc8cc41e28ab4b923

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

simpleStatement :: SpecWith ()
simpleStatement = do
    let parseSimpleStmt = parseOnly (fmap bareStmt $ simpleStmt >>= unSemiP)

    it "parses any kind of simple statement" $ do
        parseSimpleStmt "x := 2" `shouldSatisfy` isRight
        parseSimpleStmt "f()" `shouldSatisfy` isRight
        parseSimpleStmt " x = 2" `shouldSatisfy` isRight
        parseSimpleStmt " x += 2" `shouldSatisfy` isRight

variableDeclaration :: SpecWith()
variableDeclaration = do
    let parseVarDecl = parseOnly (fmap (map bareStmt) varDeclP)
    let justInt = Just $ Fix $ NamedType "int"

    it "parses the three forms of declaration with one variable" $ do
        parseVarDecl "var x int = 2" `shouldBe`
            (r [varDeclStmt ["x"] justInt [int 2]])

        parseVarDecl "var x = 2" `shouldBe`
            (r [varDeclStmt ["x"] Nothing [int 2]])

        parseVarDecl "var x int" `shouldBe`
            (r [varDeclStmt ["x"] justInt []])

    it "parses the three forms of declaration with multiple variables" $ do
        parseVarDecl "var x, y int = 2, 3" `shouldBe`
            r [varDeclStmt ["x", "y"] justInt [int 2, int 3]]

        parseVarDecl "var x, y = 2, 3" `shouldBe`
            r [varDeclStmt ["x", "y"] Nothing [int 2, int 3]]

        parseVarDecl "var x, y int" `shouldBe`
            r [varDeclStmt ["x", "y"] justInt []]

    it "parses distributed versions of the three forms of declaration" $ do
        parseVarDecl "var ( x = 2; y int = 2; z int; )" `shouldBe`
            r [ varDeclStmt ["x"] Nothing [int 2],
                varDeclStmt ["y"] justInt [int 2],
                varDeclStmt ["z"] justInt []]

        parseVarDecl "var (x, y = 2, 3; z, w int = 4, 5; u, v int;)" `shouldBe`
            r [ varDeclStmt ["x", "y"] Nothing [int 2, int 3],
                varDeclStmt ["z", "w"] justInt [int 4, int 5],
                varDeclStmt ["u", "v"] justInt []]

    it "parses distributed statments with one spec as normal declarations" $ do
        parseVarDecl "var (x int;)" `shouldBe` parseVarDecl "var x int"
        parseVarDecl "var (x int = 2;)" `shouldBe` parseVarDecl "var x int = 2"
        parseVarDecl "var (x = 2;)" `shouldBe` parseVarDecl "var x = 2"

    it "parses empty distributed declarations" $ do
        parseVarDecl "var ()" `shouldBe` r []

    it "does not parse if a spec is missing a semicolon" $ do
        parseVarDecl "var (x = 2 y = 3;)" `shouldSatisfy` isLeft
        parseVarDecl "var (x int y = 3;)" `shouldSatisfy` isLeft
        parseVarDecl "var (x int = 2 y = 3;)" `shouldSatisfy` isLeft
        parseVarDecl "var (x, y = 2, 3 z = 3;)" `shouldSatisfy` isLeft
        parseVarDecl "var (x, y int y = 3;)" `shouldSatisfy` isLeft
        parseVarDecl "var (x, y int = 2, 3 y = 3;)" `shouldSatisfy` isLeft

    it "does not parse if there is no semicolon at the end" $ do
        parseVarDecl "var x = 2 {}" `shouldSatisfy` isLeft
        parseVarDecl "var x int {}" `shouldSatisfy` isLeft
        parseVarDecl "var x, y = 2, 3 {}" `shouldSatisfy` isLeft
        parseVarDecl "var (x = 2;) {}" `shouldSatisfy` isLeft

    it "does not parse if there is an explicit semi on the var keyword" $ do
        parseVarDecl "var; x = 2" `shouldSatisfy` isLeft
        parseVarDecl "var\n x = 2" `shouldSatisfy` isRight

    it "does not parse if one of the indentifiers has a semi" $ do
        parseVarDecl "var x; = 2" `shouldSatisfy` isLeft
        parseVarDecl "var x\n = 2" `shouldSatisfy` isLeft
        parseVarDecl "var x, y;, z = 2, 3, 4" `shouldSatisfy` isLeft
        parseVarDecl "var x, y\n, z = 2, 3, 4" `shouldSatisfy` isLeft

    it "does not parse if the type has a semi but there are no expressions" $ do
        parseVarDecl "var x int; = 2" `shouldSatisfy` isLeft
        parseVarDecl "var x int\n = 2" `shouldSatisfy` isLeft

        parseVarDecl "var x int;" `shouldSatisfy` isRight
        parseVarDecl "var x int\n" `shouldSatisfy` isRight

    it "does not parse if any expression but the last has a semi" $ do
        parseVarDecl "var x, y = 2;, 3" `shouldSatisfy` isLeft
        parseVarDecl "var x, y = 2\n, 3" `shouldSatisfy` isLeft

    it "does not parse if the left-hand side contains non-identifiers" $ do
        parseVarDecl "var x, 1 = 2, 3" `shouldSatisfy` isLeft
        parseVarDecl "var 1 = 2" `shouldSatisfy` isLeft

    it "does no parse if there are no identifiers on the left" $ do
        parseVarDecl "var = 3" `shouldSatisfy` isLeft
        parseVarDecl "var ( = x; )" `shouldSatisfy` isLeft

    it "does not parse if the var keyword is absent" $ do
        parseVarDecl "x = 3" `shouldSatisfy` isLeft
        parseVarDecl "(x = 3;)" `shouldSatisfy` isLeft

    it "does not parse if there is no assignment operator nor expressions" $ do
        parseVarDecl "var x 3" `shouldSatisfy` isLeft
        parseVarDecl "var x int 3" `shouldSatisfy` isLeft
        parseVarDecl "var (x 3;)" `shouldSatisfy` isLeft
        parseVarDecl "var (x int 3;)" `shouldSatisfy` isLeft

typeDeclaration :: SpecWith ()
typeDeclaration = do
    let parseTyDecl = parseOnly (fmap (map bareStmt) typeDeclP)
    let tyDeclStmt i t = Fix $ DeclStmt $ TypeDecl $ TypeDeclBody i t
    let intSlice = sliceType $ namedType "int"
    let boolSlice = sliceType $ namedType "bool"

    it "parses simple type declarations" $ do
        parseTyDecl "type a []int" `shouldBe`
            r [ tyDeclStmt "a" intSlice]

    it "parses distributed declarations (with zero, one or more specs)" $ do
        parseTyDecl "type ()" `shouldBe` r []

        parseTyDecl "type (a []int;)" `shouldBe` parseTyDecl "type a []int"

        parseTyDecl "type (a []int; b []bool;)" `shouldBe`
            r [ tyDeclStmt "a" intSlice,
                tyDeclStmt "b" boolSlice ]

    it "does not parse when not terminated by a semi" $ do
        parseTyDecl "type () {}" `shouldSatisfy` isLeft
        parseTyDecl "type (a []int;) {}" `shouldSatisfy` isLeft
        parseTyDecl "type (a []int; b []bool;) {}" `shouldSatisfy` isLeft

    it "does not parse when the type keyword has an explicit semi" $ do
        parseTyDecl "type; a []int" `shouldSatisfy` isLeft
        parseTyDecl "type\n a []int" `shouldSatisfy` isRight

    it "does not parse when the identifier has a semi" $ do
        parseTyDecl "type a; []int" `shouldSatisfy` isLeft
        parseTyDecl "type a\n []int" `shouldSatisfy` isLeft
        parseTyDecl "type (a; []int;)" `shouldSatisfy` isLeft
        parseTyDecl "type (a\n []int;)" `shouldSatisfy` isLeft

    it "does not parse when the type has no semi in a distributed decl" $ do
        parseTyDecl "type (a []int)" `shouldSatisfy` isLeft
        parseTyDecl "type (a []int b []int;)" `shouldSatisfy` isLeft
        parseTyDecl "type (a []int; b []int)" `shouldSatisfy` isLeft

    it "does not parse when any component (keyword, id, type) is missing" $ do
        parseTyDecl "a []int" `shouldSatisfy` isLeft
        parseTyDecl "(a []int;)" `shouldSatisfy` isLeft
        parseTyDecl "type []int" `shouldSatisfy` isLeft
        parseTyDecl "type ([]int;)" `shouldSatisfy` isLeft
        parseTyDecl "type a;" `shouldSatisfy` isLeft
        parseTyDecl "type (a;)" `shouldSatisfy` isLeft

simpleKeywordStmts :: SpecWith ()
simpleKeywordStmts = do
    let parseStmt = parseOnly (fmap (map bareStmt) stmt)
    it "parses the keywords `break`, `continue` and `fallthrough`" $ do
        parseStmt "break" `shouldBe` r [breakStmt]
        parseStmt "continue" `shouldBe` r [continueStmt]
        parseStmt "fallthrough" `shouldBe` r [fallthroughStmt]

    it "does not parses if the keywords are missing a semi" $ do
        parseStmt "break {}" `shouldSatisfy` isLeft
        parseStmt "continue {}" `shouldSatisfy` isLeft
        parseStmt "fallthrough {}" `shouldSatisfy` isLeft

blockStatement :: SpecWith ()
blockStatement = do
    let parseBlock = parseOnly (fmap bareStmt blockStmt)
    it "parses a block containing one, many or no statements" $ do
        parseBlock "{}" `shouldBe` r (block [])
        parseBlock "{x++\n}" `shouldBe`
            r (block [assignment [variable "x"] PlusEq [int 1]])

        parseBlock "{x++\ny++\n}" `shouldBe`
            r (block [ assignment [variable "x"] PlusEq [int 1],
                assignment [variable "y"] PlusEq [int 1] ])

    it "doesn't parse if one of the enclosing statements don't have a semi" $ do
        parseBlock "{x++}" `shouldSatisfy` isLeft
        parseBlock "{x++; y++}" `shouldSatisfy` isLeft

    it "doesn't parse if the block doesn't have a semi" $ do
        parseBlock "{} {}" `shouldSatisfy` isLeft

    it "parses nested blocks" $ do
        parseBlock "{x++;{y++;{z++;};};}" `shouldBe`
            r (block [(assignment [variable "x"] PlusEq [int 1]),
                block [(assignment [variable "y"] PlusEq [int 1]),
                 block [(assignment [variable "z"] PlusEq [int 1])]]])

    it "handles statements parsers that return multiple statements" $ do
        parseBlock "{var (x = 2; y = 3;); x++;}" `shouldBe`
            r (block [
                varDeclStmt ["x"] Nothing [int 2],
                varDeclStmt ["y"] Nothing [int 3],
                (assignment [variable "x"] PlusEq [int 1])])
