{-# LANGUAGE OverloadedStrings #-}

module Parser.StmtDecl (
  variableDeclaration
, typeDeclaration
) where

import Core

-- | Keystroke-saver for a variable declaration statement.
varDeclStmt :: [id] -> Maybe ty -> [e]
            -> Fix (StatementF (Declaration tyDecl (VarDecl id ty e)) ex i a c)
varDeclStmt i t e = Fix $ DeclStmt $ VarDecl $ VarDeclBody i t e


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
        parseVarDecl "var (y = 3)" `shouldSatisfy` isLeft

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

    it "does not parse if there is no type nor expressions" $ do
        parseVarDecl "var x" `shouldSatisfy` isLeft
        parseVarDecl "var x, y" `shouldSatisfy` isLeft

    it "does not parse if either side has a different number of expressions" $ do
        parseVarDecl "var x = 1, 2" `shouldSatisfy` isLeft
        parseVarDecl "var x, y = 2" `shouldSatisfy` isLeft
        parseVarDecl "var x int = 1, 2" `shouldSatisfy` isLeft
        parseVarDecl "var x, y int = 2" `shouldSatisfy` isLeft


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
