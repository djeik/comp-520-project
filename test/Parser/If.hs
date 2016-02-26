{-# LANGUAGE OverloadedStrings #-}

module Parser.If where

import Core

ifStatement :: SpecWith ()
ifStatement = do
    let parseIf = parseOnly (fmap bareStmt ifStmtP)

    it "parses a normal if statement, with or without an initializer" $ do
        parseIf "if 2 > 1 { ; } " `shouldBe`
            r (ifStmt
                Nothing
                (binaryOp GreaterThan (int 2) (int 1))
                [emptyStmt]
                Nothing)

        parseIf "if x, y := 3, 4; x > y { ; }" `shouldBe`
            r (ifStmt
                (Just $ shortVarDecl ["x", "y"] [int 3, int 4])
                (binaryOp GreaterThan (variable "x") (variable "y"))
                [emptyStmt]
                Nothing)

        parseIf "if x, y := 3, 4; x > y { ; }"
            `shouldBe`
            (parseIf "if x, y := 3, 4\n x > y { ; }")

        parseIf "if x, y := 3, 4; x > y { }" `shouldBe`
            r (ifStmt
                (Just $ shortVarDecl ["x", "y"] [int 3, int 4])
                (binaryOp GreaterThan (variable "x") (variable "y"))
                []
                Nothing)

        parseIf "if ; x > y { ; }" `shouldBe`
            r (ifStmt
                (Just $ emptyStmt)
                (binaryOp GreaterThan (variable "x") (variable "y"))
                [emptyStmt]
                Nothing)

    it "parses an if statement with an else or else-if part" $ do
        parseIf "if false { ; } else { ; }" `shouldBe`
            r (ifStmt
                Nothing
                (variable "false")
                [emptyStmt]
                (Just [emptyStmt]))

        parseIf "if false { } else { }" `shouldBe`
            r (ifStmt
                Nothing
                (variable "false")
                []
                (Just []))

        parseIf "if false { ; } else if true { ; }" `shouldBe`
            r (ifStmt
                Nothing
                (variable "false")
                [emptyStmt]
                (Just [(ifStmt
                            Nothing
                            (variable "true")
                            [emptyStmt]
                            Nothing)]))

    it "does not parse if the initializer doesn't have a semi" $ do
        parseIf "if x += 2 3 == 2 { }" `shouldSatisfy` isLeft

    it "doesn't parse if the expression has a semi" $ do
        parseIf "if 2 > 3; { }" `shouldSatisfy` isLeft

    it "doesn't parse if the expression is missing" $ do
        parseIf "if x := 2; { }" `shouldSatisfy` isLeft
        parseIf "if { }" `shouldSatisfy` isLeft

    it "doesn't parse if the else keyword has an explicit semi" $ do
        parseIf "if 2 > 3 { } else; {}" `shouldSatisfy` isLeft
        parseIf "if 2 > 3 { } else\n {}" `shouldSatisfy` isRight

    it "doesn't parse if the first block has a semi" $ do
        parseIf "if 2 > 3 { }\n else {}" `shouldSatisfy` isLeft
        parseIf "if 2 > 3 { }; else {}" `shouldSatisfy` isLeft

    it "doesn't parse if some keywords are absent" $ do
        parseIf "2 > 3 { }" `shouldSatisfy` isLeft
        parseIf "x := 2; 2 > 3 { }" `shouldSatisfy` isLeft

        parseIf "if 2 > 3 { } { }" `shouldSatisfy` isLeft
        parseIf "if 2 > 3 { } if { }" `shouldSatisfy` isLeft

        parseIf "2 > 3 { } { }" `shouldSatisfy` isLeft

    it "doesn't parse if some blocks are absent" $ do
        parseIf "if 2 > 3 else {}" `shouldSatisfy` isLeft
        parseIf "if 2 > 3 {} else" `shouldSatisfy` isLeft
        parseIf "if 2 > 3 else" `shouldSatisfy` isLeft

        parseIf "if 2 > 3 else if 3 > 2 {}" `shouldSatisfy` isLeft
        parseIf "if 2 > 3 {} else if 3 > 2" `shouldSatisfy` isLeft
        parseIf "if 2 > 3 else if 3 > 2" `shouldSatisfy` isLeft

        parseIf "if 2 > 3" `shouldSatisfy` isLeft
        parseIf "if x := 2; 2 > 3" `shouldSatisfy` isLeft

    it "parses an `else if` like an `else {if ...}`" $ do
        parseIf "if x {} else if y {}"
            `shouldBe`
            (parseIf "if x {} else { if y {}; }")


