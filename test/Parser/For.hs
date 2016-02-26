{-# LANGUAGE OverloadedStrings #-}

module Parser.For where

import Core

forStatement :: SpecWith ()
forStatement = do
    let parseFor = parseOnly (fmap bareStmt forStmtP)

    it "parses the infinite for" $ do
        parseFor "for\n { ; }" `shouldBe`
            r (forStmt Nothing Nothing Nothing [emptyStmt])

        parseFor "for { ; }" `shouldBe`
            r (forStmt Nothing Nothing Nothing [emptyStmt])

        parseFor "for { }" `shouldBe`
            r (forStmt Nothing Nothing Nothing [])

    it "parses the expression for" $ do
        parseFor "for 2 > 1 { ; }" `shouldBe`
            r (forStmt
                Nothing
                (Just $ binaryOp GreaterThan (int 2) (int 1))
                Nothing [emptyStmt])

        parseFor "for\n 2 > 1 { ; }" `shouldBe`
            r (forStmt
                Nothing
                (Just $ binaryOp GreaterThan (int 2) (int 1))
                Nothing [emptyStmt])

        parseFor "for 2 > 1 {}" `shouldBe`
         r (forStmt
                Nothing
                (Just $ binaryOp GreaterThan (int 2) (int 1))
                Nothing [])

    it "parses the full for with or without its components" $ do
        parseFor "for ;; { ; }" `shouldBe`
            r (forStmt
                (Just $ emptyStmt)
                Nothing
                Nothing
                [emptyStmt])

        parseFor "for a() ;; { ; }" `shouldBe`
            r (forStmt
                (Just $ exprStmt $ call (variable "a") Nothing [])
                Nothing
                Nothing
                [emptyStmt])

        parseFor "for ; 2 > 1; { ; }" `shouldBe`
            r (forStmt
                (Just $ emptyStmt)
                (Just $ binaryOp GreaterThan (int 2) (int 1))
                Nothing
                [emptyStmt])

        parseFor "for ;; b() { ; }" `shouldBe`
            r (forStmt
                (Just $ emptyStmt)
                Nothing
                (Just $ exprStmt $ call (variable "b") Nothing [])
                [emptyStmt])

        parseFor "for a() ; 2 > 1 ; { ; }" `shouldBe`
            r (forStmt
                (Just $ exprStmt $ call (variable "a") Nothing [])
                (Just $ binaryOp GreaterThan (int 2) (int 1))
                Nothing
                [emptyStmt])

        parseFor "for ; 2 > 1 ; b() { ; }" `shouldBe`
            r (forStmt
                (Just $ emptyStmt)
                (Just $ binaryOp GreaterThan (int 2) (int 1))
                (Just $ exprStmt $ call (variable "b") Nothing [])
                [emptyStmt])

        parseFor "for a() ; ; b() { ; }" `shouldBe`
            r (forStmt
                (Just $ exprStmt $ call (variable "a") Nothing [])
                Nothing
                (Just $ exprStmt $ call (variable "b") Nothing [])
                [emptyStmt])

        parseFor "for a() ; 2 > 1 ; b() { ; }" `shouldBe`
            r (forStmt
                (Just $ exprStmt $ call (variable "a") Nothing [])
                (Just $ binaryOp GreaterThan (int 2) (int 1))
                (Just $ exprStmt $ call (variable "b") Nothing [])
                [emptyStmt])

        parseFor "for a() ; 2 > 1 ; b() { }" `shouldBe`
            r (forStmt
                (Just $ exprStmt $ call (variable "a") Nothing [])
                (Just $ binaryOp GreaterThan (int 2) (int 1))
                (Just $ exprStmt $ call (variable "b") Nothing [])
                [])

    it "does not parse a loop with a short var decl in post position" $ do
        parseFor "for ;; x := 2 {}" `shouldSatisfy` isLeft

        -- Other simple statements are okay.
        parseFor "for ;; x++ {}" `shouldSatisfy` isRight
        parseFor "for ;; x += 2 {}" `shouldSatisfy` isRight
        parseFor "for ;; f() {}" `shouldSatisfy` isRight

    it "does not parse when the `for` keyword is missing" $ do
        -- Does not work in any version of the loop.
        parseFor "{}" `shouldSatisfy` isLeft
        parseFor "x > 2 {}" `shouldSatisfy` isLeft
        parseFor ";; {}" `shouldSatisfy` isLeft

    it "does not parse when the block is missing" $ do
        parseFor "for" `shouldSatisfy` isLeft
        parseFor "for x > 2" `shouldSatisfy` isLeft
        parseFor "for ;;" `shouldSatisfy` isLeft
        parseFor "for ;; f()" `shouldSatisfy` isLeft

    it "does not parse if the statement has no semi" $ do
        parseFor "for {} {}" `shouldSatisfy` isLeft
        parseFor "for x > 2 {} {}" `shouldSatisfy` isLeft
        parseFor "for ;; {} {}" `shouldSatisfy` isLeft

    it "does not parse if the expression in a simple for has a semi" $ do
        parseFor "for x > 2; {}" `shouldSatisfy` isLeft
        parseFor "for x; > 2 {}" `shouldSatisfy` isLeft

    it "does not parse if some semis are missing from the full for" $ do
        parseFor "for ; {}" `shouldSatisfy` isLeft
        parseFor "for f(); {}" `shouldSatisfy` isLeft
        parseFor "for ; x > 2 f() {}" `shouldSatisfy` isLeft
        parseFor "for ; x > 2 {}" `shouldSatisfy` isLeft
        parseFor "for x > 2; f() {}" `shouldSatisfy` isLeft

