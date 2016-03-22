{-# LANGUAGE OverloadedStrings #-}

module Parser.Switch where

import Core

switchStatement :: SpecWith ()
switchStatement = do
    let parseSwitch = parseOnly (fmap bareStmt switchStmtP)
    it "parses switch statements with or without init/expression/clauses" $ do
        parseSwitch "switch {}" `shouldBe`
            r (switchStmt Nothing Nothing [])

        parseSwitch "switch ; {}" `shouldBe`
            r (switchStmt (Just emptyStmt) Nothing [])

        parseSwitch "switch 2 > 1 {}" `shouldBe`
            r (switchStmt
                Nothing
                (Just $ binaryOp GreaterThan (int 2) (int 1))
                [])

        parseSwitch "switch ; 2 > 1 {}" `shouldBe`
            r (switchStmt
                (Just emptyStmt)
                (Just $ binaryOp GreaterThan (int 2) (int 1))
                [])

        parseSwitch "switch x = 2; {}" `shouldBe`
            r (switchStmt
                (Just $ assignment [variable "x"] Assign [int 2])
                Nothing
                [])

        parseSwitch "switch x = 2; 2 > 1 {}" `shouldBe`
            r (switchStmt
                (Just $ assignment [variable "x"] Assign [int 2])
                (Just $ binaryOp GreaterThan (int 2) (int 1))
                [])

        parseSwitch "switch x = 2; 2 > 1 {}"
            `shouldBe` parseSwitch "switch x = 2\n2 > 1 {}"

        parseSwitch "switch {case a: x++;}" `shouldBe`
            r (switchStmt
                Nothing
                Nothing
                [(CaseExpr [variable "a"],
                    [increment (variable "x") ])])

        parseSwitch "switch {case a: x++; default: y++;}" `shouldBe`
            r (switchStmt
                Nothing
                Nothing
                [(CaseExpr [variable "a"],
                    [increment (variable "x") ]),
                 (CaseDefault,
                    [increment (variable "y") ])])

        parseSwitch "switch {case a: x++; x++; default: y++;}" `shouldBe`
            r (switchStmt
                Nothing
                Nothing
                [(CaseExpr [variable "a"],
                    [increment (variable "x") ,
                    increment (variable "x") ]),
                 (CaseDefault,
                    [increment (variable "y") ])])

    it "parses a switch with a multi-clause" $ do
        parseSwitch "switch {case a, b: x++;}" `shouldBe`
            r (switchStmt
                Nothing
                Nothing
                [ ( CaseExpr [variable "a", variable "b"]
                  , [ increment (variable "x") ]
                  )
                ]
            )

    it "parses a switch with a clause containing no statements" $ do
        parseSwitch "switch {case a: }" `shouldBe`
            r (switchStmt
                Nothing
                Nothing
                [(CaseExpr [variable "a"], [])])

        parseSwitch "switch {case a: case b:}" `shouldBe`
            r (switchStmt
                Nothing
                Nothing
                [(CaseExpr [variable "a"], []),
                 (CaseExpr [variable "b"], [])])


    it "doesn't care about whitespace in the expressions" $ do
        parseSwitch "switch {case a,b: }"
            `shouldBe` parseSwitch "switch {case a\t,\tb\t: }"

    it "does not parse when the expressions in a clause have semis" $ do
        parseSwitch "switch {case a;: }" `shouldSatisfy` isLeft
        parseSwitch "switch {case a\n: }" `shouldSatisfy` isLeft
        parseSwitch "switch {case a;, b: }" `shouldSatisfy` isLeft
        parseSwitch "switch {case a\n, b: }" `shouldSatisfy` isLeft

    it "does not parse if the colon in a clause is missing" $ do
        parseSwitch "switch {case a x++;}" `shouldSatisfy` isLeft
        parseSwitch "switch {case a, b x++;}" `shouldSatisfy` isLeft
        parseSwitch "switch {case default x++;}" `shouldSatisfy` isLeft

    it "does not parse if there are no expressions in a non-default clause" $ do
        parseSwitch "switch {case: x++;}" `shouldSatisfy` isLeft

    it "does not parse if there are expressions in a default clause" $ do
        parseSwitch "switch {default a: x++;}" `shouldSatisfy` isLeft
        parseSwitch "switch {default a, b: x++;}" `shouldSatisfy` isLeft

    it "does not parse if the case/default keywords have explicit semis" $ do
        parseSwitch "switch {case; a: x++;}" `shouldSatisfy` isLeft
        parseSwitch "switch {default;: x++;}" `shouldSatisfy` isLeft
        parseSwitch "switch {case\n a: x++;}" `shouldSatisfy` isRight
        parseSwitch "switch {default\n: x++;}" `shouldSatisfy` isRight

    it "does not parse if there are semis in the expressions" $ do
        parseSwitch "switch {case a;: x++;}" `shouldSatisfy` isLeft
        parseSwitch "switch {case a\n: x++;}" `shouldSatisfy` isLeft
        parseSwitch "switch {case a, b;, c: x++;}" `shouldSatisfy` isLeft
        parseSwitch "switch {case a, b\n, c: x++;}" `shouldSatisfy` isLeft

    it "does not parse if the initializer is not a simple statement" $ do
        parseSwitch "switch switch {}; {}" `shouldSatisfy` isLeft
        parseSwitch "switch break; {}" `shouldSatisfy` isLeft

    it "does not parse if the initializer has no semi" $ do
        parseSwitch "switch x := 2 {}" `shouldSatisfy` isLeft
        parseSwitch "switch x := 2 x > 2 {}" `shouldSatisfy` isLeft

    it "does not parse if the expression has a semi" $ do
        parseSwitch "switch x > 2; {}" `shouldSatisfy` isLeft
        parseSwitch "switch x > 2\n {}" `shouldSatisfy` isLeft

    it "does not parse if there is no block" $ do
        parseSwitch "switch" `shouldSatisfy` isLeft

    it "does not parse if the switch keyword is missing" $ do
        parseSwitch "2 > 1 {}" `shouldSatisfy` isLeft
        parseSwitch "x = 2; {}" `shouldSatisfy` isLeft
        parseSwitch "x = 2; 2 > 1 {}" `shouldSatisfy` isLeft
