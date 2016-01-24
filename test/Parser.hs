module Parser where

import Language.GoLite
import TestUtil

parser :: SpecWith()
parser = describe "Parser" $ do
    describe "Expression" $ do
        expression

expression :: SpecWith ()
expression = describe "expr" $ do
    -- define some shorthands to save some keystrokes
    let parseExpr = parseOnly (expr >>= unSemiP)
    let r = Right
    let var = Variable
    let int = Literal . IntLit
    let float = Literal . FloatLit
    let str = Literal . StringLit
    let args = NormalArguments
    let noArgs = args []
    let bin = BinaryOp
    let un = UnaryOp

    it "parses variables" $ do
        parseExpr "a" `shouldBe` r (var "a")

    it "parses raw literals" $ do
        parseExpr "3" `shouldBe` r (int 3)
        parseExpr "3.0" `shouldBe` r (float 3.0)
        parseExpr "\"Hello, world!\"" `shouldBe` r (str "Hello, world!")

    it "parses unary minus" $ do
        parseExpr "-010" `shouldBe` r (un Negative $ int 8)
        parseExpr "-.7" `shouldBe` r (un Negative $ float 0.7)
        parseExpr "-\"hello\"" `shouldBe` r (un Negative $ str "hello")

    it "parses function calls with normal arguments" $ do
        parseExpr "a(3, 4, b)"
            `shouldBe`
            r (Call (var "a") (NormalArguments [int 3, int 4, var "b"]))

        parseExpr "complicatedFunction13()"
            `shouldBe`
            r (Call (var "complicatedFunction13") noArgs)

        parseExpr "a(b(a), c(b), d(c))"
            `shouldBe`
            r (Call (var "a")
                    (args [ Call (var "b") (args [var "a"])
                          , Call (var "c") (args [var "b"])
                          , Call (var "d") (args [var "c"])
                          ]))

        parseExpr "(3 + 4)(4 / a)"
            `shouldBe`
            r (Call (bin Plus (int 3) (int 4))
                    (args [bin Divide (int 4) (var "a")]))

    it "parses chained function calls" $ do
        parseExpr "f(a)(b)(c)"
            `shouldBe`
            r (Call (Call (Call (var "f")
                                (args [var "a"]))
                          (args [var "b"]))
                    (args [var "c"]))

