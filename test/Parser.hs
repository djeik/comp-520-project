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
    let conv = Conversion
    let intSlice = SliceType (NamedType "int")
    let sel = Selector

    it "parses variables" $ do
        parseExpr "a" `shouldBe` r (var "a")

    it "parses raw literals" $ do
        parseExpr "3" `shouldBe` r (int 3)
        parseExpr "3.0" `shouldBe` r (float 3.0)
        parseExpr "\"Hello, world!\"" `shouldBe` r (str "Hello, world!")
        parseExpr "`Hello,\nworld!`" `shouldBe` r (str "Hello,\nworld!")
        parseExpr "'a'" `shouldBe` r ((Literal . RuneLit) 'a')

    it "parses selectors" $ do
        parseExpr "a.b" `shouldBe` r (sel (var "a") "b")
        parseExpr "3.a" `shouldBe` r (sel (int 3) "a")
        parseExpr "3..a" `shouldBe` r (sel (float 3.0) "a")

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

    it "parses calls chained with other postfix operators" $ do
        parseExpr "f(a)(b)(c)"
            `shouldBe`
            r (Call (Call (Call (var "f")
                                (args [var "a"]))
                          (args [var "b"]))
                    (args [var "c"]))

        parseExpr "[]int(a)(1)"
            `shouldBe`
            r (Call (conv intSlice (var "a")) (args [int 1]))

        parseExpr "x.f(2)"
            `shouldBe`
            r (Call (sel (var "x") "f") (args [int 2]))

        parseExpr "fs[0](a)"
            `shouldBe`
            r (Call (Index (var "fs") (int 0)) (args [var "a"]))

        parseExpr "fs[1:2](a)"
            `shouldBe`
            r (Call
                (Slice (var "fs")
                    (SliceFromTo (Just $ int 1) (Just $ int 2)))
                (args [var "a"]))

        parseExpr "f.([]int)(2)"
            `shouldBe`
            r (Call
                (TypeAssertion (var "f") intSlice)
                (args [int 2]))
