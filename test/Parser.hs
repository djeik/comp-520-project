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
    let bin = BinaryOp
    let un = UnaryOp
    let conv = Conversion
    let intSlice = SliceType (NamedType "int")
    let sel = Selector
    let ta = TypeAssertion

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

    it "parses conversions" $ do
        parseExpr "[]int(a)" `shouldBe` r (conv intSlice (var "a"))

    it "parses indexing operators" $ do
        parseExpr "t[2]" `shouldBe` r (Index (var "t") (int 2))
        parseExpr "t[a]" `shouldBe` r (Index (var "t") (var "a"))
        parseExpr "t[f[0]]" `shouldBe` r (Index (var "t")
                                            (Index (var "f") (int (0))))

    it "parses unary minus" $ do
        parseExpr "-010" `shouldBe` r (un Negative $ int 8)
        parseExpr "-.7" `shouldBe` r (un Negative $ float 0.7)
        parseExpr "-\"hello\"" `shouldBe` r (un Negative $ str "hello")

    it "parses slices" $ do
        parseExpr "a[1:2]" `shouldBe` r
            (Slice (var "a") (Just $ int 1) (Just $ int 2) Nothing)
        parseExpr "a[:1]" `shouldBe` r
            (Slice (var "a") Nothing (Just $ int 1) Nothing)
        parseExpr "a[1:]" `shouldBe` r
            (Slice (var "a") (Just $ int 1) Nothing Nothing)
        parseExpr "a[:]" `shouldBe` r
            (Slice (var "a") Nothing Nothing Nothing)
        parseExpr "a[0:10:2]" `shouldBe` r
            (Slice (var "a") (Just $ int 0) (Just $ int 10) (Just $ int 2))
        parseExpr "a[:10:2]" `shouldBe` r
            (Slice (var "a") Nothing (Just $ int 10) (Just $ int 2))

        parseExpr "a[::2]" `shouldSatisfy` isLeft
        parseExpr "a[:2:]" `shouldSatisfy` isLeft
        parseExpr "a[::]" `shouldSatisfy` isLeft

    it "parses type assertions" $ do
        parseExpr "x.([]int)" `shouldBe`
            r (TypeAssertion (var "x") (intSlice))

    it "parses function calls with normal arguments" $ do
        parseExpr "a(3, 4, b)"
            `shouldBe`
            r (Call
                (var "a")
                Nothing
                [int 3, int 4, var "b"])

        parseExpr "complicatedFunction13()"
            `shouldBe`
            r (Call
                (var "complicatedFunction13")
                Nothing
                [])

        parseExpr "a(b(a), c(b), d(c))"
            `shouldBe`
            r (Call (var "a")
                Nothing
                [ Call (var "b") Nothing [var "a"]
                , Call (var "c") Nothing [var "b"]
                , Call (var "d") Nothing [var "c"]
                ])

        parseExpr "(3 + 4)(4 / a)"
            `shouldBe`
            r (Call
                (bin Plus (int 3) (int 4))
                Nothing
                [bin Divide (int 4) (var "a")])

    it "parses calls chained with other postfix operators" $ do
        parseExpr "f(a)(b)(c)"
            `shouldBe`
            r (Call
                (Call
                    (Call
                        (var "f")
                        Nothing
                        [var "a"])
                    Nothing
                    [var "b"])
                Nothing
                [var "c"])

        parseExpr "[]int(a)(1)"
            `shouldBe`
            r (Call (conv intSlice (var "a")) Nothing [int 1])

        parseExpr "x.f(2)"
            `shouldBe`
            r (Call (sel (var "x") "f") Nothing [int 2])

        parseExpr "fs[0](a)"
            `shouldBe`
            r (Call (Index (var "fs") (int 0)) Nothing [var "a"])

        parseExpr "fs[1:2](a)"
            `shouldBe`
            r (Call
                (Slice (var "fs") (Just $ int 1) (Just $ int 2) Nothing)
                Nothing [var "a"])

        parseExpr "f.([]int)(2)"
            `shouldBe`
            r (Call
                (TypeAssertion (var "f") intSlice)
                Nothing [int 2])

    it "parses conversions chained with other postfix operators" $ do
        parseExpr "[]int([]float(a))" `shouldBe`
            r (conv intSlice
                (conv (SliceType (NamedType "float"))
                    (var "a")))

        parseExpr "[]int(a.x)" `shouldBe`
            r (conv intSlice
                (sel (var "a") "x"))

        parseExpr "[]int(a[0])" `shouldBe`
            r (conv intSlice
                (Index (var "a") (int 0)))

        parseExpr "[]int(a[:])" `shouldBe`
            r (conv intSlice
                (Slice (var "a") Nothing Nothing Nothing))

        parseExpr "[]int(a.([]int))" `shouldBe`
            r (conv intSlice (ta (var "a") intSlice))

        parseExpr "[]int(f())" `shouldBe`
            r (conv intSlice
                (Call (var "f") Nothing []))

    it "parses selectors chained with other postfix operators" $ do
        parseExpr "[]int(a).x" `shouldBe` -- TODO Check golang
            r (sel (conv intSlice (var "a")) "x")

        parseExpr "a.x.y" `shouldBe` r (sel (sel (var "a") "x") "y")

        parseExpr "a[0].x" `shouldBe` r (sel (Index (var "a") (int 0)) "x")

        parseExpr "a[:].x" `shouldBe`
            r (sel (Slice (var "a") Nothing Nothing Nothing) "x")

        parseExpr "a.([]int).x" `shouldBe` -- TODO Check golang
            r (sel (ta (var "a") intSlice) "x")

        parseExpr "f().x" `shouldBe` r (sel (Call (var "f") Nothing []) "x")

    it "parses indices chained with other postfix operators" $ do
        parseExpr "[]int(a)[0]" `shouldBe`
            r (Index (conv intSlice (var "a")) (int 0))

        parseExpr "a.x[0]" `shouldBe` r (Index (sel (var "a") "x") (int 0))

        parseExpr "a[0][1]" `shouldBe`
            r (Index (Index (var "a") (int 0)) (int 1))

        parseExpr "a[:][0]" `shouldBe`
            r (Index (Slice (var "a") Nothing Nothing Nothing) (int 0))

        parseExpr "a.([]int)[0]" `shouldBe`
            r (Index (ta (var "a") intSlice) (int 0))

        parseExpr "f()[0]" `shouldBe` r (Index (Call (var "f") Nothing []) (int 0))

    it "parses slices chained with other postfix operators" $ do
        parseExpr "[]int(a)[:]" `shouldBe`
            r (Slice (conv intSlice (var "a")) Nothing Nothing Nothing)

        parseExpr "a.x[:]" `shouldBe`
            r (Slice (sel (var "a") "x") Nothing Nothing Nothing)

        parseExpr "a[0][:]" `shouldBe`
            r (Slice (Index (var "a") (int 0)) Nothing Nothing Nothing)

        parseExpr "a.([]int)[:]" `shouldBe`
            r (Slice (ta (var "a") intSlice) Nothing Nothing Nothing)

        parseExpr "f()[:]" `shouldBe`
            r (Slice (Call (var "f") Nothing []) Nothing Nothing Nothing)

    it "parses type assertions chained with other postfix operators" $ do
        parseExpr "[]int(a).([]int)" `shouldBe`
            r (ta (conv intSlice (var ("a"))) intSlice)

        parseExpr "a.x.([]int)" `shouldBe` r (ta (sel (var "a") "x") intSlice)

        parseExpr "a[0].([]int)" `shouldBe`
            r (ta (Index (var "a") (int 0)) intSlice)

        parseExpr "a.([]int).([]int)" `shouldBe`
            r (ta (ta (var ("a")) intSlice) intSlice)

        parseExpr "f().([]int)" `shouldBe`
            r (ta (Call (var "f") Nothing []) intSlice)
