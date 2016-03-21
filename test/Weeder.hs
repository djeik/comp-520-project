module Weeder where

import Core
import Language.GoLite.Weeder.Stmt
import Language.GoLite.Weeder.Expr
import Language.GoLite.Weeder.TypeLit

weeder :: SpecWith ()
weeder = describe "Weeder" $ do
    describe "Package" $ do
        it "cannot have the blank identifier as a name" $ do
            withParseAndWeed weedPackage packageP "package _;"
            `shouldSatisfy` isWeedError

    describe "Function declaration" $ do
        it "must end with a terminating statement" $ do
            withParseAndWeed weedFunDecl funDecl
                "func a() int {\
                \  return 2;\
                \}"
                `shouldSatisfy` isRight

            withParseAndWeed weedFunDecl funDecl
                "func a() int {\
                \  {  ;; return 2; }\n\
                \}"
                `shouldSatisfy` isRight

            withParseAndWeed weedFunDecl funDecl
                "func a() int {\
                \  {  ;; return 2; }\n\
                \}"
                `shouldSatisfy` isRight

            withParseAndWeed weedFunDecl funDecl
                "func a() int {\
                \  if false { return 2; } else { return 3; }\n\
                \}"
                `shouldSatisfy` isRight

            withParseAndWeed weedFunDecl funDecl
                "func a() int {\
                \  for {}\n\
                \}"
                `shouldSatisfy` isRight

            withParseAndWeed weedFunDecl funDecl
                "func a() int {\
                \  switch a {\n\
                    \case a: return 1\n\
                    \default: return 0\n\
                  \}\n\
                \}"
                `shouldSatisfy` isRight

            withParseAndWeed weedFunDecl funDecl
                "func a() int { ; }" `shouldSatisfy` isWeedError

            withParseAndWeed weedFunDecl funDecl
                "func a() int { return; }" `shouldSatisfy` isWeedError

            withParseAndWeed weedFunDecl funDecl
                "func a() int {\
                \  if false { return 2; }\
                \}"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedFunDecl funDecl
                "func a() int {\
                \  if false { } else { return 2; }\n\
                \}"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedFunDecl funDecl
                "func a() int {\
                \  for { break; }\n\
                \}"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedFunDecl funDecl
                "func a() int {\
                \  switch a {\n\
                    \case a: break;\n\
                    \default: return 0\n\
                  \}\n\
                \}"
                `shouldSatisfy` isWeedError

    describe "Statement" $ do
        it "must not allow the blank identifier on the left of an assign-op" $ do
            withParseAndWeed weedStmt oneStmt
                "_ += 2"
                `shouldSatisfy` isWeedError

        it "may allow the blank identifier on the right of an assignment" $ do
            withParseAndWeed weedStmt oneStmt
                "_ = 2"
                `shouldSatisfy` isRight

        it "must not allow the blank identifier on the right" $ do
            withParseAndWeed weedStmt oneStmt
                "a = _"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedStmt oneStmt
                "a += _"
                `shouldSatisfy` isWeedError

        it "must only allow returns that agree with their function" $ do
            withParseAndWeed weedFunDecl funDecl
                "func f() { return; }"
                `shouldSatisfy` isRight

            withParseAndWeed weedFunDecl funDecl
                "func f() { return 2; }"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedFunDecl funDecl
                "func f() int { return; }"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedFunDecl funDecl
                "func f() int { return 2; }"
                `shouldSatisfy` isRight

        it "must not allow multiple defaults in a switch" $ do
            withParseAndWeed weedStmt oneStmt
                "switch { default: default: }"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedStmt oneStmt
                "switch { case a: default: }"
                `shouldSatisfy` isRight

            withParseAndWeed weedStmt oneStmt
                "switch { case a: case b: }"
                `shouldSatisfy` isRight

        it "must only allow breaks in a switch or loop" $ do
            withParseAndWeed weedFunDecl funDecl
                "func f() { break; }"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedFunDecl funDecl
                "func f() {\n\
                \    for {\n\
                \       break;\n\
                \    }\n\
                \}"
                `shouldSatisfy` isRight

            withParseAndWeed weedFunDecl funDecl
                "func f() {\n\
                \    for {}\n\
                \    break;\n\
                \}"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedFunDecl funDecl
                "func f() {\n\
                \    for {\n\
                \       for {}\n\
                \       break;\n\
                \    }\n\
                \}"
                `shouldSatisfy` isRight

            withParseAndWeed weedFunDecl funDecl
                "func f() {\n\
                \    switch {\n\
                \       default: break;\n\
                \    }\n\
                \}"
                `shouldSatisfy` isRight

            withParseAndWeed weedFunDecl funDecl
                "func f() {\n\
                \    switch {\n\
                \       default: break;\n\
                \    }\n\
                \}"
                `shouldSatisfy` isRight

            withParseAndWeed weedFunDecl funDecl
                "func f() {\n\
                \    switch {}\n\
                \    break;\n\
                \}"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedFunDecl funDecl
                "func f() { continue; }"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedFunDecl funDecl
                "func f() {\n\
                \    for {\n\
                \       continue;\n\
                \    }\n\
                \}"
                `shouldSatisfy` isRight

            withParseAndWeed weedFunDecl funDecl
                "func f() {\n\
                \    for {}\n\
                \    continue;\n\
                \}"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedFunDecl funDecl
                "func f() {\n\
                \    for {\n\
                \       for {}\n\
                \       continue;\n\
                \    }\n\
                \}"
                `shouldSatisfy` isRight

        it "does not support fallthrough" $ do
            withParseAndWeed weedStmt oneStmt
                "fallthrough"
                `shouldSatisfy` isLeft

    describe "Expression" $ do
        it "must not refer to a blank field" $ do
            withParseAndWeed weedExpr (expr >>= unSemiP)
                "a._"
                `shouldSatisfy` isWeedError

        it "must not accept type assertions" $ do
            withParseAndWeed weedExpr (expr >>= unSemiP)
                "a.(int)"
                `shouldSatisfy` isWeedError

        it "must not accept the blank identifier" $ do
            withParseAndWeed weedExpr (expr >>= unSemiP)
                "_"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedExpr (expr >>= unSemiP)
                "_.x"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedExpr (expr >>= unSemiP)
                "_()"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedExpr (expr >>= unSemiP)
                "a(_)"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedExpr (expr >>= unSemiP)
                "_ + _"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedExpr (expr >>= unSemiP)
                "+_"
                `shouldSatisfy` isWeedError

    describe "Type" $ do
        it "must not accept types named with the blank identifier" $ do
            withParseAndWeed weedType (type_ >>= unSemiP)
                "_"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedType (type_ >>= unSemiP)
                "[]_"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedType (type_ >>= unSemiP)
                "[10]_"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedType (type_ >>= unSemiP)
                "struct { a int; b _; }"
                `shouldSatisfy` isWeedError

        it "must not accept structs with duplicate fields" $ do
            withParseAndWeed weedType (type_ >>= unSemiP)
                "struct { a, a int; }"
                `shouldSatisfy` isWeedError

            withParseAndWeed weedType (type_ >>= unSemiP)
                "struct { _, _ int; }"
                `shouldSatisfy` isRight

-- | Checks that a result is a weeding error.
isWeedError :: Either TestError a -> Bool
isWeedError (Left (Weed _)) = True
isWeedError _ = False

oneStmt :: Parser SrcAnnStatement
oneStmt = do
    s <- stmt
    pure $ head s

