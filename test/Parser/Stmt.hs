{-|
Module      : Stmt
Description : Tests for statements
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings #-}

module Parser.Stmt
( statement
) where

import Language.GoLite
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Syntax.Sugar as Sugar

import Core

import Parser.For
import Parser.If
import Parser.Switch
import Parser.StmtDecl
import Parser.Simple

-- | Used for a specific test with blocks.
varDeclStmt :: [id] -> Maybe ty -> [e]
            -> Fix (StatementF (Declaration tyDecl (VarDecl id ty e)) ex i a c)
varDeclStmt i t e = Fix $ DeclStmt $ VarDecl $ VarDeclBody i t e

statement :: SpecWith ()
statement = do
                describe "assignStmt" assign
                describe "shortVarDecl" shortVariableDeclaration
                describe "exprStmt" expressionStatement
                describe "simpleStmt" simpleStatement
                describe "varDecl" variableDeclaration
                describe "typeDecl" typeDeclaration
                describe "break/continue/fallthrough" simpleKeywordStmts
                describe "blockStmt" blockStatement
                describe "switchStmt" switchStatement
                describe "forStmt" forStatement
                describe "ifStmt" ifStatement
                describe "returnStmt" returnStatement
                describe "printStmt" printStatement

simpleKeywordStmts :: SpecWith ()
simpleKeywordStmts = do
    let parseStmt = parseOnly (fmap (map bareStmt) stmt)
    it "parses the keywords `break`, `continue` and `fallthrough`" $ do
        parseStmt "break" `shouldBe` r [breakStmt]
        parseStmt "continue" `shouldBe` r [continueStmt]

        -- fallthrough is not supported
        parseStmt "fallthrough" `shouldSatisfy` isLeft

    it "does not parses if the keywords are missing a semi" $ do
        parseStmt "break {}" `shouldSatisfy` isLeft
        parseStmt "continue {}" `shouldSatisfy` isLeft

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

    it "does not parse if there are no or missing braces" $ do
        parseBlock "x++" `shouldSatisfy` isLeft
        parseBlock "{x++" `shouldSatisfy` isLeft
        parseBlock "x++;}" `shouldSatisfy` isLeft

returnStatement :: SpecWith ()
returnStatement = do
    let parseReturn = parseOnly (fmap bareStmt returnStmtP)

    it "parses return statements with or without an expression" $ do
        parseReturn "return" `shouldBe` r (returnStmt Nothing)
        parseReturn "return 3" `shouldBe` r (returnStmt $ Just (int 3))

    it "does not parse return statements with more than one expression" $ do
        parseReturn "return 3, 3" `shouldSatisfy` isLeft

    it "does not parse if there is no `return` keyword" $ do
        parseReturn "3, 3" `shouldSatisfy` isLeft
        parseReturn "3" `shouldSatisfy` isLeft

    it "needs a semi if there is no expr, or no semi if there is an expr" $ do
        parseReturn "return; 3" `shouldSatisfy` isLeft
        parseReturn "return\n 3" `shouldSatisfy` isLeft
        parseReturn "return {}" `shouldSatisfy` isLeft

printStatement :: SpecWith ()
printStatement = do
    let parsePrint = parseOnly (fmap bareStmt printStmtP)

    it "parses print statements with or without expressions" $ do
        parsePrint "print()" `shouldBe`
            r (printStmt [])

        parsePrint "print(3)" `shouldBe`
            r (printStmt [int 3])

        parsePrint "print(3, 4)" `shouldBe`
            r (printStmt [int 3, int 4])

        parsePrint "println()" `shouldBe`
            r (printStmt [stringLit "\n"])

        parsePrint "println(3)" `shouldBe`
            r (printStmt [int 3, stringLit "\n"])

        parsePrint "println(3, 4)" `shouldBe`
            r (printStmt [int 3, int 4, stringLit "\n"])

    it "does not parse if the keyword is missing" $ do
        parsePrint "(3)" `shouldSatisfy` isLeft
        parsePrint "(3, 4)" `shouldSatisfy` isLeft

    it "does not parse if one of the expressions has a semi" $ do
        parsePrint "print(3;)" `shouldSatisfy` isLeft
        parsePrint "print(3;, 4)" `shouldSatisfy` isLeft
        parsePrint "print(4, 3;)" `shouldSatisfy` isLeft
        parsePrint "print(;)" `shouldSatisfy` isLeft
        parsePrint "println(3;)" `shouldSatisfy` isLeft
        parsePrint "println(3;, 4)" `shouldSatisfy` isLeft
        parsePrint "println(4, 3;)" `shouldSatisfy` isLeft
        parsePrint "println(;)" `shouldSatisfy` isLeft

    it "does not parse if the parens are missing/malformed" $ do
        parsePrint "print 3" `shouldSatisfy` isLeft
        parsePrint "print(3" `shouldSatisfy` isLeft
        parsePrint "print 3)" `shouldSatisfy` isLeft
        parsePrint "println 3" `shouldSatisfy` isLeft
        parsePrint "println(3" `shouldSatisfy` isLeft
        parsePrint "println 3)" `shouldSatisfy` isLeft

    it "does not parse if the keyword has an explicit semi" $ do
        parsePrint "print; (3)" `shouldSatisfy` isLeft
        parsePrint "print\n (3)" `shouldSatisfy` isRight
        parsePrint "println; (3)" `shouldSatisfy` isLeft
        parsePrint "println\n (3)" `shouldSatisfy` isRight



