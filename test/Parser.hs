module Parser where

import Core

import Parser.Expression
import Parser.Stmt

parser :: SpecWith()
parser = describe "Parser" $ do
    describe "Expression" $ do
        expression
    describe "Statements" $ do
        statement

