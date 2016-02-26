{-# LANGUAGE OverloadedStrings #-}

module Lexer
(
  lexer
) where

import Core

import Lexer.Literal
import Lexer.Type

lexer :: SpecWith ()
lexer = describe "Lexer" $ do
    testDecimalLiteral
    testOctalLiteral
    testHexLiteral
    testIntLiteral
    testFloatLiteral
    testEscapeCode
    testRuneLiteral
    testRawString
    testInterpretedString
    testIdentifier
    testType