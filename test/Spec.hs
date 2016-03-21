module Main where

import Test.Hspec

import Lexer
import Parser
import Weeder

main :: IO ()
main = hspec goLite

goLite :: SpecWith ()
goLite = describe "Language.GoLite" $ do
    lexer
    parser
    weeder