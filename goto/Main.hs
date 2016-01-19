module Main where

import Language.GoLite

import Text.Megaparsec
import Text.Megaparsec.String
import Text.Show.Pretty

parseOnly :: Parser a -> String -> String -> Either ParseError a
parseOnly m = parse (m <* eof)

main :: IO ()
main = do
    stdin <- getContents
    putStrLn $ case parseOnly expr "stdin" stdin of
        Left e -> ppShow e
        Right r -> ppShow r
