module Main where

import Language.GoLite
import Language.GoLite.Pretty

import Text.Show.Pretty

parseOnly :: Parser a -> String -> String -> Either ParseError a
parseOnly m = parse (m <* lexeme eof)

main :: IO ()
main = do
    stdin <- getContents
    let ex = parseOnly (expr >>= unSemiP) "stdin" stdin
    putStrLn $ case ex of
        Left e -> ppShow e
        Right r -> concat
            [ pretty r
            , "\n"
            , ppShow r
            ]
