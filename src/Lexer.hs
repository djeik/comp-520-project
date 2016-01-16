module Lexer where

import Text.Megaparsec
import Text.Megaparsec.String -- Parsing a string
import Control.Monad (void)
import Data.Digits
import Data.Char (ord)

import qualified Text.Megaparsec.Lexer as L

sc :: Parser ()
sc = L.space (void spaceChar) lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

symbol = L.symbol sc
lexeme = L.lexeme sc
        
kw :: Parser String
kw =    symbol "goto"
    <|> symbol "return" -- ...
    
decimal_lit :: Parser Int
decimal_lit = d1 <|> d2 
        where 
            d1 = do char '0'
                    lookAhead spaceChar
                    return 0
            d2 = do h <- oneOf "123456789" 
                    t <- many digitChar
                    return $ read (h:t)
                    
octal_lit :: Parser Int
octal_lit = do char '0'
               t <- many octDigitChar
               return $ read ("0o0" ++ t)

hex_lit :: Parser Int
hex_lit = do try (symbol "0x") <|> symbol "0X"
             t <- some hexDigitChar
             return $ read ("0x" ++ t)

int_lit :: Parser Int
int_lit = decimal_lit <|> octal_lit <|> hex_lit
