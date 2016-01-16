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
octal_lit = fmap (unDigits 8 . map (\x -> ord x - ord '0')) (char '0' >> many octDigitChar)

int_lit :: Parser Int
int_lit = decimal_lit <|> octal_lit
