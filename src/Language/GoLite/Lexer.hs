module Language.GoLite.Lexer where

import Language.GoLite.Syntax

import Control.Monad (void)
import Data.String ( IsString, fromString )
import Text.Megaparsec
import Text.Megaparsec.String -- Parsing a string
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
decimal_lit = label "decimal integer literal" $ d1 <|> d2
        where
            d1 = do char '0'
                    lookAhead spaceChar
                    return 0
            d2 = do h <- oneOf "123456789"
                    t <- many digitChar
                    return $ read (h:t)

octal_lit :: Parser Int
octal_lit = label "octal integer literal" $ do
    char '0'
    t <- many octDigitChar
    return $ read ("0o0" ++ t)

hex_lit :: Parser Int
hex_lit = label "hexadecimal integer literal" $ do
    try (symbol "0x") <|> symbol "0X"
    t <- some hexDigitChar
    return $ read ("0x" ++ t)

int_lit :: Parser Int
int_lit
    = label "integer literal"
    $ decimal_lit <|> octal_lit <|> hex_lit

float_lit :: Parser Double
float_lit = label "float literal" (d1 <|> d2)
        where
            d1 = do i <- some digitChar
                    char '.'
                    d <- many digitChar
                    return $ read (i ++ "." ++ d ++ "0")
            d2 = do char '.'
                    d <- many digitChar
                    return $ read ("0." ++ d)


between_c c = between (symbol c) (symbol c)

try_all xs  = foldr ((<|>) . try . symbol) (symbol $ head xs) (tail xs)

-- TODO Re-arrange the escape support

common_escapes = ["\\a", "\\b", "\\f", "\\n", "\\r", "\\t", "\\v", "\\\\"]

escape_runes = try_all ("\\'":common_escapes)
escape_string = try_all ("\\\"":common_escapes)

rune_lit :: Parser Char
rune_lit
    = label "rune literal"
    $ between_c "'"  (L.charLiteral <|> noneOf "\n'")

raw_string
    = label "raw string literal"
    $ between_c "`" (many $ optional (char '\r') >> noneOf "`")

-- TODO Should disallow \n in a string
interp_string :: Parser String
interp_string
    = label "interpreted string literal"
    $ char '"' >> manyTill L.charLiteral (char '"')

string_lit
    = label "string literal"
    $ interp_string <|> raw_string

identifier :: IsString a => Parser a
identifier = p <?> "identifier" where
    p = do
        c <- letterChar
        cs <- many alphaNumChar
        return $ fromString (c:cs)

type_ :: Parser Type
type_ = label "type" $ sliceType <|> arrayType <|> namedType where
    sliceType
        = label "slice type"
        $ SliceType
        <$> (symbol "[" *> symbol "]" *> type_)
    arrayType
        = label "array type"
        $ ArrayType
        <$> (symbol "[" *> lexeme int_lit <* symbol "]")
        <*> type_
    namedType
        = label "named type"
        $ NamedType
        <$> lexeme identifier

parens = between (symbol "(") (symbol ")")
squareBrackets = between (symbol "[") (symbol "]")

literal
    = label "literal"
    $ choice
        [ fmap IntLit int_lit
        , fmap FloatLit float_lit
        , fmap RuneLit rune_lit
        , fmap StringLit string_lit
        ]
