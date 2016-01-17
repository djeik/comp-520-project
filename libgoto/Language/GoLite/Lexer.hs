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

-- | Parses a verbatim string, allowing for potential whitespace after.
--
-- The string is also as-is by the parser.
symbol :: String -> Parser String
symbol = L.symbol sc

-- | Parses a verbatim string, allowing for potential whitespace after.
--
-- This is a variant of "symbol" that does not return the parsed string.
symbol_ :: String -> Parser ()
symbol_ = void . symbol

-- | Wraps a parser with trailing whitespace and comment handling to make it
-- properly act as a parser for a lexeme in the language.
--
-- The result of the supplied parser is returned.
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parses a decimal integer literal.
--
-- A decimal integer literal begins with one of `1` through `9` and continues
-- with digits `0` through `9`. Notice that as such that `00` is not a decimal
-- integer literal, but rather an octal integer literal.
decimal_lit :: Parser Int
decimal_lit = label "decimal integer literal" $ d1 <|> d2
        where
            d1 = do char '0'
                    lookAhead spaceChar
                    return 0
            d2 = do h <- oneOf "123456789"
                    t <- many digitChar
                    return $ read (h:t)

-- | Parses an octal integer literal.
--
-- An octal integer literal begins with `0` and continues with digits `0`
-- through `7`.
octal_lit :: Parser Int
octal_lit = label "octal integer literal" $ do
    char '0'
    t <- many octDigitChar
    return $ read ("0o0" ++ t)

-- | Parses a hexadecimal integer literal.
--
-- A hexadecimal integer literal begins with either `0x` or `0X` and continues
-- with digits `0` through `f`. The case of the letter-digits is ignored.
hex_lit :: Parser Int
hex_lit = label "hexadecimal integer literal" $ do
    try (symbol "0x") <|> symbol "0X"
    t <- some hexDigitChar
    return $ read ("0x" ++ t)

-- | Parses an integer literal by trying "decimal_lit", "octal_lit", and
-- "hex_lit".
int_lit :: Parser Int
int_lit
    = label "integer literal"
    $ decimal_lit <|> octal_lit <|> hex_lit

-- | Parses a floating point literal.
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

-- | Requires a parse of a given character around a provided arbitrary parser.
surroundingWith :: Char -> Parser a -> Parser a
surroundingWith c = between (char c) (char c)

-- | Tries to parse any of the given strings as symbols.
try_all :: [String] -> Parser String
try_all xs  = foldr ((<|>) . try . symbol) (symbol $ head xs) (tail xs)

-- TODO Re-arrange the escape support

common_escapes :: [String]
common_escapes = ["\\a", "\\b", "\\f", "\\n", "\\r", "\\t", "\\v", "\\\\"]

escape_runes :: Parser String
escape_runes = try_all ("\\'":common_escapes)

escape_string :: Parser String
escape_string = try_all ("\\\"":common_escapes)

rune_lit :: Parser Char
rune_lit
    = label "rune literal"
    $ surroundingWith '\''  (L.charLiteral <|> noneOf "\n'")

raw_string :: Parser String
raw_string
    = label "raw string literal"
    $ surroundingWith '`' (many $ optional (char '\r') >> noneOf "`")

-- TODO Should disallow \n in a string
interp_string :: Parser String
interp_string
    = label "interpreted string literal"
    $ char '"' >> manyTill L.charLiteral (char '"')

string_lit :: Parser String
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

-- | Runs a given parser requiring that it be surrounded by matched parentheses
-- "symbol_"s.
parens :: Parser a -> Parser a
parens = between (symbol_ "(") (symbol_ ")")

-- | Runs a given parses requiring that it be surrounded by matched square
-- bracket "symbol_"s.
squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol_ "[") (symbol_ "]")

-- | Parses a literal.
literal :: Parser Literal
literal
    = label "literal"
    $ choice
        [ fmap IntLit int_lit
        , fmap FloatLit float_lit
        , fmap RuneLit rune_lit
        , fmap StringLit string_lit
        ]
