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
decimalLiteral :: Parser Int
decimalLiteral = label "decimal integer literal" $ d1 <|> d2
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
octalLiteral :: Parser Int
octalLiteral = label "octal integer literal" $ do
    char '0'
    t <- many octDigitChar
    return $ read ("0o0" ++ t)

-- | Parses a hexadecimal integer literal.
--
-- A hexadecimal integer literal begins with either `0x` or `0X` and continues
-- with at least one of digits `0` through `f` (case-insensitive)
hexLiteral :: Parser Int
hexLiteral = label "hexadecimal integer literal" $ do
    try (symbol "0x") <|> symbol "0X"
    t <- some hexDigitChar
    return $ read ("0x" ++ t)

-- | Parses an integer literal by trying "decimalLiteral", "octalLiteral", and
-- "hexLiteral".
integerLiteral :: Parser Int
integerLiteral
    = label "integer literal"
    $ decimalLiteral <|> octalLiteral <|> hexLiteral

-- | Parses a floating point literal.
floatLiteral :: Parser Double
floatLiteral = label "float literal" (d1 <|> d2)
        where
            d1 = do i <- some digitChar
                    char '.'
                    d <- many digitChar
                    return $ read (i ++ "." ++ d ++ "0")
            d2 = do char '.'
                    d <- some digitChar
                    return $ read ("0." ++ d)

-- | Requires a parse of a given character around a provided arbitrary parser.
surroundingWith :: Char -> Parser a -> Parser a
surroundingWith c = between (char c) (char c)

-- | Tries to parse any of the given strings as symbols.
tryAll :: [String] -> Parser String
tryAll xs  = foldr ((<|>) . try . symbol) (symbol $ head xs) (tail xs)

-- TODO Re-arrange the escape support

commonEscapes :: [String]
commonEscapes = ["\\a", "\\b", "\\f", "\\n", "\\r", "\\t", "\\v", "\\\\"]

escapesRunes :: Parser String
escapesRunes = tryAll ("\\'":commonEscapes)

escapesStrings :: Parser String
escapesStrings = tryAll ("\\\"":commonEscapes)

runeLiteral :: Parser Char
runeLiteral
    = label "rune literal"
    $ surroundingWith '\''  (L.charLiteral <|> noneOf "\n'")

rawStringLiteral :: Parser String
rawStringLiteral
    = label "raw string literal"
    $ surroundingWith '`' (many $ optional (char '\r') >> noneOf "`")

-- TODO Should disallow \n in a string
interpStringLiteral :: Parser String
interpStringLiteral
    = label "interpreted string literal"
    $ char '"' >> manyTill L.charLiteral (char '"')

stringLiteral :: Parser String
stringLiteral
    = label "string literal"
    $ interpStringLiteral <|> rawStringLiteral

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
        <$> (symbol "[" *> lexeme integerLiteral <* symbol "]")
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
        [ fmap IntLit integerLiteral
        , fmap FloatLit floatLiteral
        , fmap RuneLit runeLiteral
        , fmap StringLit stringLiteral
        ]
