module Language.GoLite.Lexer where

import Language.GoLite.Syntax

import Control.Monad (void)
import Data.String ( IsString, fromString )
import qualified Data.Map.Strict as Map
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
-- with digits `0` through `9`. Notice that as such that `0`, `00`, etc. are not
-- decimal integer literals, but rather octal integer literals.
decimalLiteral :: Parser Int
decimalLiteral = label "decimal integer literal" $ do
                    h <- oneOf "123456789"
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
--
-- A floating point literal has an integral and decimal part, at least one of
-- which must be present. The two parts are separated by a mandatory dot, and
-- may only contain decimal digits, including 0. Therefore, `0.0`, `0.`, and
-- `.0` are all valid floating point literals. `.` is invalid.
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

-- | List of escape codes that are valid in intepreted strings and runes.
commonEscapes :: [Char]
commonEscapes = "abfnrtv\\"

-- | Map of escape characters to their escaped value (e.g. 'a' maps to '\a')
escapedChars :: Map.Map Char Char
escapedChars = Map.fromList [('a', '\a'), ('b', '\b'), ('f', '\f'), ('n', '\n'),
                             ('r', '\r'), ('t', '\t'), ('v', '\v'), ('"', '\"'),
                             ('\'', '\''), ('\\', '\\')]

-- | Parses an escape code, succeeding when it is part of the provided list.
-- The escaped value is returned (e.g. parsing the string `\n` results in a
-- single newline character being returned).
--
-- An escape code is a backslash (`\`) followed by a character. The argument
-- `codes` provides a list of valid escape characters.
escapeCode :: [Char] -> Parser Char
escapeCode codes = label "escape code" $ do
                    char '\\'
                    code <- oneOf codes
                    return $ escapedChars Map.! code

-- | Parses a rune literal.
--
-- A rune literal is either a character or a valid escape code enclosed in
-- single quotes. The new line (\n) and single quote (') characters are not
-- allowed inside a rune literal.
runeLiteral :: Parser Char
runeLiteral
    = label "rune literal"
    $ surroundingWith '\'' (escapeCode ('\'':commonEscapes) <|> noneOf "\n'")

-- | Parses a raw string literal
--
-- A raw string literal is surrounded by back-ticks (`) and may contain any
-- character, including new lines. Escape sequences are not interpreted. Any
-- carriage return character (\r) in the string is dropped.
rawStringLiteral :: Parser String
rawStringLiteral
    = label "raw string literal"
    $ surroundingWith '`' (many $ optional (char '\r') >> noneOf "`")

-- | Parses an interpreted string literal
--
-- An interpreted string literal is surrounded by double quotes ("). Escape
-- sequences are interpreted. The new line character (\n) is not allowed inside
-- an interpreted string literal.
interpStringLiteral :: Parser String
interpStringLiteral
    = label "interpreted string literal"
    $ surroundingWith '"'
        $ many (escapeCode ('\"':commonEscapes) <|> noneOf "\n\"")

-- | Parses a string literal by trying "interpStringLiteral" and
-- "rawStringLiteral".
stringLiteral :: Parser String
stringLiteral
    = label "string literal"
    $ interpStringLiteral <|> rawStringLiteral

-- | Parses an identifier
-- An identifier starts with an letter, followed by any number of alphanumeric
-- characters. `_` is considered a letter.
identifier :: Parser String
identifier = p <?> "identifier" where
    p = do
        c <- char '_' <|> letterChar
        cs <- many $ char '_' <|> alphaNumChar
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

-- | Tries to parse any of the given strings as symbols.
tryAll :: [String] -> Parser String
tryAll xs  = foldr ((<|>) . try . symbol) (symbol $ head xs) (tail xs)

-- | Requires a parse of a given character around a provided arbitrary parser.
surroundingWith :: Char -> Parser a -> Parser a
surroundingWith c = between (char c) (char c)

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
