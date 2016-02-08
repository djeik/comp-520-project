module Language.GoLite.Lexer.Literal
( -- * Literal lexers
  literal
, identifier
, type_
, structType -- Exported separately because some situations only allow this type
, decimalLiteral
, octalLiteral
, hexLiteral
, integerLiteral
, floatLiteral
, runeLiteral
, rawStringLiteral
, interpStringLiteral
, stringLiteral
  -- ** Escape code handling
, commonEscapes
, escapedChars
, escapeCode
) where

import Language.GoLite.Lexer.Core
import Language.GoLite.Lexer.Semi
import Language.GoLite.Lexer.Symbols
import Language.GoLite.Lexer.Keywords

import Language.GoLite.SrcAnn
import Language.GoLite.Syntax

import qualified Data.Map.Strict as Map
import Data.String ( fromString )

-- | Parses a literal.
literal :: Parser (Semi SrcAnnLiteral)
literal = withDetectSemicolon $ withSrcAnnF $ do
    label "literal" $ choice
        [ fmap FloatLit (try floatLiteral)
        , fmap IntLit integerLiteral
        , fmap RuneLit runeLiteral
        , fmap StringLit stringLiteral
        ]

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
    try (symbol "0x") <|> try (symbol "0X")
    t <- some hexDigitChar
    return $ read ("0x" ++ t)

-- | Parses an integer literal by trying "decimalLiteral", "octalLiteral", and
-- "hexLiteral".
integerLiteral :: Parser Int
integerLiteral
    = label "integer literal"
    $ decimalLiteral <|> hexLiteral <|> octalLiteral

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
                    notFollowedBy identifier
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
identifier :: Parser (Semi SrcAnnIdent)
identifier = p <?> "identifier" where
    p = withDetectSemicolon $ withSrcAnn Ident $ do
        c <- char '_' <|> letterChar
        cs <- many $ char '_' <|> alphaNumChar
        pure $ fromString (c:cs)

-- SrcAnnType
-- = SrcAnnFix SrcAnnTypeF
-- = Fix (Ann SrcSpan SrcAnnTypeF)
--
-- Parser (Semi (f (Fix f))) -> Parser (Semi (Fix f))

-- | Parses a type.
type_ :: Parser (Semi SrcAnnType)
type_
    = label "type"
    $ sliceType <|> arrayType <|> structType <|> namedType where
        sliceType :: Parser (Semi SrcAnnType)
        sliceType = label "slice type" $ withPushSrcAnnFix $ do
            symbol_ "["
            closeBracket >>= noSemiP
            s <- type_
            pure $ SliceType <$> s

        arrayType :: Parser (Semi SrcAnnType)
        arrayType = label "array type" $ withPushSrcAnnFix $ do
            symbol_ "["
            i <- withSrcAnnConst $ lexeme integerLiteral
            closeBracket >>= noSemiP
            s <- type_
            pure $ fmap (ArrayType i) s

        namedType :: Parser (Semi SrcAnnType)
        namedType = label "named type" $ withPushSrcAnnFix $ do
            fmap NamedType <$> lexeme identifier

-- | Parses a struct type, which is the keyword "struct" followed by a list of
-- fields enclosed in braces.
structType :: Parser (Semi SrcAnnType)
structType = label "struct type" $ withPushSrcAnnFix $ do
    kwStruct >>= noSemiP
    symbol_ "{"
    fields <- semiList (many field) requireSemi (pure ())
    closeBrace
    pure $ fmap StructType fields

-- | Parses a field of a struct, which is a non-empty list of identifiers
-- followed by a type.
field :: Parser (Semi ([SrcAnnIdent], SrcAnnType))
field = do
    ids <- (lexeme identifier) `sepBy1` comma
    typ <- type_
    pure $ do
        ids' <- sequenceA ids
        typ' <- typ
        pure $ (ids' , typ')


-- | Requires a parse of a given character around a provided arbitrary parser.
surroundingWith :: Char -> Parser a -> Parser a
surroundingWith c = between (char c) (char c)
