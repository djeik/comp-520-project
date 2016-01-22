{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Language.GoLite.Lexer
Description : Lexer combinators for the GoLite language.
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2013
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental


-}

module Language.GoLite.Lexer
( -- * Basic lexers
  sc
, symbol
, symbol_
, lexeme
, parens
, squareBrackets
  -- * Literal lexers
, literal
, identifier
, type_
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
  -- * Semicolon handling
, Semi() -- abstract !
, SemiError(..)
  -- ** Semi introduction
, withDetectSemicolon
  -- ** Semi elimination
, requireSemi
, requireSemiP
, noSemiP
, noSemi
, unSemi
, unSemiP
  -- * Keywords and symbols
, Keyword
, kwBreak
, kwReturn
, kwContinue
, kwFallthrough
, kwPrint
, kwPrintLn
, kwRead
, opIncrement
, opDecrement
, semicolon
, comma
, closeParen
, closeBracket
, closeBrace
  -- * Reexports for convenience
, module Text.Megaparsec
, module Text.Megaparsec.String
) where

import Language.GoLite.Syntax

import Control.Monad.State
import Control.Monad.Except
import Data.Maybe ( fromMaybe )
import Data.String ( fromString )
import qualified Data.Map.Strict as Map
import Text.Megaparsec hiding ( State )
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

($>) :: Functor f => f a -> b -> f b
f $> c = fmap (const c) f

type Keyword = String

data SemiError
    = UnexpectedSemicolon
    | ExpectedSemicolon
    | NoSemicolonDetection

-- | Represent explicit or implicit semicolons.
--
-- Parsers that parse values wrapped in "Semi" will detect implicit or explicit
-- semicolons. They do so with the `withDetectSemicolon` combinator.
newtype Semi a
    = Semi { runSemi :: StateT (Maybe Bool) (Except SemiError) a }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState (Maybe Bool)
        , MonadError SemiError
        )

-- | Runs a computation in the "Semi" monad, requiring that there be a
-- semicolon. Exceptions from the "Semi" monad are reinterpreted as parse
-- errors, and the presence of a semicolon will cause an \"expected ; or
-- newline\" parser failure.
requireSemiP :: Semi a -> Parser a
requireSemiP s = case runExcept $ runStateT (runSemi s) Nothing of
    Left UnexpectedSemicolon ->
        failure [Unexpected ";", Unexpected "newline"]

    Left ExpectedSemicolon ->
        failure [Expected ";", Expected "newline"]

    Left NoSemicolonDetection ->
        failure [Message "No semicolon detection performed!"]

    Right e -> case e of
        (_, Nothing) -> failure [Message "No semicolon detection performed!"]
        (x, Just True) -> pure x
        (_, Just False) -> failure [Expected ";", Expected "newline"]

-- | Analyzes the current state of the "Semi" monad and throws errors if no
-- semicolon is present.
requireSemi :: Semi ()
requireSemi = do
    isSemiM <- get
    case isSemiM of
        Nothing -> throwError NoSemicolonDetection
        Just True -> pure ()
        Just False -> throwError ExpectedSemicolon

-- | Runs a computation in the "Semi" monad, requiring that there be no
-- semicolon. Exceptions from the "Semi" monad are reinterpreted as parse
-- errors, and the presence of a semicolon will cause an \"unexpected ; or
-- newline\" parser failure.
noSemiP :: Semi a -> Parser a
noSemiP s = case runExcept $ runStateT (runSemi s) Nothing of
    Left UnexpectedSemicolon ->
        failure [Unexpected ";", Unexpected "newline"]

    Left ExpectedSemicolon ->
        failure [Expected ";", Expected "newline"]

    Left NoSemicolonDetection ->
        failure [Message "No semicolon detection performed!"]

    Right e -> case e of
        (_, Nothing) -> failure [Message "No semicolon detection performed!"]
        (x, Just False) -> pure x
        (_, Just True) -> unexpected "ohhh nooo ;"

-- | Analyzes the current state of the "Semi" monad and throws errors if no
-- semicolon detection has been performed or if there is a semicolon present.
noSemi :: Semi ()
noSemi = do
    isSemiM <- get
    case isSemiM of
        Nothing -> throwError NoSemicolonDetection
        Just True -> throwError UnexpectedSemicolon
        Just False -> pure ()

-- | Runs a computation in the "Semi" monad, extracting the parse result or the
-- error if any.
unSemi :: Semi a -> Either SemiError a
unSemi s = runExcept $ evalStateT (runSemi s) Nothing

-- | Runs a computation in the "Semi" monad, returning the parse result in the
-- "Parser" monad or reinterpreting the error as a parse error if any.
unSemiP :: Semi a -> Parser a
unSemiP s = case unSemi s of
    Left UnexpectedSemicolon ->
        failure [Unexpected ";", Unexpected "newline"]

    Left ExpectedSemicolon ->
        failure [Expected ";", Expected "newline"]

    Left NoSemicolonDetection ->
        failure [Message "No semicolon detection performed!"]

    Right x -> pure x

-- | Consumes whitespace until reaching the end of line/file.
eventuallyEol :: Parser ()
eventuallyEol = hidden $ void $ manyTill spaceChar (void eol <|> eof)

-- | Consumes whitespace and comments.
sc :: Parser ()
sc = L.space (void spaceChar) lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"

-- | Parses a verbatim string, allowing for potential whitespace before.
--
-- The string is also as-is by the parser.
symbol :: String -> Parser String
symbol s = do
    try $ do
        sc
        string s
    pure s

-- | Parses a verbatim string, allowing for potential whitespace before.
--
-- This is a variant of "symbol" that does not return the parsed string.
symbol_ :: String -> Parser ()
symbol_ = void . symbol

-- | Wraps a parser with leading whitespace and comment handling to make it
-- properly act as a parser for a lexeme in the language.
--
-- The result of the supplied parser is returned.
lexeme :: Parser a -> Parser a
lexeme p = try $ do
    sc
    p

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

whichMaybe :: Maybe a -> Bool
whichMaybe = fromMaybe False . fmap (const True)

detectSemicolon :: Parser Bool
detectSemicolon = whichMaybe <$> optional (semicolon <|> try eventuallyEol)

withDetectSemicolon :: Parser a -> Parser (Semi a)
withDetectSemicolon p = do
    q <- p
    t <- detectSemicolon
    pure $ do
        put (Just t)
        pure q

-- | Parses an identifier
-- An identifier starts with an letter, followed by any number of alphanumeric
-- characters. `_` is considered a letter.
identifier :: Parser (Semi String)
identifier = p <?> "identifier" where
    p = withDetectSemicolon $ do
        c <- char '_' <|> letterChar
        cs <- many $ char '_' <|> alphaNumChar
        pure $ fromString (c:cs)

type_ :: Parser (Semi Type)
type_ = label "type" $ sliceType <|> arrayType <|> namedType where
    sliceType = label "slice type" $ do
        symbol_ "["
        closeBracket >>= noSemiP
        s <- type_
        pure $ fmap SliceType s

    arrayType :: Parser (Semi Type)
    arrayType = label "array type" $ do
        symbol_ "["
        i <- lexeme integerLiteral
        closeBracket >>= noSemiP
        s <- type_
        pure $ fmap (ArrayType i) s

    namedType = label "named type" $ do
        fmap NamedType <$> lexeme identifier

-- | Runs a given parser requiring that it be surrounded by matched parentheses
-- "symbol_"s.
parens :: Parser a -> Parser (Semi a)
parens p = do
    symbol_ "("
    q <- p
    s <- closeParen
    pure (s $> q)

-- | Runs a given parses requiring that it be surrounded by matched square
-- bracket "symbol_"s.
squareBrackets :: Parser a -> Parser (Semi a)
squareBrackets p = do
    symbol_ "["
    q <- p
    s <- closeBracket
    pure (s $> q)

-- | Requires a parse of a given character around a provided arbitrary parser.
surroundingWith :: Char -> Parser a -> Parser a
surroundingWith c = between (char c) (char c)

-- | Parses a literal.
literal :: Parser (Semi Literal)
literal = withDetectSemicolon $ do
    label "literal" $ choice
        [ fmap IntLit integerLiteral
        , fmap FloatLit floatLiteral
        , fmap RuneLit runeLiteral
        , fmap StringLit stringLiteral
        ]

-- | Parses a semicolon symbol \";\".
semicolon :: Parser ()
semicolon = symbol_ ";"

comma :: Parser () -- | Parses a comma symbol \",\".
comma = symbol_ ","

semisym :: String -> Parser (Semi Keyword)
semisym = withDetectSemicolon . symbol

-- | Parses the \"break\" keyword, checking for a semicolon.
kwBreak :: Parser (Semi Keyword)
kwBreak = semisym "break"

-- | Parses the \"return\" keyword, checking for a semicolon.
kwReturn :: Parser (Semi Keyword)
kwReturn = semisym "return"

-- | Parses the \"continue\" keyword, checking for a semicolon.
kwContinue :: Parser (Semi Keyword)
kwContinue = semisym "continue"

-- | Parses the \"fallthrough\" keyword, checking for a semicolon.
kwFallthrough :: Parser (Semi Keyword)
kwFallthrough = semisym "fallthrough"

-- | Parses the \"print\" keyword, checking for a semicolon.
kwPrint :: Parser (Semi Keyword)
kwPrint = semisym "print"

-- | Parses the \"println\" keyword, checking for a semicolon.
kwPrintLn :: Parser (Semi Keyword)
kwPrintLn = semisym "println"

-- | Parses the \"read\" keyword, checking for a semicolon.
kwRead :: Parser (Semi Keyword)
kwRead = semisym "read"

-- | Parses the increment symbol \"++\", checking for a semicolon.
opIncrement :: Parser (Semi String)
opIncrement = semisym "++"

-- | Parses the decrement symbol \"--\", checking for a semicolon.
opDecrement :: Parser (Semi String)
opDecrement = semisym "--"

-- | Parses a closing parenthesis \")\", checking for a semicolon.
closeParen :: Parser (Semi String)
closeParen = semisym ")"

-- | Parses a closing bracket \"]\", checking for a semicolon.
closeBracket :: Parser (Semi String)
closeBracket = semisym "]"

-- | Parses a closing brace \"}\", checking for a semicolon.
closeBrace :: Parser (Semi String)
closeBrace = semisym "}"
