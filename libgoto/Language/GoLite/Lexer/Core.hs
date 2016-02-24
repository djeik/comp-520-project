{-|
Module      : Language.GoLite.Lexer.Core
Description : Basic lexer combinators
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Defines the basic lexeme and symbol lexers used throughout the lexer and
parser. Megaparsec modules are reexported to avoid needing to import them in
all lexer and parser modules.
-}

module Language.GoLite.Lexer.Core
( sc
, symbol
, symbol_
, symbol__
, lexeme
, ($>)
, HasNewline
  -- * Reexports for convenience
, module Text.Megaparsec
, module Text.Megaparsec.String
) where

import Control.Monad ( void )

import Data.Maybe ( isJust )
import Text.Megaparsec
import Text.Megaparsec.String

-- | Track the presence of newlines with a boolean.
type HasNewline = Bool

-- | The \"fmap const\" operator replaces the contents of a "Functor" with a
-- given value.
($>) :: Functor f => f a -> b -> f b
f $> c = fmap (const c) f

-- | Consumes whitespace if any and performs a newline detection.
sc :: Parser HasNewline
sc = hidden $ (||) <$> (any id <$> many (choice [ch, lc, bc])) <*> (isJust <$> optional eof) where
    ch = do
        c <- spaceChar
        pure $ case c of
            '\n' -> True
            _ -> False
    lc = (try $ string "//") *> manyTill anyChar newline *> pure True
    bc = (try $ string "/*") *> (any ('\n' ==) <$> manyTill anyChar (try $ string "*/"))

-- | Parses a verbatim string, allowing for potential whitespace after and
-- performing a newline detection.
--
-- This parser can be backtracked out of until the whole string is parsed.
--
-- The string is returned as-is by the parser.
symbol :: String -> Parser (HasNewline, String)
symbol s = do
    try (string s)
    n <- sc
    pure (n, s)

-- | Parses a verbatim string, allowing for potential whitespace before.
--
-- This is a variant of "symbol" that returns only the 'NewlineStatus'.
symbol_ :: String -> Parser HasNewline
symbol_ = fmap fst . symbol

-- | Parses a verbatim string, allowing for potential whitespace after. The
-- result is thrown away.
symbol__ :: String -> Parser ()
symbol__ = void . symbol

-- | Wraps a parser with trailing whitespace and comment handling to make it
-- properly act as a parser for a lexeme in the language.
--
-- The trailing whitespace handling is done by 'sc' which also performs newline
-- detection.
--
-- The result of the supplied parser is returned alongside the determined
-- 'NewlineStatus'.
lexeme :: Parser a -> Parser (HasNewline, a)
lexeme p = try $ do
    x <- p
    s <- sc
    pure (s, x)
