module Language.GoLite.Lexer.Core
( sc
, symbol
, symbol_
, lexeme
, ($>)
  -- * Reexports for convenience
, module Text.Megaparsec
, module Text.Megaparsec.String
) where

import Control.Monad ( void )

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

-- | The \"fmap const\" operator replaces the contents of a "Functor" with a
-- given value.
($>) :: Functor f => f a -> b -> f b
f $> c = fmap (const c) f

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
