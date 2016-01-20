module Language.GoLite.Parser
( module Language.GoLite.Parser.Expression
) where

import Language.GoLite.Parser.Expression
import Language.GoLite.Syntax
import Language.GoLite.Lexer

import Text.Megaparsec
import Text.Megaparsec.String
import Control.Monad (void)


stmt :: Parser Statement
stmt = do
    checkSemicolon
    -- Actual statement parsing here...
    optional $ char ';'
    return BreakStmt -- Placeholder

-- | Verifies that a statement is properly terminated
--
-- According to the golang spec, terminating semicolons may be omitted when a
-- line's final token is of a certain kind. This parser checks that either a
-- semicolon is present or the line ends with the proper kind of token.
checkSemicolon :: Parser ()
checkSemicolon = (lookAhead . void) $
            try (manyTill (hidden anyChar) (char ';'))
        <|> try (manyTill (hidden anyChar) (try $ allowsInsertion >> eventuallyEol))

-- | Helper; succeeds when there is only whitespace left on this line.
eventuallyEol :: Parser ()
eventuallyEol = hidden $ void $ manyTill spaceChar (void eol <|> eof)

-- | Helper; succeeds when encountering a token after which a semicolon may
--   be inserted.
allowsInsertion :: Parser ()
allowsInsertion = hidden $ void (try literal)
                <|> void (tryAll ["break", "continue", "fallthrough",
                                    "return", ")", "]", "}"])
                <|> void identifier
