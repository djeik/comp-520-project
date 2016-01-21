module Language.GoLite.Parser
( module Language.GoLite.Parser.Expression
) where

import Language.GoLite.Parser.Expression
import Language.GoLite.Syntax
import Language.GoLite.Lexer

import Text.Megaparsec
import Text.Megaparsec.String
import Control.Monad (void)
import Control.Applicative


statement :: Parser Statement
statement = do
    str <- upToStmtTermination
    result <- parse stmt' "" str
    case result of
        Right stmt  -> return stmt
        Left err    -> -- TODO if it's an eof, complain about semicolon...

-- | Parses a potential statement until its natural termination.
--
-- A statement is usually terminated by a semicolon. According to the golang
-- spec, terminating semicolons may be omitted when a line's final token is of a
-- certain kind. This parser accepts characters until either reaching a
-- semicolon or a token of the proper kind followed by a new line. Examples:
--
-- `x + x;`  -> `x + x` (semicolons are discarded)
-- `x + x`   -> `x + x` (semicolons are optional)
-- `x + x\n` -> `x + x` (whitespace at the end of a line is discarded)
-- `x + ;x`  -> `x + ` (parsing stops immediately when a semicolon is seen)
-- `x +\nx`  -> `x + \nx` (`+` doesn't trigger semicolon insertion)
-- `x\n+ x`  -> `x` (`x` is an identifier, which triggers semicolon insertion)
upToStmtTermination :: Parser String
upToStmtTermination = manyIncluding (hidden anyChar)
            ((string ";" >> return "") -- Consume and discard semicolon
         <|> (try $ allowsInsertion <* eventuallyEol))

-- | Consumes whitespace until reaching the end of line/file.
eventuallyEol :: Parser ()
eventuallyEol = hidden $ void $ manyTill spaceChar (void eol <|> eof)

-- | Succeeds when encountering a token after which a semicolon may be inserted.
-- TODO add a branch for literals
allowsInsertion :: Parser String
allowsInsertion = (tryAll ["break", "continue", "fallthrough",
                                    "return", ")", "]", "}"])
                <|> identifier

-- | Same as @manyTill@, but also returns the result of the @end@ parser.
-- This explains why the type of the @end@ parser must be [a].
--
-- The code for this (and @someIncluding@) is largely inspired by the code for
-- Megaparsec's @manyTill@.
manyIncluding :: Alternative m => m a -> m [a] -> m[a]
manyIncluding p end = end <|> someIncluding p end

-- | Same as @someTill@, but also returns the result of the @end@ parser.
someIncluding :: Alternative m => m a -> m [a] -> m[a]
someIncluding p end = (:) <$> p <*> (manyIncluding p end)