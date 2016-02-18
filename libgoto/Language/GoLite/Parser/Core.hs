module Language.GoLite.Parser.Core (
  specList
, semiTerminatedCommaList
, module Language.GoLite.Lexer
, module Language.GoLite.Syntax.Types
, module Language.GoLite.Parser.Expression
) where

import Language.GoLite.Lexer
import Language.GoLite.Syntax.Types
import Language.GoLite.Parser.Expression

-- | Parses a specification list. The list is enclosed in parentheses. The
-- elements of the lists themselves must end with a semicolon, except the last
-- element which may or may not have a semicolon.
specList :: Parser ([Semi a]) -> Parser (Semi [a])
specList p = parens (semiList p requireSemi -- Semicolon at each internal spec
                (pure ())
                >>= unSemiP)  -- For the last one, we don't care:
                                        -- var (x = 2) or var (x = 2;) is OK.

-- | Parses one or more instances of `p`, separated by commas, requiring a
-- semicolon on the last instance, but no semicolon on any other instance.
semiTerminatedCommaList :: Parser (Semi a) -> Parser (Semi [a])
semiTerminatedCommaList p = semiList (p `sepBy` comma) noSemi requireSemi

