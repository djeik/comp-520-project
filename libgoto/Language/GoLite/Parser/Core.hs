module Language.GoLite.Parser.Core (
  semiList
, semiTerminatedCommaList
, module Language.GoLite.Lexer
, module Language.GoLite.Syntax
, module Language.GoLite.Parser.Expression
) where

import Language.GoLite.Lexer
import Language.GoLite.Syntax
import Language.GoLite.Parser.Expression

-- | Parses one or more instances of `p`, separated by commas, requiring a
-- semicolon on the last instance, but no semicolon on any other instance.
semiTerminatedCommaList :: Parser (Semi a) -> Parser (Semi [a])
semiTerminatedCommaList p = semiList (p `sepBy` comma) noSemi requireSemi

-- | Transforms a parser producing a list of Semi elements into a parser
-- producing a Semi list of elements, with potentially different semicolon
-- checks for the last element versus the rest of the list.
semiList :: Parser ([Semi a]) -> Semi () -> Semi () -> Parser (Semi [a])
semiList p internal end = do
    s <- p
    pure $ foldr (\cur acc -> do
                    acc' <- acc
                    cur' <- cur
                    case acc' of
                            [] -> end
                            _ -> internal
                    pure $ cur':acc') (pure []) s