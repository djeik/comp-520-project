module Language.GoLite.Parser.SimpleStmts (
  simpleStmt
) where

import Language.GoLite.Parser.Core

-- | Parses a simple statement. In some contexts (such as the initializer for
-- \"if\" and \"switch\" statements), only simple statements are allowed.
simpleStmt :: Parser (Semi Statement)
simpleStmt
    = try assignStmt
    <|> shortVarDecl
    <|> exprStmt

-- | Parses a short variable declaration: a list of identifiers followed by the
-- operator \":=\", then by a list of expressions.
shortVarDecl :: Parser (Semi Statement)
shortVarDecl = do
    ids <- (lexeme identifier >>= noSemiP) `sepBy1` comma <* shortVarDeclarator
    exprs <- semiTerminatedCommaList expr
    pure $ do
        exprs' <- exprs
        pure $ ShortVarDecl ids exprs'

-- | Parses an assignment statement: a list of expressions followed by an
-- assignment operator (\"=\", \"+=\", etc.) and a list of expressions.
--
-- TODO: Only certain kinds of expressions are allowed on the left-hand side of
-- the assignment operator (namely, addressable expressions). We need to check
-- that this is the case and raise an error otherwise.
assignStmt :: Parser (Semi Statement)
assignStmt = try (incDecStmt opIncrement PlusEq)
    <|>  try (incDecStmt opDecrement MinusEq)
    <|>  do
            lhs <- (expr >>= noSemiP) `sepBy1` comma
            op <- opAssign >>= noSemiP
            rhs <- semiTerminatedCommaList expr
            pure $ do
                rhs' <- rhs
                pure $ Assignment lhs op rhs'

-- | Parses an increment or decrement statement (\"x++\", \"y--\"). This is
-- parsed to the same representation as \"x += 1\" or \"y -= 1\".
incDecStmt :: Parser a -> AssignOp -> Parser (Semi Statement)
incDecStmt opParse op = do
    e <- expr
    opParse
    pure $ do
        e' <- e
        noSemi
        pure $ Assignment [e'] op [Literal (IntLit 1)]


-- | Parses an expression as a statement.
--
-- TODO: Go only allows certain kinds of expressions to act as statements. We
-- need to introduce a check that causes invalid expressions to raise errors.
exprStmt :: Parser (Semi Statement)
exprStmt = do
    e <- expr
    pure (ExprStmt <$> e)
