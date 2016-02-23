module Language.GoLite.Parser.SimpleStmts (
  simpleStmt
, shortVarDeclP
, assignStmt
, incDecStmt
, exprStmt
) where

import Language.GoLite.Parser.Core

-- | Parses a simple statement. In some contexts (such as the initializer for
-- \"if\" and \"switch\" statements), only simple statements are allowed.
simpleStmt :: Parser (Semi SrcAnnStatement)
simpleStmt
    = assignStmt
    <|> shortVarDeclP
    <|> exprStmt

-- | Parses a short variable declaration: a list of identifiers followed by the
-- operator \":=\", then by a list of expressions.
shortVarDeclP :: Parser (Semi SrcAnnStatement)
shortVarDeclP = do
    ids@(i1:_) <- try $
        (lexeme identifier >>= noSemiP)
        `sepBy1`
        comma <* shortVarDeclarator

    exprs <- semiTerminatedCommaList expr

    -- just for the purpose of getting the last expression in the list for
    -- constructing the source span
    exprs_ <- unSemiP exprs
    el <- case exprs_ of
        [] -> failure [Expected "expression list"]
        xs -> pure $ last xs

    pure $ do
        exprs' <- exprs

        let a = SrcSpan (srcStart (ann i1)) (srcEnd (topAnn el))

        pure $ Fix $ Ann a $ ShortVarDecl ids exprs'

-- | Parses an assignment statement: a list of expressions followed by an
-- assignment operator (\"=\", \"+=\", etc.) and a list of expressions.
--
-- TODO: Only certain kinds of expressions are allowed on the left-hand side of
-- the assignment operator (namely, addressable expressions). We need to check
-- that this is the case and raise an error otherwise.
assignStmt :: Parser (Semi SrcAnnStatement)
assignStmt = try (incDecStmt (fmap (fmap (const ())) opIncrement) PlusEq)
    <|>  try (incDecStmt (fmap (fmap (const ())) opDecrement) MinusEq)
    <|>  do
            (al, lhs, op) <- try $ do
                (Ann al lhs) <- withSrcAnnF $ (expr >>= noSemiP) `sepBy1` comma
                op <- withSrcAnnF $ opAssign >>= noSemiP
                pure (al, lhs, op)

            (Ann ar rhs) <- withSrcAnnF $ semiTerminatedCommaList expr

            let a = SrcSpan (srcStart al) (srcEnd ar)

            pure $ do
                rhs' <- rhs

                pure $ Fix $ Ann a $ Assignment lhs op rhs'

-- | Parses an increment or decrement statement (\"x++\", \"y--\"). This is
-- parsed to the same representation as \"x += 1\" or \"y -= 1\".
incDecStmt :: Parser (Semi ()) -> AssignOp () -> Parser (Semi SrcAnnStatement)
incDecStmt opParse op = do
    e <- expr

    (Ann opSpan parsedOp) <- withSrcAnnF opParse

    pure $ do
        e' <- e
        noSemi

        -- We need to do this to ensure that the Semi state will be propagated
        -- despite our artificial construction below.
        parsedOp

        let a = SrcSpan (srcStart (topAnn e')) (srcEnd opSpan)

        -- Annotate the artificial operator and constant with the position of
        -- of the IncDec operator. This matches golang behavior.
        pure $ Fix $ Ann a $
            Assignment
                [e']
                (Ann opSpan op)
                [Fix $ Ann opSpan $ Literal (Ann opSpan $ IntLit 1)]


-- | Parses an expression as a statement.
--
-- TODO: Go only allows certain kinds of expressions to act as statements. We
-- need to introduce a check that causes invalid expressions to raise errors.
exprStmt :: Parser (Semi SrcAnnStatement)
exprStmt = do
    e <- expr
    pure $ do
        e' <- e

        let a = topAnn e'

        pure $ Fix $ Ann a $ ExprStmt e'
