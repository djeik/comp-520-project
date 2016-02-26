{-|
Module      : Language.GoLite.Parser.SimpleStmts
Description : Parsers for simple statements
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

module Language.GoLite.Parser.SimpleStmts (
  simpleStmt
, shortVarDeclP
, assignStmt
, incDecStmt
, exprStmtP
, emptyStmtP
) where

import Language.GoLite.Parser.Core

-- | Parses a simple statement. In some contexts (such as the initializer for
-- \"if\" and \"switch\" statements), only simple statements are allowed.
simpleStmt :: Parser (Semi SrcAnnStatement)
simpleStmt =
        emptyStmtP
    <|> assignStmt
    <|> shortVarDeclP
    <|> exprStmtP


-- | Parses an empty statement. An empty statement is a semicolon by itself.
emptyStmtP :: Parser (Semi SrcAnnStatement)
emptyStmtP = do
    (Ann a _ ) <- withSrcAnnF (symbol ";")
    pure $ semiPresent $ Fix $ Ann a $ EmptyStmt

-- | Parses a short variable declaration: a list of identifiers followed by the
-- operator \":=\", then by a list of expressions.
shortVarDeclP :: Parser (Semi SrcAnnStatement)
shortVarDeclP = do
    ids@(i1:_) <- try $
        (identifier >>= noSemiP)
        `sepBy1`
        comma <* shortVarDeclarator

    -- The try here is okay, since the very next thing we do after this is parse
    -- an expression again, without a try.
    rhs_a <- many (try ((expr >>= noSemiP) <* comma))
    (Ann ar rhs_b) <- withSrcAnnF expr

    -- Both sides must have the same length.
    if length ids == 1 + length rhs_a then pure ()
    else failure [Message "Assignments need the same number of operands on each side"]

    let a = SrcSpan (srcStart (ann i1)) (srcEnd ar)

    pure $ do
        rhs_b' <- rhs_b


        pure $ Fix $ Ann a $ ShortVarDecl ids (rhs_a ++ [rhs_b'])

-- | Parses an assignment statement: a list of expressions followed by an
-- assignment operator (\"=\", \"+=\", etc.) and a list of expressions.
assignStmt :: Parser (Semi SrcAnnStatement)
assignStmt = try (incDecStmt (fmap (fmap (const ())) opIncrement) PlusEq)
    <|> try (incDecStmt (fmap (fmap (const ())) opDecrement) MinusEq)
    <|> do
            (al, lhs, op) <- try $ do
                (Ann al lhs) <- withSrcAnnF $
                                (addressableExpr >>= noSemiP) `sepBy1` comma
                op <- withSrcAnnF $ opAssign >>= noSemiP
                pure (al, lhs, op)

            -- The try here is okay, since the very next thing we do after this
            -- is parse an expression again, without a try.
            rhs_a <- many (try ((expr >>= noSemiP) <* comma))
            (Ann ar rhs_b) <- withSrcAnnF expr

            -- Both sides must have the same length.
            if length lhs == 1 + length rhs_a then pure ()
            else failure [Message "Assignments need the same number of operands on each side"]

            -- Both sides must have just 1 operand in the case of an assign-op.
            if length lhs == 1 then pure ()
            else case op of
                (Ann _ Assign) -> pure ()
                _ -> failure [Message "Assignment operations may have only one operand."]


            let a = SrcSpan (srcStart al) (srcEnd ar)

            pure $ do
                rhs_b' <- rhs_b
                pure $ Fix $ Ann a $ Assignment lhs op (rhs_a ++ [rhs_b'])

-- | Parses an increment or decrement statement (\"x++\", \"y--\"). This is
-- parsed to the same representation as \"x += 1\" or \"y -= 1\".
incDecStmt :: Parser (Semi ()) -> AssignOp () -> Parser (Semi SrcAnnStatement)
incDecStmt opParse op = do
    e <- addressableExpr

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
exprStmtP :: Parser (Semi SrcAnnStatement)
exprStmtP = do
    e <- expr

    condUnSemiP e isValidExprForStmt "Illegal expression in statement context."

    pure $ do
        e' <- e

        let a = topAnn e'

        pure $ Fix $ Ann a $ ExprStmt e'

-- | Checks whether an expression can be used in statement context.
-- This is not entirely correct since some built-ins are disallowed but we need
-- the symbol table to rule out those cases.
isValidExprForStmt :: SrcAnnExpr -> Bool
isValidExprForStmt (Fix (Ann _ (Call _ _ _))) = True
isValidExprForStmt _ = False
