module Language.GoLite.Parser.SimpleStmts (
  simpleStmt
) where

import Language.GoLite.Parser.Core
import Language.GoLite.SrcAnn

import Data.Functor.Foldable ( Fix(..) )

-- | Parses a simple statement. In some contexts (such as the initializer for
-- \"if\" and \"switch\" statements), only simple statements are allowed.
simpleStmt :: Parser (Semi SrcAnnStatement)
simpleStmt
    = try assignStmt
    <|> shortVarDecl
    <|> exprStmt

-- | Parses a short variable declaration: a list of identifiers followed by the
-- operator \":=\", then by a list of expressions.
shortVarDecl :: Parser (Semi SrcAnnStatement)
shortVarDecl = do
    ids@(i1:_) <-
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
assignStmt = try (incDecStmt opIncrement PlusEq)
    <|>  try (incDecStmt opDecrement MinusEq)
    <|>  do
            (Ann al lhs) <- withSrcAnnF $ (expr >>= noSemiP) `sepBy1` comma
            op <- withSrcAnnF $ opAssign >>= noSemiP
            (Ann ar rhs) <- withSrcAnnF $ semiTerminatedCommaList expr

            let a = SrcSpan (srcStart al) (srcEnd ar)

            pure $ do
                rhs' <- rhs

                pure $ Fix $ Ann a $ Assignment lhs op rhs'

-- | Parses an increment or decrement statement (\"x++\", \"y--\"). This is
-- parsed to the same representation as \"x += 1\" or \"y -= 1\".
incDecStmt :: Parser a -> AssignOp () -> Parser (Semi SrcAnnStatement)
incDecStmt opParse op = do
    e <- expr

    (Ann opSpan _) <- withSrcAnnConst opParse

    pure $ do
        e' <- e
        noSemi

        let a = SrcSpan (srcStart (topAnn e')) (srcEnd opSpan)

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
