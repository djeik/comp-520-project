module Language.GoLite.Parser.Decl (
  typeDecl
, varDecl
) where

import Language.GoLite.Parser.Core

typeDecl :: Parser [Statement]
typeDecl = error "typeDecl"

-- | Parses a variable declaration. It consists of the \"var\" keyword, followed
-- by either one variable specification, or multiple variable specifications
-- enclosed in parentheses. Returns a list of all the variable specifications.
varDecl :: Parser [Statement]
varDecl = (kwVar >>= noSemiP) >> (manyVarSpecs <|> oneVarSpec) where
    oneVarSpec = fmap pure (varSpec >>= requireSemiP)
    manyVarSpecs = parens (semiList (many varSpec)
                requireSemi -- Want a semicolon at each internal varSpec
                (pure ()) >>= unSemiP)  -- For the last one, we don't care:
                                        -- var (x = 2) or var (x = 2;) is OK.
        >>= requireSemiP -- Need a semi after the closing paren.

-- | Parses a variable specification. It consists of a comma-separated list of
-- identifiers, followed by a type, then by the assignment operator \"=\", then
-- by a comma-separated list of expressions. Either the type or the list of
-- expressions may be omitted, but not both.
varSpec :: Parser (Semi Statement)
varSpec = (try varSpecNoExpr) <|> do
    ids <- (lexeme identifier >>= noSemiP) `sepBy1` comma
    typ <- optional (type_ >>= noSemiP)
    exprs <- opAssignSimple >> (expr `sepBy1` comma)
    pure $ do
        exprs' <- sequenceA exprs
        pure $ DeclStmt (VarDecl (VarDeclBody ids typ exprs'))

-- TODO see if there's a way to make this cleaner?
varSpecNoExpr :: Parser (Semi Statement)
varSpecNoExpr = do
    ids <- (lexeme identifier >>= noSemiP) `sepBy1` comma
    typ <- type_
    pure $ do
        typ' <- typ
        pure $ DeclStmt (VarDecl (VarDeclBody ids (Just typ') []))
