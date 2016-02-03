module Language.GoLite.Parser.Decl (
  typeDecl
, varDecl
) where

import Language.GoLite.Parser.Core

-- | Parses a type declaration. It consists of the \"type\" keyword, followed
-- by either one type specification, or multiple type specifications enclosed in
-- parentheseses. Returns a list of all the type declarations.
typeDecl :: Parser [Statement]
typeDecl = decl kwType typeSpec

-- | Parses a type specification: an identifier followed by a type.
typeSpec :: Parser (Semi Statement)
typeSpec = do
    id_ <- lexeme identifier >>= noSemiP
    typ <- lexeme type_
    pure $ do
        typ' <- typ
        pure $ DeclStmt (TypeDecl (TypeDeclBody id_ typ'))

-- | Parses a variable declaration. It consists of the \"var\" keyword, followed
-- by either one variable specification, or multiple variable specifications
-- enclosed in parentheses. Returns a list of all the variable specifications.
varDecl :: Parser [Statement]
varDecl = decl kwVar varSpec

-- | Generates a declaration parser. It will run the given keyword parser, then
-- parse either one declaration specification or several specifications enclosed
-- in parentheses.
decl :: Parser (Semi a) -> Parser (Semi Statement) -> Parser [Statement]
decl kw spec = (kw >>= noSemiP) >> (manySpecs <|> oneSpec) where
    oneSpec = fmap pure (spec >>= requireSemiP)
    manySpecs = specList (many spec) >>= requireSemiP

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
