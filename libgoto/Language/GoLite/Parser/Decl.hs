module Language.GoLite.Parser.Decl (
  typeDecl
, varDecl
) where

import Language.GoLite.Parser.Core
import Language.GoLite.SrcAnn

-- | Parses a type declaration. It consists of the \"type\" keyword, followed
-- by either one type specification, or multiple type specifications enclosed in
-- parentheseses. Returns a list of all the type declarations.
typeDecl :: Parser [SrcAnnStatement]
typeDecl = decl kwType typeSpec

-- | Parses a type specification: an identifier followed by a type.
typeSpec :: Parser (Semi SrcAnnStatement)
typeSpec = do
    id_ <- lexeme identifier >>= noSemiP
    typ <- lexeme type_
    pure $ do
        typ' <- typ

        let a = SrcSpan (srcStart (ann id_)) (srcEnd (topAnn typ'))

        pure $ Fix $ Ann a $ DeclStmt (TypeDecl (TypeDeclBody id_ typ'))

-- | Parses a variable declaration. It consists of the \"var\" keyword, followed
-- by either one variable specification, or multiple variable specifications
-- enclosed in parentheses. Returns a list of all the variable specifications.
varDecl :: Parser [SrcAnnStatement]
varDecl = decl kwVar varSpec

-- | Generates a declaration parser. It will run the given keyword parser, then
-- parse either one declaration specification or several specifications enclosed
-- in parentheses.
decl
    :: Parser (Semi a)
    -> Parser (Semi SrcAnnStatement)
    -> Parser [SrcAnnStatement]
decl kw spec = (kw >>= noSemiP) >> (manySpecs <|> oneSpec) where
    oneSpec = fmap pure (spec >>= requireSemiP)
    manySpecs = specList (many spec) >>= requireSemiP

-- | Parses a variable specification. It consists of a comma-separated list of
-- identifiers, followed by a type, then by the assignment operator \"=\", then
-- by a comma-separated list of expressions. Either the type or the list of
-- expressions may be omitted, but not both.
varSpec :: Parser (Semi SrcAnnStatement)
varSpec = (try varSpecNoExpr) <|> do
    (Ann l ids) <- withSrcAnnF $
        (lexeme identifier >>= noSemiP) `sepBy1` comma

    typ <- optional (type_ >>= noSemiP)

    (Ann r exprs) <- withSrcAnnF $ opAssignSimple >> (expr `sepBy1` comma)

    let a = SrcSpan (srcStart l) (srcEnd r)

    pure $ do
        exprs' <- sequenceA exprs
        pure $ Fix $ Ann a $ DeclStmt (VarDecl (VarDeclBody ids typ exprs'))

-- TODO see if there's a way to make this cleaner?
varSpecNoExpr :: Parser (Semi SrcAnnStatement)
varSpecNoExpr = do
    (Ann b ids) <- withSrcAnnF $ (lexeme identifier >>= noSemiP) `sepBy1` comma
    typ <- type_

    pure $ do
        typ' <- typ

        let a = SrcSpan (srcStart b) (srcEnd (topAnn typ'))

        pure $ Fix $ Ann a $
            DeclStmt (VarDecl (VarDeclBody ids (Just typ') []))
