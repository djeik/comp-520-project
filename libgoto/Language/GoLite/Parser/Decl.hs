{-|
Module      : Language.GoLite.Parser.Decl
Description : Parsers for declarations
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

module Language.GoLite.Parser.Decl
( typeDeclP
, varDeclP
) where

import Language.GoLite.Parser.Core

-- | Parses a type declaration. It consists of the \"type\" keyword, followed
-- by either one type specification, or multiple type specifications enclosed in
-- parentheseses. Returns a list of all the type declarations.
typeDeclP :: Parser [SrcAnnStatement]
typeDeclP = decl kwType typeSpec

-- | Parses a type specification: an identifier followed by a type.
typeSpec :: Parser (SrcAnnStatement)
typeSpec = do
    id_ <- lexeme identifier >>= noSemiP
    typ <- lexeme type_ >>= requireSemiP

    let a = SrcSpan (srcStart (ann id_)) (srcEnd (topAnn typ))

    pure $ Fix $ Ann a $ DeclStmt (TypeDecl (TypeDeclBody id_ typ))

-- | Parses a variable declaration. It consists of the \"var\" keyword, followed
-- by either one variable specification, or multiple variable specifications
-- enclosed in parentheses. Returns a list of all the variable specifications.
varDeclP :: Parser [SrcAnnStatement]
varDeclP = decl kwVar (try varSpec <|> varSpecNoExpr)

-- | Generates a declaration parser. It will run the given keyword parser, then
-- parse either one declaration specification or several specifications enclosed
-- in parentheses.
decl
    :: Parser (Semi a)
    -> Parser (SrcAnnStatement)
    -> Parser [SrcAnnStatement]
decl kw spec = (kw >>= noSemiP) >> (manySpecs <|> oneSpec) where
    oneSpec = fmap pure spec
    manySpecs = (parens $ many spec) >>= requireSemiP

-- | Parses a variable specification. It consists of a comma-separated list of
-- identifiers, followed by a type, then by the assignment operator \"=\", then
-- by a comma-separated list of expressions. Either the type or the list of
-- expressions may be omitted, but not both.
varSpec :: Parser (SrcAnnStatement)
varSpec = do
    (Ann l ids) <- withSrcAnnF $
        (lexeme identifier >>= noSemiP) `sepBy1` comma

    typ <- optional (type_ >>= noSemiP)

    (Ann r exprs) <- withSrcAnnF $ opAssignSimple >>
                (semiList (expr `sepBy1` comma) noSemi requireSemi >>= unSemiP)

    let a = SrcSpan (srcStart l) (srcEnd r)

    pure $ Fix $ Ann a $ DeclStmt (VarDecl (VarDeclBody ids typ exprs))

-- TODO see if there's a way to make this cleaner?
varSpecNoExpr :: Parser (SrcAnnStatement)
varSpecNoExpr = do
    (Ann b ids) <- withSrcAnnF $ (lexeme identifier >>= noSemiP) `sepBy1` comma
    typ <- type_ >>= requireSemiP

    let a = SrcSpan (srcStart b) (srcEnd (topAnn typ))
    pure $ Fix $ Ann a $
            DeclStmt (VarDecl (VarDeclBody ids (Just typ) []))
