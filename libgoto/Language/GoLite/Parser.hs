{-|
Module      : Language.GoLite.Parser
Description : High level parsers and re-exports
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

This module contains the top-most parser 'package' as well as parsers that
combine both declarations and statements.
-}

module Language.GoLite.Parser
( packageP
  -- * Declarations
, topLevelDeclP
, funDecl
, module Language.GoLite.Parser.Decl
  -- * Statements
, module Language.GoLite.Parser.Stmt
) where

import Language.Common.Misc ( distTuple )
import Language.GoLite.Parser.Core
import Language.GoLite.Parser.Decl
import Language.GoLite.Parser.Stmt

-- | Parses a package: the package header (`package` keyword followed by an
-- identifier), followed by a list of top-level declarations.
packageP :: Parser SrcAnnPackage
packageP = do
    name <- (kwPackage >>= noSemiP) >> (identifier >>= requireSemiP)
    decls <- many topLevelDeclP


    pure $ Package name (concat decls)

-- | Parses a top-level declaration: either a regular declaration (type/var) or
-- a function declaration. Since declarations can be distributed, this returns
-- a list.
topLevelDeclP :: Parser [SrcAnnTopLevelDecl]
topLevelDeclP = typ <|> var <|> fun
    where
        typ = fmap (map fromDeclStmt) typeDeclP
        var = fmap (map fromDeclStmt) varDeclP
        fun = fmap (:[]) (fmap TopLevelFun funDecl)

-- | Converts a declaration statement into a top-level declaration. This is
-- so we can just use the same parser twice.
fromDeclStmt :: SrcAnnStatement -> SrcAnnTopLevelDecl
fromDeclStmt (Fix (Ann _ (DeclStmt d))) = TopLevelDecl d
fromDeclStmt _ = error "Not a DeclStmt"

-- | Parses a function declaration: The func keyword, followed by a function
-- name, a parenthesized potentially empty list of parameter fields
-- (comma-separated list of identifiers then a type) separated by commas, an
-- optional return type and a block.
funDecl :: Parser SrcAnnFunDecl
funDecl = do
    name <- (kwFunc >>= noSemiP) >> (identifier >>= noSemiP)
    params <- (parens $ (field >>= noSemiP) `sepBy` comma) >>= noSemiP
    ret <- optional (type_ >>= noSemiP)
    b <- blockP >>= requireSemiP

    pure $ FunDecl name (concatMap (uncurry distTuple) params) ret b
