module Language.GoLite.Parser
-- TODO revisit the list of exports
(
  package,
  -- * Declarations
  topLevelDecl,
  funDecl,
  module Language.GoLite.Parser.Decl,
  module Language.GoLite.Parser.Stmt
) where

import Language.GoLite.Parser.Core
import Language.GoLite.Parser.Decl
import Language.GoLite.Parser.Stmt

-- | Parses a package: the package header (`package` keyword followed by an
-- identifier), followed by a list of top-level declarations.
package :: Parser SrcAnnPackage
package = do
    name <- (kwPackage >>= noSemiP) >>
            (lexeme identifier >>= requireSemiP)
    decls <- many topLevelDecl

    pure $ Package name (concat decls)

-- | Parses a top-level declaration: either a regular declaration (type/var) or
-- a function declaration. Since declarations can be distributed, this returns
-- a list.
topLevelDecl :: Parser [SrcAnnTopLevelDecl]
topLevelDecl = typ <|> var <|> fun
    where
        typ = fmap (map fromDeclStmt) typeDecl
        var = fmap (map fromDeclStmt) varDecl
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
    name <- (kwFunc >>= noSemiP) >> lexeme identifier >>= noSemiP
    params <- (parens $ (field >>= noSemiP) `sepBy` comma) >>= noSemiP
    ret <- optional (type_ >>= noSemiP)
    b <- block

    pure $ FunDecl name params ret b


