module Language.GoLite.Parser
( -- * Statements
  stmt
, declStmt
, printStmt
, exprStmt
, returnStmt
, ifStmt
, switchStmt
, forStmt
, breakStmt
, continueStmt
  -- * Declarations
, decl
, typeDecl
, varDecl
  -- * Expressions
, module Language.GoLite.Parser.Expression
) where

import Language.GoLite.Lexer
import Language.GoLite.Parser.Expression
import Language.GoLite.Syntax

stmt :: Parser Statement
stmt = declStmt
    <|> printStmt
    <|> exprStmt
    <|> returnStmt
    <|> ifStmt
    <|> switchStmt
    <|> forStmt
    <|> breakStmt
    <|> continueStmt

declStmt :: Parser Statement
declStmt = DeclStmt <$> (decl >>= requireSemiP)

printStmt :: Parser Statement
printStmt = do
    hasLn <- (kwPrint *> pure False) <|> (kwPrintLn *> pure True)
    exprs <- parens (expr `sepBy` comma) >>= requireSemiP
    PrintStmt <$> mapM noSemiP exprs <*> pure hasLn

-- | Parses an expression as a statement.
--
-- TODO: Go only allows certain kinds of expressions to act as statements. We
-- need to introduce a check that causes invalid expressions to raise errors.
exprStmt :: Parser Statement
exprStmt = ExprStmt <$> (expr >>= requireSemiP)

-- | Parses a return statement.
returnStmt :: Parser Statement
returnStmt = do
    s <- kwReturn
    se <- optional expr

    requireSemiP $ case se of
        Nothing -> s *> pure (ReturnStmt Nothing)
        Just e -> s *> noSemi *> fmap (ReturnStmt . Just) e

ifStmt :: Parser Statement
ifStmt = error "ifStmt"

switchStmt :: Parser Statement
switchStmt = error "switchStmt"

forStmt :: Parser Statement
forStmt = error "forStmt"

breakStmt :: Parser Statement
breakStmt = error "breakStmt"

continueStmt :: Parser Statement
continueStmt = error "continueStmt"

decl :: Parser (Semi Declaration)
decl = typeDecl
    <|> varDecl

typeDecl :: Parser (Semi Declaration)
typeDecl = error "typeDecl"

varDecl :: Parser (Semi Declaration)
varDecl = error "varDecl"

simpleStmt :: Parser (Semi SimpleStatement)
simpleStmt = error "simpleStmt"

shortVarDecl :: Parser (Semi ShortVarDecl)
shortVarDecl = error "shortVarDecl"

assignStmt :: Parser (Semi Assignment)
assignStmt = error "assignStmt"