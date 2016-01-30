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
    <|> returnStmt
    <|> ifStmt
    <|> switchStmt
    <|> forStmt
    <|> breakStmt
    <|> continueStmt
    <|> (simpleStmt >>= requireSemiP)

declStmt :: Parser Statement
declStmt = DeclStmt <$> (decl >>= requireSemiP)

printStmt :: Parser Statement
printStmt = do
    hasLn <- (kwPrint *> pure False) <|> (kwPrintLn *> pure True)
    exprs <- parens (expr `sepBy` comma) >>= requireSemiP
    PrintStmt <$> mapM noSemiP exprs <*> pure hasLn

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
breakStmt = (kwBreak >>= requireSemiP) *> pure BreakStmt

continueStmt :: Parser Statement
continueStmt = (kwContinue >>= requireSemiP) *> pure ContinueStmt

decl :: Parser (Semi Declaration)
decl = typeDecl
    <|> varDecl

typeDecl :: Parser (Semi Declaration)
typeDecl = error "typeDecl"

varDecl :: Parser (Semi Declaration)
varDecl = error "varDecl"

-- | Parses a variable specification.
varSpec :: Parser (Semi Declaration)
varSpec = error "varSpec"

-- | Parses a simple statement. In some contexts (such as the initializer for
-- \"if\" and \"switch\" statements), only simple statements are allowed.
simpleStmt :: Parser (Semi Statement)
simpleStmt
    = exprStmt
    <|> shortVarDecl
    <|> assignStmt


-- | Parses a short variable declaration: a list of identifiers followed by the
-- operator \":=\", then by a list of expressions.
shortVarDecl :: Parser (Semi Statement)
shortVarDecl = do
        ids <- (identifier >>= noSemiP) `sepBy1` comma <* shortVarDeclarator
        exprs <- semiTerminatedList expr
        pure $ do
            exprs' <- exprs
            pure $ ShortVarDecl ids exprs'

-- | Parses an assignment statement: a list of expressions followed by an
-- assignment operator (\"=\", \"+=\", etc.) and a list of expressions.
--
-- TODO: Only certain kinds of expressions are allowed on the left-hand side of
-- the assignment operator (namely, addressable expressions). We need to check
-- that this is the case and raise an error otherwise.
assignStmt :: Parser (Semi Statement)
assignStmt = try (incDecStmt opIncrement PlusEq)
        <|>  try (incDecStmt opDecrement MinusEq)
        <|>  do
                lhs <- (expr >>= noSemiP) `sepBy1` comma
                op <- opAssign >>= noSemiP
                rhs <- semiTerminatedList expr
                pure $ do
                    rhs' <- rhs
                    pure $ Assignment lhs op rhs'

-- | Parses an increment or decrement statement (\"x++\", \"y--\"). This is
-- parsed to the same representation as \"x += 1\" or \"y -= 1\".
incDecStmt :: Parser a -> AssignOp -> Parser (Semi Statement)
incDecStmt opParse op = do
        e <- expr
        opParse
        pure $ do
            e' <- e
            noSemi
            pure $ Assignment [e'] op [Literal (IntLit 1)]


-- | Parses an expression as a statement.
--
-- TODO: Go only allows certain kinds of expressions to act as statements. We
-- need to introduce a check that causes invalid expressions to raise errors.
exprStmt :: Parser (Semi Statement)
exprStmt = do
    e <- expr
    pure (ExprStmt <$> e)

-- | Parses one or more instances of `p`, separated by commas, requiring a
-- semicolon on the last instance, but no semicolon on any other instance.
semiTerminatedList :: Parser (Semi a) -> Parser (Semi [a])
semiTerminatedList p = do
        s <- p `sepBy1` comma
        pure $ foldr (\cur acc -> do
                        acc' <- acc
                        cur' <- cur
                        case acc' of
                                [] -> requireSemi
                                _ -> noSemi
                        pure $ cur':acc') (pure []) s