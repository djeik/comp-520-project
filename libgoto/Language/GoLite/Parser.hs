module Language.GoLite.Parser
-- TODO revisit the list of exports
( -- * Statements
  stmt
, printStmt
, exprStmt
, returnStmt
, ifStmt
, switchStmt
, forStmt
, breakStmt
, continueStmt
  -- * Declarations
, typeDecl
, varDecl
  -- * Expressions
, module Language.GoLite.Parser.Expression
) where

import Language.GoLite.Lexer
import Language.GoLite.Parser.Expression
import Language.GoLite.Syntax

stmt :: Parser [Statement]
stmt =  varDecl
    <|> typeDecl
    <|> choice (map (fmap pure)
        [ printStmt
        , returnStmt
        , ifStmt
        , switchStmt
        , forStmt
        , breakStmt
        , continueStmt
        , (simpleStmt >>= requireSemiP) ])

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

typeDecl :: Parser [Statement]
typeDecl = error "typeDecl"

-- Parses a variable declaration. It consists of the \"var\" keyword, followed
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
-- identifiers, followed by an optional type, then by the assignment operator
-- \"=\", then by a comma-separated list of expressions.
varSpec :: Parser (Semi Statement)
varSpec = do
    ids <- (lexeme identifier >>= noSemiP) `sepBy1` comma
    typ <- optional (type_ >>= noSemiP)
    exprs <- opAssignSimple >> (expr `sepBy1` comma)
    pure $ do
        exprs' <- sequenceA exprs
        pure $ DeclStmt (VarDecl (VarDeclBody ids typ exprs'))

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
    ids <- (lexeme identifier >>= noSemiP) `sepBy1` comma <* shortVarDeclarator
    exprs <- semiTerminatedCommaList expr
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
            rhs <- semiTerminatedCommaList expr
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
semiTerminatedCommaList :: Parser (Semi a) -> Parser (Semi [a])
semiTerminatedCommaList p = semiList (p `sepBy` comma) noSemi requireSemi

-- | Transforms a parser producing a list of Semi elements into a parser
-- producing a Semi list of elements, with potentially different semicolon
-- checks for the last element versus the rest of the list.
semiList :: Parser ([Semi a]) -> Semi () -> Semi () -> Parser (Semi [a])
semiList p internal end = do
    s <- p
    pure $ foldr (\cur acc -> do
                    acc' <- acc
                    cur' <- cur
                    case acc' of
                            [] -> end
                            _ -> internal
                    pure $ cur':acc') (pure []) s