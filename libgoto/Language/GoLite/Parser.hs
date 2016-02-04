module Language.GoLite.Parser
-- TODO revisit the list of exports
( -- * Statements
  stmt
, expr
, printStmt
, returnStmt
, ifStmt
, switchStmt
, fallthroughStmt
, forStmt
, breakStmt
, continueStmt
  -- * Declarations
, typeDecl
, varDecl
) where

import Language.GoLite.Parser.Core
import Language.GoLite.Parser.SimpleStmts
import Language.GoLite.Parser.Decl

stmt :: Parser [Statement]
stmt =  varDecl
    <|> typeDecl
    <|> choice (map (fmap pure)
        [ printStmt
        , returnStmt
        , ifStmt
        , switchStmt
        , fallthroughStmt
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

-- | Parses an if statement. It consists of the keyword if, followed by an
-- optional simple statement, then an expression, then a block, followed by
-- an optional else part. Note that unlike some other languages, the body of the
-- statement must be a block (i.e. enclosed in braces). It cannot be a naked
-- statement.
--
-- If the optional initializer is present, it must have a semicolon. There
-- cannot be a semicolon between the expression and the block.
ifStmt :: Parser Statement
ifStmt = do
    initializer <- (kwIf >>= unSemiP) >> optional (simpleStmt >>= requireSemiP)
    cond <- expr >>= noSemiP
    thens <- block
    elses <- optional else_
    pure $ IfStmt initializer cond thens elses

-- | Parses the else part of an if statement. It's the \"else\" keyword followed
-- either by a block or another if statement.
else_ :: Parser Block
else_ = (kwElse >>= unSemiP) >> block <|> fmap (:[]) ifStmt

-- | Parses a switch statement. It consists of the \"switch\" keyword, followed
-- by an optional initializer simple statement, an optional expression, then a
-- potentially empty list of case clauses enclosed in brackets.
switchStmt :: Parser Statement
switchStmt = do
    kwSwitch
    initializer <- optional (simpleStmt >>= requireSemiP)
    e <- optional (expr >>= noSemiP)
    clauses <- (braces $ many caseClause) >>= noSemiP
    pure $ SwitchStmt initializer e clauses

-- | Parses a case clause. It is a case head and a block separated by a colon.
caseClause :: Parser (CaseHead, Block)
caseClause = do
    head_ <- caseHead <* colon
    stmts <- many stmt
    pure $ (head_, concat stmts)
    -- Each statement parser may produce multiple statements, so use concat.

-- | Parses a case head. It is either the keyword \"default\", or the keyword
-- \"case\" followed by a comma-separated list of expressions.
caseHead :: Parser CaseHead
caseHead = default_ <|> case_ where
    default_ = (kwDefault >>= noSemiP) *> pure CaseDefault
    case_ = do
        kwCase
        exprs <- (expr >>= noSemiP) `sepBy1` comma
        pure $ CaseExpr exprs

-- | Parses a for statement. It starts with the \"for\" keyword, then the for
-- head, then a block. The for head has three forms: nothing, an expression, or
-- an initializer simple statement followed by an expression and another simple
-- statement. In this last case, all the components are optional, and the
-- initializer and expression must end with a semicolon.
forStmt :: Parser Statement
forStmt = (kwFor >>= noSemiP) >> (infiniteFor <|> fullFor <|> simpleFor)

-- | Parses an infinite for loop, which does not contain anything in its head.
-- This parser may be backtracked out of before reaching the beginning of a
-- block.
infiniteFor :: Parser Statement
infiniteFor = do
    (try . lookAhead . symbol_) "{"
    b <- block
    pure $ ForStmt Nothing Nothing Nothing b

-- | Parses a full for loop, which may contain an initializer simple statement,
-- an expression and a post-iteration simple statement. All those components
-- are optional. The initializer and expression must end with a semicolon.
--
-- TODO We need to support cases where the initializer or expression are not
-- present but semicolons are, e.g.:
--      `for ; x < 2; { `
--      `for ;; y-- {`
--      `for ;; {`
-- ... and so on.
fullFor :: Parser Statement
fullFor = do
    initializer <- optional (simpleStmt >>= requireSemiP)
    cond <- optional (expr >>= requireSemiP)
    post <- optional (simpleStmt >>= noSemiP)
    b <- block
    pure $ ForStmt initializer cond post b

-- | Parses a for loop that only has a condition in its head. This parser can be
-- backtracked out of until reaching the beginning of a block.
simpleFor :: Parser Statement
simpleFor = do
    e <- try $ do
        e <- expr >>= noSemiP
        lookAhead $ symbol_ "{" -- Make sure a block begins next.
        pure e
    b <- block
    pure $ ForStmt Nothing (Just e) Nothing b

-- | Parses a block, which is a potentially empty list of statements enclosed in
-- braces.
block :: Parser Block
block = do
        symbol "{"
        -- Here we're not applying rule 2 of semicolon omission because we don't
        -- have to and it makes things simpler.
        stmts <- many stmt
        closeBrace >>= requireSemiP
        -- Each statement parser may produce multiple statements.
        pure $ concat stmts

-- | Parses a break statement, which consists of the \"break\" keyword.
breakStmt :: Parser Statement
breakStmt = (kwBreak >>= requireSemiP) *> pure BreakStmt

-- | Parses a continue statement, which consists of the \"continue\" keyword.
continueStmt :: Parser Statement
continueStmt = (kwContinue >>= requireSemiP) *> pure ContinueStmt

-- | Parses a fallthrough statement, which consists of the \"fallthrough\"
-- keyword.
fallthroughStmt :: Parser Statement
fallthroughStmt = (kwFallthrough >>= requireSemiP) *> pure ContinueStmt

