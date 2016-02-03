module Language.GoLite.Parser
-- TODO revisit the list of exports
( -- * Statements
  stmt
, expr
, printStmt
, returnStmt
, ifStmt
, switchStmt
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
