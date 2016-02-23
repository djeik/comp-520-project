{-|
Module      : Language.GoLite.Parser.Stmt
Description : Parsers for statements
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE ViewPatterns #-}

module Language.GoLite.Parser.Stmt
( stmt
, expr
, blockStmt
, blockP -- used in function declarations
, printStmt
, returnStmt
, ifStmt
, switchStmtP
, fallthroughStmtP
, forStmt
, breakStmtP
, continueStmtP
) where

import Language.GoLite.Parser.Core
import Language.GoLite.Parser.SimpleStmts
import Language.GoLite.Parser.Decl

import Control.Monad ( void )

-- | Parses a statement.
--
-- Some statement parsers produce several statements at once (specifically,
-- distributed @type@ and @var@ declarations), so this parser takes care of
-- wrapping the simpler statement parsers into singleton lists.
stmt :: Parser [SrcAnnStatement]
stmt =  varDeclP
    <|> typeDeclP
    <|> choice (map (fmap pure)
        [ printStmt
        , returnStmt
        , ifStmt
        , switchStmtP
        , fallthroughStmtP
        , forStmt
        , breakStmtP
        , continueStmtP
        , blockStmt
        , simpleStmt >>= requireSemiP
        ]
    )

-- | Parses a print statement.
--
-- @println@ is internally represented as a @print@ statement in which a
-- synthetic @"\n"@ is appended to the expression list to print.
printStmt :: Parser SrcAnnStatement
printStmt = do
    (Ann l (runIdentity -> hasLn)) <- withSrcAnnId $
        (try $ kwPrintLn *> pure True) <|> (kwPrint *> pure False)

    (Ann r exprs) <- withSrcAnnF $ parens (expr `sepBy` comma) >>= requireSemiP

    exprs' <- mapM noSemiP exprs

    let a = SrcSpan (srcStart l) (srcEnd r)

    pure $ Fix $ Ann a $ PrintStmt $ case hasLn of
        True ->
            exprs' ++ [Fix (Ann l (Literal (Ann l (StringLit "\n"))))]
        False -> exprs'

-- | Parses a return statement.
returnStmt :: Parser SrcAnnStatement
returnStmt = do
    (Ann l s) <- withSrcAnnF kwReturn
    se <- optional expr

    requireSemiP $ case se of
        Nothing -> do
            s
            pure $ Fix $ Ann l $ ReturnStmt Nothing

        Just e -> do
            s *> noSemi
            e' <- e
            let a = SrcSpan (srcStart l) (srcEnd (topAnn e'))
            pure $ Fix $ Ann a $ ReturnStmt $ Just e'

-- | Parses an if statement. It consists of the keyword if, followed by an
-- optional simple statement, then an expression, then a block, followed by
-- an optional else part. Note that unlike some other languages, the body of the
-- statement must be a block (i.e. enclosed in braces). It cannot be a naked
-- statement.
--
-- If the optional initializer is present, it must have a semicolon. There
-- cannot be a semicolon between the expression and the block.
ifStmt :: Parser SrcAnnStatement
ifStmt = do
    (Ann l _) <- withSrcAnnConst $ kwIf

    initializer <- optional (simpleStmt >>= requireSemiP)
    cond <- expr >>= noSemiP
    thens <- blockP
    (Ann r elses) <- withSrcAnnF $ optional else_

    let a = SrcSpan (srcStart l) (srcEnd r)
    pure $ Fix $ Ann a $ IfStmt initializer cond thens elses

-- | Parses the else part of an if statement. It's the \"else\" keyword followed
-- either by a block or another if statement.
else_ :: Parser [SrcAnnStatement]
else_ = (kwElse >>= unSemiP) >> blockP <|> fmap (:[]) ifStmt

-- | Parses a switch statement. It consists of the \"switch\" keyword, followed
-- by an optional initializer simple statement, an optional expression, then a
-- potentially empty list of case clauses enclosed in brackets.
switchStmtP :: Parser SrcAnnStatement
switchStmtP = do
    (Ann l _) <- withSrcAnnConst kwSwitch
    initializer <- optional (try $ simpleStmt >>= requireSemiP)
    e <- optional (expr >>= noSemiP)
    (Ann r clauses) <- withSrcAnnF $ (braces $ many caseClause) >>= requireSemiP

    let a = SrcSpan (srcStart l) (srcEnd r)

    pure $ Fix $ Ann a $ SwitchStmt initializer e clauses

-- | Parses a case clause. It is a case head and a block separated by a colon.
caseClause :: Parser (SrcAnnCaseHead, [SrcAnnStatement])
caseClause = do
    head_ <- caseHead <* colon
    stmts <- stmt `manyTill` lookAhead (try $ void caseHead <|> void closeBrace)
    pure $ (head_, concat stmts)
    -- Each statement parser may produce multiple statements, so use concat.

-- | Parses a case head. It is either the keyword \"default\", or the keyword
-- \"case\" followed by a comma-separated list of expressions.
caseHead :: Parser SrcAnnCaseHead
caseHead = default_ <|> case_ where
    default_ = (kwDefault >>= noSemiP) *> pure CaseDefault
    case_ = do
        kwCase >>= noSemiP
        exprs <- (expr >>= noSemiP) `sepBy1` comma
        pure $ CaseExpr exprs

-- | Parses a for statement. It starts with the \"for\" keyword, then the for
-- head, then a block. The for head has three forms: nothing, an expression, or
-- an initializer simple statement followed by an expression and another simple
-- statement. In this last case, all the components are optional, and the
-- initializer and expression must end with a semicolon.
forStmt :: Parser SrcAnnStatement
forStmt = do
    (Ann l _) <- withSrcAnnConst kwFor
    (Fix (Ann r s)) <- infiniteFor <|> fullFor <|> simpleFor
    let a = SrcSpan (srcStart l) (srcEnd r)
    pure $ Fix $ Ann a s

-- | Parses an infinite for loop, which does not contain anything in its head.
-- This parser may be backtracked out of before reaching the beginning of a
-- block.
infiniteFor :: Parser SrcAnnStatement
infiniteFor = do
    (try . lookAhead . symbol_) "{"
    (Ann a b) <- withSrcAnnF blockP
    pure $ Fix $ Ann a $ ForStmt Nothing Nothing Nothing b

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
fullFor :: Parser SrcAnnStatement
fullFor = do
    (Ann l (runIdentity -> (initializer, cond, post))) <- withSrcAnnId $
        (,,)
            <$> optional (simpleStmt >>= requireSemiP)
            <*> optional (expr >>= requireSemiP)
            <*> optional (simpleStmt >>= noSemiP)

    (Ann r b) <- withSrcAnnF blockP

    let a = SrcSpan (srcStart l) (srcEnd r)

    pure $ Fix $ Ann a $ ForStmt initializer cond post b

-- | Parses a for loop that only has a condition in its head. This parser can be
-- backtracked out of until reaching the beginning of a block.
simpleFor :: Parser SrcAnnStatement
simpleFor = do
    e <- try $ do
        e <- expr >>= noSemiP
        lookAhead $ symbol_ "{" -- Make sure a block begins next.
        pure e
    (Ann a b) <- withSrcAnnF blockP
    pure $ Fix $ Ann a $ ForStmt Nothing (Just e) Nothing b

-- | Parses a block, which is a potentially empty list of statements enclosed in
-- braces.
blockP :: Parser [SrcAnnStatement]
blockP = do
    symbol "{"
    -- Here we're not applying rule 2 of semicolon omission because we don't
    -- have to and it makes things simpler.
    stmts <- many stmt
    closeBrace >>= requireSemiP

    --let a = SrcSpan (srcStart s) (srcEnd e)
    -- Each statement parser may produce multiple statements.
    pure $ concat stmts

-- | Parses a block, wrapped as a statement.
blockStmt :: Parser SrcAnnStatement
blockStmt = do
    (Ann a b) <- withSrcAnnF blockP
    pure $ Fix $ Ann a $ Block b

-- | Parses a break statement, which consists of the \"break\" keyword.
breakStmtP :: Parser SrcAnnStatement
breakStmtP = do
    (Ann a _) <- withSrcAnnConst $ kwBreak >>= requireSemiP
    pure $ Fix $ Ann a $ BreakStmt

-- | Parses a fallthrough statement, which consists of the \"fallthrough\"
-- keyword.
fallthroughStmtP :: Parser SrcAnnStatement
fallthroughStmtP = do
    (Ann a _) <- withSrcAnnConst $ kwFallthrough >>= requireSemiP
    pure $ Fix $ Ann a $ FallthroughStmt

-- | Parses a continue statement, which consists of the \"continue\" keyword.
continueStmtP :: Parser SrcAnnStatement
continueStmtP = do
    (Ann a _) <- withSrcAnnConst $ kwContinue >>= requireSemiP
    pure $ Fix $ Ann a $ ContinueStmt
