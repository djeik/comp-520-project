{-|
Module      : Language.GoLite.Weeder.Stmt
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

"Weeder" traversal definition for declarations and statements.
-}

module Language.GoLite.Weeder.Stmt
( weedDecl
, weedStmt
) where

import Language.GoLite.Weeder.Core
import Language.GoLite.Weeder.Expr
import Language.GoLite.Weeder.TypeLit

-- | Weeds a declaration (either variable or type) and its components.
weedDecl :: SrcAnnDeclaration -> Weeder ()
weedDecl (TypeDecl (TypeDeclBody _ ty)) = weedType ty
weedDecl (VarDecl (VarDeclBody _ ty es)) = do
    case ty of
        Nothing -> pure ()
        Just ty' -> weedType ty'
    void $ mapM weedExpr es


{- | Weeds a statement and its components.

    * In an assignment, the blank identifier may not appear as an operand on
      the right-hand side.
    * In an assignment-operation, the blank identifier may not appear as a
      operand on the left-hand side.
    * In a return statement, the expression must be present if and only if the
      function declares a return type.
    * In a switch statement, only one default is allowed.
    * A break statement may not occur outside of a for/switch statement.
    * A continue statement may not occur outside of a for statement.
    * Fallthrough statements are not supported.
-}
weedStmt :: SrcAnnStatement -> Weeder ()

-- Declaration statement: weed the inner declaration.
weedStmt (Fix (Ann _ (DeclStmt d))) = weedDecl d

-- Expression statement: then weed the inner expression.
weedStmt (Fix (Ann _ (ExprStmt e))) = weedExpr e

-- Short variable declaration: weed the inner expressions.
weedStmt (Fix (Ann _ (ShortVarDecl _ es))) = void $ mapM weedExpr es

-- Assignment: weed the expressions on each side. Blank identifiers are allowed
-- as operands on the left in the case of a normal assignment.
weedStmt (Fix (Ann _ (Assignment lhs op rhs))) = do
    case bare op of
        Assign -> void $ mapM weedExprAllowingBlanks lhs
        _ -> void $ mapM weedExpr lhs

    void $ mapM weedExpr rhs
    where
        weedExprAllowingBlanks :: SrcAnnExpr -> Weeder ()
        weedExprAllowingBlanks e = when (not ("_" `isIdAsOperand` e)) (weedExpr e)

-- Print statement: weed the inner expressions
weedStmt (Fix (Ann _ (PrintStmt es))) = void $ mapM weedExpr es

-- Return statement with no expression: check that we're in a function that has
-- no return.
weedStmt (Fix (Ann a (ReturnStmt Nothing))) = do
    ret <- gets funcHasReturn
    when (ret) (reportError $ WeederException a "not enough arguments to return")

-- Return statement with an expression: check that we're in a function that has
-- a return, then weed the expression.
weedStmt (Fix (Ann a (ReturnStmt (Just e)))) = do
    ret <- gets funcHasReturn
    when (not ret) (reportError $ WeederException a "too many arguments to return")

    weedExpr e

-- If statement: weed the initializer, expression, then statements and else
-- statements.
weedStmt (Fix (Ann _ (IfStmt init' e thens elses))) = do
    case init' of
        Nothing -> pure ()
        Just init'' -> weedStmt init''
    weedExpr e
    void $ mapM weedStmt thens
    case elses of
        Nothing -> pure ()
        Just elses' -> void $ mapM weedStmt elses'

-- Switch statement: check that there is only one default clause, then weed the
-- initializer, the expression and the clauses.
weedStmt (Fix (Ann a (SwitchStmt init' e clauses))) = do
    case init' of
        Nothing -> pure ()
        Just init'' -> weedStmt init''

    case e of
        Nothing -> pure ()
        Just e' -> weedExpr e'

    when (length (filter isDefaultCase clauses) > 1)
        (reportError $ WeederException a "multiple defaults in switch")

    modify $ \s -> incSwitchLevel s
    void $ mapM weedCaseHead clauses
    modify $ \s -> decSwitchLevel s

-- For statement: weed the pre-statement, condition, post-statement and body.
weedStmt (Fix (Ann _ (ForStmt pre cond post body))) = do
    case pre of
        Nothing -> pure ()
        Just pre' -> weedStmt pre'

    case cond of
        Nothing -> pure ()
        Just cond' -> weedExpr cond'

    case post of
        Nothing -> pure ()
        Just post' -> weedStmt post'

    modify $ \s -> incForLevel s
    void $ mapM weedStmt body
    modify $ \s -> decForLevel s

-- Break statement: may not occur outside of a for/switch.
weedStmt (Fix (Ann a BreakStmt)) = do
    f <- gets forLevel
    s <- gets switchLevel
    when (f == 0 && s == 0)
        (reportError $ WeederException a "break outside of a loop/switch")

-- Continue statement: may not occur outside of a for.
weedStmt (Fix (Ann a ContinueStmt)) = do
    f <- gets forLevel
    when (f == 0)
        (reportError $ WeederException a "continue outside of a loop")

-- Fallthrough statement: may not occur (is not supported)
weedStmt (Fix (Ann a FallthroughStmt)) =
    reportError $ WeederException a "fallthrough is unsupported"

-- Block statement: weed inner statements.
weedStmt (Fix (Ann _ (Block b))) = void $ mapM weedStmt b

-- Empty statement: nothing.
weedStmt (Fix (Ann _ EmptyStmt)) = pure ()

-- Case head: for a normal case with expressions, weed the expressions.
-- Weed the inner statements.
weedCaseHead :: (SrcAnnCaseHead, [SrcAnnStatement]) -> Weeder ()
weedCaseHead c = do
    case fst c of
        CaseExpr es -> do
            void $ mapM weedExpr es
        _ -> pure ()
    void $ mapM weedStmt (snd c)