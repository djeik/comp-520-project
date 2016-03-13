{-|
Module      : Language.GoLite.Weeder
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

This module contains the Weeder traversal. This traversal cannot be aborted.
Non-fatal errors are accumulated in the state.
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.GoLite.Weeder where

import Data.Maybe ( isJust, isNothing )

import Language.GoLite.Monad.Traverse
import Language.GoLite.Syntax
import Language.GoLite.Syntax.SrcAnn

import Data.Void ( Void )

type WeederException = (SrcSpan, String)

-- | Internal state used by the weeder. The only field of interest to the
-- outside world is the "weedErrors" field.
data WeederState
    = WeederState
        { funcHasReturn :: Bool
        -- ^ Specifies whether the function we are in has a declared return type
        -- or not. Outside of functions, the value is undetermined.
        , forLevel :: Int
        -- ^ Indicates the current level of for-loop nesting.
        , switchLevel :: Int
        -- ^ Indicates the current level of switch nesting.
        , weedErrors :: [WeederException]
        }
    deriving (Show)

setFuncHasReturn :: WeederState -> Bool -> WeederState
setFuncHasReturn s b = s { funcHasReturn = b }

incForLevel :: WeederState -> WeederState
incForLevel s = s { forLevel = forLevel s + 1 }

decForLevel :: WeederState -> WeederState
decForLevel s = s { forLevel = forLevel s - 1 }

incSwitchLevel :: WeederState -> WeederState
incSwitchLevel s = s { switchLevel = switchLevel s + 1 }

decSwitchLevel :: WeederState -> WeederState
decSwitchLevel s = s { switchLevel = switchLevel s - 1 }


newtype Weeder a
    = Weeder { runWeeder :: Traversal Void WeederState a }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState WeederState
        , MonadError Void
        )

instance MonadTraversal Weeder where
    type TraversalError Weeder = WeederException
    type TraversalState Weeder = WeederState
    type TraversalException Weeder = Void

    reportError e = modify $ \s -> s { weedErrors = e:(weedErrors s) }

    getErrors s = weedErrors s

-- | Weeds a package and its components. A package is invalid if its identifier
-- is the blank one.
weedPackage :: SrcAnnPackage -> Weeder ()
weedPackage (Package (Ann a (Ident n)) tlds) = do
    when (n == "_")
        (reportError (a, "Package name may not be the blank identifier."))

    void $ mapM weedTopLevelDecl tlds

-- | Weeds a top-level declaration: either a variable/type declaration or a
-- function declaration.
weedTopLevelDecl :: SrcAnnTopLevelDecl -> Weeder ()
weedTopLevelDecl (TopLevelDecl d) = weedDecl d
weedTopLevelDecl (TopLevelFun f) = weedFunDecl f

{- | Weeds a function declaration.

    * Functions named @init@ may not declare any arguments or return types.
    * Functions with declared return types must end with a terminating statement.
-}
weedFunDecl :: SrcAnnFunDecl -> Weeder ()
weedFunDecl (FunDecl (Ann a (Ident n)) pars rty bod) = do
    when (n == "init" && (pars /= [] || rty /= Nothing))
        (reportError (a, "func init must have no arguments \
                            \and no return values"))

    when (n /= "init"
        && isJust rty
        && (bod == [] || (not $ isTerminating $ bareStmt (last bod))))
        (reportError (a, "missing return at end of function"))

    modify $ \s -> s { funcHasReturn = (isJust rty) }
    void $ mapM weedStmt bod


{- | Checks if a statement is terminating.

    * A return statement is terminating.
    * A call to the @panic@ function is terminating.
    * A block is terminating if it ends with a terminating statement.
    * An if statement is terminating if both branches are present and both end
      with a terminating statement.
    * A for loop is terminating if it has no condition and no break that refers
      to it.
    * A switch statement is terminating if:

        * It has a default case.
        * No case contains a break.
        * Every case ends in a terminating statement.

    * Every other statement is non-terminating.
-}
isTerminating :: BasicStatement -> Bool
isTerminating (Fix (ReturnStmt _)) = True
isTerminating (Fix (ExprStmt e)) =
    case e of
        (Fix (Call (Fix (Variable (Ident n))) _ _)) -> n == "panic"
        _ -> False
isTerminating (Fix (Block stmts)) = isTerminating' stmts
isTerminating (Fix (IfStmt _ _ thens elses)) =
    isTerminating' thens &&
        case elses of
            Just elses' -> isTerminating' elses'
            Nothing -> False
isTerminating (Fix (ForStmt _ Nothing _ body)) = not $ hasBreak body
isTerminating (Fix (SwitchStmt _ _ heads)) =
    any isDefaultCase heads && all isTerminating' (map snd heads)
isTerminating _ = False


-- | Checks if a list of statements is terminating, which is the case when the
-- list is non-empty and the last element is terminating.
isTerminating' :: [BasicStatement] -> Bool
isTerminating' [] = False
isTerminating' xs = (isTerminating . last) xs

-- | Returns True just in case this case is the default case.
isDefaultCase :: (CaseHead e, f) -> Bool
isDefaultCase (CaseDefault, _) = True
isDefaultCase _ = False

-- | Checks if a list of statements contains a break. It does not check inside
-- complex statements that would be referred to by an enclosed break
-- (i.e. "ForStmt" and "SwitchStmt").
hasBreak :: [BasicStatement] -> Bool
hasBreak [] = False
hasBreak (x:xs) = case x of
    (Fix BreakStmt) -> True
    (Fix (IfStmt _ _ thens elses)) ->
        hasBreak thens
        || case elses of
            Just elses' -> hasBreak elses'
            Nothing -> False
        || hasBreak xs

    (Fix (Block bod)) -> hasBreak bod || hasBreak xs
    {- For other statements:
        * If it's a for/switch, we don't need to check inside them because any
          break there will refer to that statement and not the one we're
          interested in.
        * If it's something else, it won't contain anything, and also
          won't be a break.
      Thus we can just check the rest of the statements. -}
    _ -> hasBreak xs


-- | Weeds a declaration (either variable or type).
-- In a variable declaration, nil cannot be used as a value when no type is
-- specified.
weedDecl :: SrcAnnDeclaration -> Weeder ()
weedDecl (TypeDecl (TypeDeclBody _ _)) = pure ()
weedDecl (VarDecl (VarDeclBody _ ty es)) = do

    when (isNothing ty) (errorOnNil es)
    void $ mapM weedExpr es
    where
        errorOnNil :: [SrcAnnExpr] -> Weeder ()
        errorOnNil [] = pure ()
        errorOnNil (x:xs) = do
            case x of
                (Fix (Ann _ (Variable (Ann a (Ident "nil"))))) ->
                    reportError (a, "use of untyped nil")
                _ -> pure ()
            errorOnNil xs

{- | Weeds a statement and its components.

    * In expression statements, some built-ins cannot appear as calls (namely
      @append@, @cap@, @complex@, @imag@, @len@, @make@, @new@, and @real@)
    * In an assignment, the blank identifier may not appear as an operand on
      the right-hand side.
    * In an assignment-operation, the blank identifier may not appear as a
      operand on the left-hand side.
    * In a return statement, the expression must be present if and only if the
      function declares a return type.
    * In a switch statement, only one default is allowed.
-}
weedStmt :: SrcAnnStatement -> Weeder ()

-- Declaration statement: weed the inner declaration.
weedStmt (Fix (Ann _ (DeclStmt d))) = weedDecl d

-- Expression statement: check that it's not a function that can't appear in
-- statement context, then weed the inner expression.
weedStmt (Fix (Ann a (ExprStmt e))) = do
    case bareExpr e of
        (Fix (Call (Fix (Variable (Ident n))) _ _)) ->
            when
                (n `elem` ["append", "cap", "complex", "imag", "len", "make",
                            "new", "real"])
                (reportError (a, n ++ " evaluated but not used."))
        _ -> pure ()
    weedExpr e

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
        weedExprAllowingBlanks e = when (not $ isBlankAsOperand e) (weedExpr e)

-- Print statement: weed the inner expressions
weedStmt (Fix (Ann _ (PrintStmt es))) = pure $ (void . map) weedExpr es

-- Return statement with no expression: check that we're in a function that has
-- no return.
weedStmt (Fix (Ann a (ReturnStmt Nothing))) = do
    ret <- gets funcHasReturn
    when (ret) (reportError (a, "not enough arguments to return"))

-- Return statement with an expression: check that we're in a function that has
-- a return, then weed the expression.
weedStmt (Fix (Ann a (ReturnStmt (Just e)))) = do
    ret <- gets funcHasReturn
    when (not ret) (reportError (a, "too many arguments to return"))

    weedExpr e

-- If statement: weed the initializer, expression, then statements and else
-- statements.
weedStmt (Fix (Ann _ (IfStmt init' e thens elses))) = do
    pure $ void (weedStmt <$> init')
    weedExpr e
    void $ mapM weedStmt thens
    void $ pure $ fmap (mapM weedStmt) elses

-- Switch statement: check that there is only one default clause, then weed the
-- initializer, the expression and the clauses.
weedStmt (Fix (Ann a (SwitchStmt init' e clauses))) = do
    pure $ void (weedStmt <$> init')
    pure $ void (weedExpr <$> e)

    let defs = filter isDefaultCase clauses

    when (length defs > 1)
        (reportError (a, "multiple defaults in switch"))

    modify $ \s -> incSwitchLevel s
    void $ mapM ((mapM weedStmt) . snd) clauses
    modify $ \s -> decSwitchLevel s

-- For statement: weed the pre-statement, condition, post-statement and body.
weedStmt (Fix (Ann _ (ForStmt pre cond post body))) = do
    pure $ void (weedStmt <$> pre)
    pure $ void (weedExpr <$> cond)
    void $ pure (weedStmt <$> post)

    modify $ \s -> incForLevel s
    void $ mapM weedStmt body
    modify $ \s -> decForLevel s

-- Break statement: may not occur outside of a for/switch.
weedStmt (Fix (Ann a BreakStmt)) = do
    f <- gets forLevel
    s <- gets switchLevel
    when (f == 0 && s == 0) (reportError (a, "break is not in a loop/switch"))

-- Continue statement: may not occur outside of a for.
weedStmt (Fix (Ann a ContinueStmt)) = do
    f <- gets forLevel
    when (f == 0) (reportError (a, "continue is not in a loop"))

-- Fallthrough statement: may not occur (is not supported)
weedStmt (Fix (Ann a FallthroughStmt)) =
    reportError (a, "fallthrough is unsupported")

-- Block statement: weed inner statements.
weedStmt (Fix (Ann _ (Block b))) = void $ mapM weedStmt b

-- Empty statement: nothing.
weedStmt (Fix (Ann _ EmptyStmt)) = pure ()

-- Case head: for a normal case with expressions, check that none of the
-- expressions are "nil", then weed the expressions. Weed the inner statements.
weedCaseHead :: (SrcAnnCaseHead, [SrcAnnStatement]) -> Weeder ()
weedCaseHead c = do
    case fst c of
        CaseExpr es -> do
            void $ mapM errorIfNil es
            void $ mapM weedExpr es
        _ -> pure ()
    void $ mapM weedStmt (snd c)

-- | Produces an error if the expression is simply \"nil\".
errorIfNil :: SrcAnnExpr -> Weeder ()
errorIfNil e = errorIfIdIs "nil" e "cannot use \"nil\" in this position"

errorIfIdIs :: String -> SrcAnnExpr -> String -> Weeder ()
errorIfIdIs i (Fix (Ann _ (Variable (Ann a (Ident n))))) e =
    when (i == n) (reportError (a, e))
errorIfIdIs _ _ _ = pure ()

isBlankAsOperand :: SrcAnnExpr -> Bool
isBlankAsOperand (Fix (Ann _ (Variable (Ann _ (Ident "_"))))) = True
isBlankAsOperand _ = False

weedExpr :: SrcAnnExpr -> Weeder ()
weedExpr = undefined
