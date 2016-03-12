{-|
Module      : Language.GoLite.Weeder
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

This module contains the Weeder traversal, which throws errors on semantically
invalid programs.
-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.GoLite.Weeder where

import Language.GoLite.Monad.Traverse
import Language.GoLite.Syntax
import Language.GoLite.Syntax.SrcAnn

import Data.Void ( Void )

-- TODO
type WeederException = (SrcSpan, String)

-- TODO
type WeederState = String

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

    reportError = undefined -- TODO add to list of errors in state

    getErrors = undefined -- TODO extract list of errors from state


-- | Weeds a package and its components. A package is invalid if its identifier
-- is the blank one.
weedPackage :: SrcAnnPackage -> Weeder ()
weedPackage (Package (Ann a (Ident n)) tlds) = do
    case n of
        "_" -> reportError (a, "Package name may not be the blank identifier.")
        _ -> pure ()
    pure $ (void . map) weedTopLevelDecl tlds

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
    case n of
        "init" ->   if pars /= [] || rty /= Nothing then
                        reportError (a, "func init must have no arguments and \
                                    \no return values")
                    else pure ()
        _ -> pure ()

    case rty of
        (Just _) -> if bod == [] || (not $ isTerminating $ bareStmt (last bod)) then
                        reportError (a, "missing return at end of function")
                    else pure ()
        _ -> pure ()

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


-- Weeds a declaration (either variable or type)
weedDecl :: SrcAnnDeclaration -> Weeder ()
weedDecl = undefined
