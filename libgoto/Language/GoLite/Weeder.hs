{-|
Module      : Language.GoLite.Weeder
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

This module contains the "Weeder" traversal for packages and top-level
declarations.
-}

module Language.GoLite.Weeder (
  weed
, weedPackage
, weedTopLevelDecl
, weedFunDecl
, Weeder ( .. )
, WeederState ( .. )
, WeederException( .. )
, WeederExceptions( .. )
) where

import Language.GoLite.Weeder.Core
import Language.GoLite.Weeder.Stmt
import Language.GoLite.Weeder.TypeLit

-- Weeds a program, returning a list of weeding errors encountered.
weed :: SrcAnnPackage -> [WeederException]
weed p =
    let start = WeederState { funcHasReturn = False
                            , forLevel = 0
                            , switchLevel = 0
                            , weedErrors = []} in
    let runToState = (runStateT . runExceptT . runTraversal . runWeeder) in
    weedErrors $ snd $ runIdentity $ runToState (weedPackage p) start

-- | Weeds a package and its components. A package is invalid if its identifier
-- is the blank one.
weedPackage :: SrcAnnPackage -> Weeder ()
weedPackage (Package i tlds) = do
    errorOnBlankIdentifier i "Package name may not be the blank identifier."

    void $ mapM weedTopLevelDecl tlds

-- | Weeds a top-level declaration: either a variable/type declaration or a
-- function declaration.
weedTopLevelDecl :: SrcAnnTopLevelDecl -> Weeder ()
weedTopLevelDecl (TopLevelDecl d) = weedDecl d
weedTopLevelDecl (TopLevelFun f) = weedFunDecl f

-- | Weeds a function declaration. Functions with declared return types must end
-- with a terminating statement.
weedFunDecl :: SrcAnnFunDecl -> Weeder ()
weedFunDecl (FunDecl (Ann a _) pars rty bod) = do

    when (isJust rty
        && (bod == [] || (not $ isTerminating $ bareStmt (last bod))))
        (reportError $ WeederException a "missing return at end of function")

    weedFields pars
    case rty of
        Nothing -> pure ()
        Just ty -> weedType ty

    modify $ \s -> s { funcHasReturn = (isJust rty) }
    void $ mapM weedStmt bod


{- | Checks if a statement is terminating.

    * A return statement is terminating.
    * A block is terminating if it ends with a terminating statement.
    * An if statement is terminating if both branches are present and both end
      with a terminating statement.
    * A for loop is terminating if it has no condition and no break that refers
      to it.
    * A switch statement is terminating if:

        * It has a default case.
        * No case contains a break.
        * Every case ends in a terminating statement.

    * No other statement is terminating.
-}
isTerminating :: BasicStatement -> Bool
isTerminating (Fix (ReturnStmt (Just _))) = True
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
