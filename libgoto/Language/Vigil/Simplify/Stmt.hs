{-|
Module      : Language.Vigil.Simplify.Stmt
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Simplifications for statements
-}

module Language.Vigil.Simplify.Stmt where

import Language.Common.Monad.Traverse
import Language.GoLite.Syntax.SrcAnn
import Language.Vigil.Simplify.Core
import Language.Vigil.Simplify.Expr
import Language.GoLite.Syntax.Types as G
import Language.Vigil.Syntax as V
import Language.Vigil.Syntax.TyAnn as V

simplifyStmt :: TySrcAnnStatement -> Simplify [TyAnnStatement]
simplifyStmt = annCata phi where
    phi :: SrcSpan
        -> TySrcAnnStatementF (Simplify [TyAnnStatement])
        -> Simplify [TyAnnStatement]
    phi a s = case s of
        G.DeclStmt d -> undefined
        G.ExprStmt e -> undefined
        G.ShortVarDecl is es -> undefined
        G.Assignment l op r -> undefined
        G.PrintStmt es -> undefined
        G.ReturnStmt e -> undefined
        G.IfStmt ini cond thens elses -> undefined
        G.SwitchStmt ini cond cases -> undefined
        G.ForStmt pre cond post bod -> undefined

        G.IncDecStmt dir e -> undefined -- do
            -- Simplify the expr
            -- Create: a = t; t = t (+/-) 1

        G.BreakStmt -> pure [Fix V.BreakStmt]

        G.ContinueStmt -> pure [Fix V.ContinueStmt]

        G.Block ss -> undefined

        G.EmptyStmt -> pure []

        G.FallthroughStmt ->
            throwError $ InvariantViolation "Unsupported fallthrough statement"

-- | Takes a simplified expression stack and realizes all the temporaries as
-- assign statements, also returning the topmost result on its own. As a
-- side-effect, records the fact that the temporaries should be declared.
realizeTemps :: Simplify [SimpleExprResult]
                -> Simplify (SimpleExprResult, [TyAnnStatement])
realizeTemps rs = do
    rs' <- rs
    let eRes = head rs'
    stmts <- forM (reverse $ tail rs') (\ser -> case ser of
        Result _ -> throwError $ InvariantViolation "Unexpected result as intermediary"
        Temp (i, c) -> do
            modify (\s -> s {newDeclarations = i:(newDeclarations s) })
            -- Build the expression to assign
            let tex = case c of
                        SimpleExpr e -> e
                        SimpleRef r -> Ref r
                        SimpleVal v -> Ref $ ValRef v
            pure $ Fix $ V.Assign (ValRef $ IdentVal i) tex)

    pure (eRes, stmts)
