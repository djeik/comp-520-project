{-|
Module      : Language.Vigil.Simplify.Stmt
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Simplifications for statements
-}

module Language.Vigil.Simplify.Stmt
( simplifyStmt
) where

import Data.List ( partition )

import Language.Common.Monad.Traverse
import Language.GoLite.Syntax.SrcAnn
import Language.Vigil.Simplify.Core
import Language.Vigil.Simplify.Expr
import Language.GoLite.Types as G
import Language.GoLite.Syntax.Types as G
import Language.GoLite.Syntax.Typecheck as G
import Language.Vigil.Syntax as V
import Language.Vigil.Syntax.Basic as V
import Language.Vigil.Syntax.TyAnn as V

{- | Simplifies a GoLite statement into a number (potentially some or none)
   Vigil statements.

    * For expressions throughout, any temporaries needed are realized as
      declarations and assignements.
    * Declarations are recorded in the state of the traversal, and replaced by
      an assignment if they have an initializing expression.
    * Expressions in return statements are replaced by refs as required.
    * In all compound statements (if, for, switch), initializers are pulled out
      as statements of their own. Expressions are made as conditional
      expressions.
    * The post statement of for statements is appended to the end of its body.
    * Blocks are flattened.
-}
simplifyStmt :: TySrcAnnStatement -> Simplify [TyAnnStatement]
simplifyStmt = annCata phi where
    phi :: SrcSpan
        -> TySrcAnnStatementF (Simplify [TyAnnStatement])
        -> Simplify [TyAnnStatement]
    phi a s = case s of
        G.DeclStmt d -> case d of
            G.TypeDecl _ -> pure [] -- Ignore type declarations
            G.VarDecl (VarDeclBody is _ es) -> declareMany is es

        G.ExprStmt e -> do
            (e', stmts) <- realizeToExpr $ simplifyExpr e
            pure $ stmts ++ [Fix $ V.ExprStmt e']

        G.ShortVarDecl is es -> declareMany is es

        G.Assignment l op r -> case bare op of
            G.Assign -> do
                -- First phase: evaluate to refs on the left, then to temps on
                -- the right. We can't just do vals on the right, because
                -- otherwise something like a, b = b, a wouldn't work.
                lrs <- forM l (realizeToRefEx . simplifyExpr)
                rts <- forM r (\r' -> do
                    (r'', s) <- realizeToExpr $ simplifyExpr r'
                    t <- makeTempAndDeclare ()
                    pure (Ref $ ValRef $ IdentVal t,
                            s ++ [Fix $ V.Assign (ValRef $ IdentVal t) r'']))

                let pre = (concat $ map snd lrs) ++ (concat $ map snd rts)

                -- Second phase: carry out the assignments from left to right
                let asss = map (\(r, e) -> Fix $ V.Assign r e)
                            (zip (map fst lrs) (map fst rts))

                pure $ pre ++ asss

            _ -> do
                -- A temporary is required for the left side because binary
                -- expressions require values, not refs. At the same time, we
                -- avoid using realizeToVal since we want to assign to this ref
                -- directly.
                (lr, ls) <- realizeToRefEx $ simplifyExpr $ head l
                t <- makeTempAndDeclare ()
                let lv = IdentVal t

                let op' = case assignOpToBinOp $ bare op of
                            Nothing -> error "Unknown operator"
                            Just op' -> gToVBinOp op'

                (rv, rs) <- realizeToVal $ simplifyExpr $ head r
                pure $ ls
                    ++ rs
                    ++ [Fix $ V.Assign (ValRef lv) (Ref lr)]
                    ++ [Fix $ V.Assign lr (Binary lv op' rv)]
            -- Regular assignment: as you'd expect
            -- Also note: the top of the l stack shouldn't be an expr. If it's
            -- an expr then it's not a ref, and that means it wasn't addressable
            -- in the first place.


        G.PrintStmt es -> do
            rs <- forM es (realizeToRef . simplifyExpr)
            pure $ (concat $ map snd rs) ++ [Fix $ V.PrintStmt $ map fst rs]

        G.ReturnStmt me -> case me of
            Nothing -> pure [Fix $ V.ReturnStmt Nothing]
            Just e -> do
                (r, stmts) <- realizeToRef $ simplifyExpr e
                pure $ stmts ++ [Fix $ V.ReturnStmt $ Just r]

        -- The initializing statement is moved out the if. Any statements
        -- required for the condition occur just before the if, but after the
        -- initializing statement.
        G.IfStmt ini cond thens elses -> do
            ini' <- maybeToListM ini
            (c', cst) <- realizeToCondExpr $ simplifyExpr cond
            thens' <- flattenBlock thens
            elses' <- pushMaybe $ fmap flattenBlock elses

            pure $ ini' ++ cst ++ [Fix $ V.IfStmt c' thens' elses']

        G.SwitchStmt ini gu cases -> do
            ini' <- maybeToListM ini

            -- Split guard into expression and initializing statements
            gu' <- pushMaybe $ fmap (realizeToExpr . simplifyExpr) gu
            let guE = fst <$> gu'
            let guS = maybeToList (snd <$> gu')

            let (def, exps) = partition (isDefault . fst) cases
            defCase <- case def of
                [] -> pure []
                (_, b):_ -> flattenBlock b

            -- Every expression in a case head has its own list of statements
            -- in order to compute an expression which is then compared to the
            -- guard.
            exps' <- forM exps (\(CaseExpr es, b) -> do
                es' <- forM es (\e -> do
                    (e', s) <- realizeToExpr $ simplifyExpr e
                    pure $ s ++ [Fix $ V.ExprStmt e'])
                b' <- flattenBlock b
                pure (es', b'))

            pure $ ini' ++ guS ++ [Fix $ V.SwitchStmt guE exps' defCase]

        G.ForStmt pre cond post bod -> do
            pre' <- maybeToListM pre
            cond' <- pushMaybe $ fmap (realizeToCondExpr . simplifyExpr) cond
            let condS = maybeToList (snd <$> cond')
            post' <- maybeToListM post
            bod' <- flattenBlock bod
            pure $ pre' ++ condS ++ [Fix $ V.ForStmt (fst <$> cond') (bod' ++ post')]

        G.IncDecStmt dir e -> do
            -- This is very similar to the assign-op case of the Assignment
            -- statement, except that we create a synthetic right value and
            -- an operator based on the inc/dec direction.
            (r, s) <- realizeToRefEx $ simplifyExpr e
            t <- makeTempAndDeclare ()
            let lv = IdentVal t

            let op = case dir of
                        Increment -> V.Plus
                        Decrement -> V.Minus

            pure $ s
                ++ [Fix $ V.Assign (ValRef lv) (Ref r)]
                ++ [Fix $ V.Assign r (Binary lv op (V.Literal undefined))]
            -- TODO We need an actual value here...


        G.BreakStmt -> pure [Fix V.BreakStmt]

        G.ContinueStmt -> pure [Fix V.ContinueStmt]

        G.Block ss -> flattenBlock ss

        G.EmptyStmt -> pure []

        G.FallthroughStmt ->
            throwError $ InvariantViolation "Unsupported fallthrough statement"

    declareMany is es = case es of
        -- If there are no expressions, we just record the declarations, and
        -- don't generate anything.
        [] -> forM is (\i ->
                modify (\s ->
                    s {newDeclarations = (gIdToVIdent i):(newDeclarations s) }))
                >> pure []
        -- If there are expressions, we perform some initialization as well.
        _ -> do
            let inis = forM (zip is es)
                    (\(i, e) -> declareAndInit i (simplifyExpr e))
            concat <$> inis

    isDefault c = case c of
        CaseDefault -> True
        _ -> False

-- | Create and declare a temporary variable.
makeTempAndDeclare :: a -> Simplify V.BasicIdent
makeTempAndDeclare _ = do
    t <- makeTemp ()
    modify (\s -> s {newDeclarations = t:(newDeclarations s) })
    pure t

-- | Replaces a Maybe list value by Just its contents or an empty list if it's
-- Nothing.
maybeToList :: Maybe [a] -> [a]
maybeToList x = case x of
    Nothing -> []
    Just a -> a

-- | A combination of "pushMaybe" and "maybeToList".
maybeToListM :: Monad m => Maybe (m [a]) -> m [a]
maybeToListM x = do
    x' <- pushMaybe x
    pure $ maybeToList x'

-- | Pushes a Maybe of a monadic value inside the monad.
pushMaybe :: Monad m => Maybe (m a) -> m (Maybe a)
pushMaybe x = case x of
    Nothing -> pure Nothing
    Just a -> do
        a' <- a
        pure $ Just a'

-- | Flattens a simplified block into one monadic "Simplify" action.
flattenBlock :: [Simplify [TyAnnStatement]] -> Simplify [TyAnnStatement]
flattenBlock b = concat <$> sequence b

-- | Declares the given identifier, and returns the statements required to
-- initialize it.
declareAndInit :: G.GlobalId
                -> Simplify [SimpleExprResult]
                -> Simplify [TyAnnStatement]
declareAndInit i e = do
    let i' = gIdToVIdent i
    modify (\s -> s {newDeclarations = i':(newDeclarations s) })
    (e, s) <- realizeToExpr e
    pure $ s ++ [Fix $ V.Assign (ValRef $ IdentVal i') e]

-- | Realizes a simplified expression stack and all its temporaries, taking the
-- topmost result into a val. The way this is done is to create a temporary
-- and assign to it whenever the result is not a val already.
realizeToVal :: Simplify [SimpleExprResult]
                -> Simplify (TyAnnVal, [TyAnnStatement])
realizeToVal rs = do
    (re, stmts) <- realizeTemps rs
    case re of
        SimpleVal v -> pure (v , stmts)
        _ -> do
            t <- makeTempAndDeclare ()
            let i = IdentVal t
            pure (i, stmts ++ [Fix $ V.Assign (ValRef i) (wrapExpr re)])
    where
        wrapExpr e = case e of
            SimpleExpr e' -> e'
            SimpleRef r -> Ref r
            _ -> error "Entirely unexpected"

-- | Realizes a simplified expression stack and all its temporaries, taking the
-- topmost result into a full expression if it was not already the case. No new
-- temporaries are created by this function.
realizeToExpr :: Simplify [SimpleExprResult]
                -> Simplify (TyAnnExpr, [TyAnnStatement])
realizeToExpr rs = do
    (re, stmts) <- realizeTemps rs
    case re of
        SimpleVal v -> pure (Ref $ ValRef v, stmts)
        SimpleRef r -> pure (Ref r, stmts)
        SimpleExpr e -> pure (e, stmts)


-- | Same as "realizeToExpr", but produces a conditional expression. If the
-- result is in fact a normal expression, an error is thrown. No new temporaries
-- are created by this function.
realizeToCondExpr :: Simplify [SimpleExprResult]
                    -> Simplify (TyAnnCondExpr, [TyAnnStatement])
realizeToCondExpr rs = do
    (re, stmts) <- realizeTemps rs
    case re of
        SimpleVal v -> pure (CondRef $ ValRef v, stmts)
        SimpleRef r -> pure (CondRef r, stmts)
        SimpleExpr (Cond c) -> pure (c, stmts)
        _ -> throwError $ InvariantViolation "Cannot realize a non-conditional \
                                        \expression as a conditional expression"

-- | Realizes a simplified expression stack and all its temporaries, taking the
-- topmost result into a ref (with an appropriate temporary as required).
realizeToRef :: Simplify [SimpleExprResult]
                -> Simplify (TyAnnRef, [TyAnnStatement])
realizeToRef rs = do
    (r, stmts) <- realizeTemps rs
    case r of
        SimpleVal v -> pure (ValRef v, stmts)
        SimpleRef r -> pure (r, stmts)
        SimpleExpr e -> do
            t <- makeTempAndDeclare ()
            let vt = ValRef $ IdentVal t in
                pure (vt, (Fix $ V.Assign vt e):stmts)

-- | Same as "realizeToRef", but throws an error when the topmost result is an
-- expression. This guarantees that the reference returned is not a temporary,
-- which is required for assignments. Throwing an exception in this case is
-- valid, since something like @(a + b) = 3@ should have been caught earlier on
-- as a non-addressable expression.
realizeToRefEx :: Simplify [SimpleExprResult]
                -> Simplify (TyAnnRef, [TyAnnStatement])
realizeToRefEx rs =  do
    (r, stmts) <- realizeTemps rs
    case r of
        SimpleVal v -> pure (ValRef v, stmts)
        SimpleRef r -> pure (r, stmts)
        SimpleExpr _ ->
            throwError $ InvariantViolation "Cannot realize expression as ref \
                                            \without a temporary"

-- | Takes a simplified expression stack and realizes all the temporaries as
-- assign statements, also returning the topmost result on its own. As a
-- side-effect, records the fact that the temporaries should be declared.
realizeTemps :: Simplify [SimpleExprResult]
                -> Simplify (SimpleConstituent, [TyAnnStatement])
realizeTemps rs = do
    rs' <- rs
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

    let eRes = head rs'
    case eRes of
        Temp _ -> throwError $ InvariantViolation "Unexpected temporary as final result."
        Result r -> pure (r, stmts)
