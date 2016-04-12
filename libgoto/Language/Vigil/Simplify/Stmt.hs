{-|
Module      : Language.Vigil.Simplify.Stmt
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Simplifications for statements.
-}

module Language.Vigil.Simplify.Stmt
( simplifyStmt
, realizeToExpr
) where

import Data.Maybe ( catMaybes, fromMaybe )
import Data.List ( partition )
import Data.Tuple ( swap )

import Language.Common.Monad.Traverse
import Language.GoLite.Types as G
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Syntax.Types as G
import Language.GoLite.Syntax.Typecheck as G
import Language.Vigil.Simplify.Core
import Language.Vigil.Simplify.Expr
import Language.Vigil.Syntax as V
import Language.Vigil.Syntax.Basic as V
import Language.Vigil.Syntax.TyAnn as V
import Language.Vigil.Types as V


{- | Simplifies a GoLite statement into a number (potentially some or none) of
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
    phi _ s = case s of
        G.DeclStmt d -> case d of
            G.TypeDecl _ -> pure [] -- Ignore type declarations
            G.VarDecl (VarDeclBody is _ es) -> declareMany is es

        G.ExprStmt e -> do
            (e', stmts) <- realizeToExpr =<< simplifyExpr e
            pure $ stmts ++ [Fix $ V.ExprStmt e']

        G.ShortVarDecl is es -> declareMany is es

        G.Assignment l op r -> case bare op of
            G.Assign -> do
                -- First phase: evaluate to refs on the left, then to temps on
                -- the right. We can't just do vals on the right, because
                -- otherwise something like a, b = b, a wouldn't work.
                lrs <- forM l (\l' -> do
                    es <- simplifyExpr l'
                    case es of
                        [] -> pure Nothing
                        es' -> Just <$> realizeToRefEx es')

                rts <- forM r (\r' -> do
                    (r''@(Ann a' _), s') <- realizeToExpr =<< simplifyExpr r'
                    case maybeGetTemp r'' of
                        Just i -> pure (Ann a' $ Ref $ Ann a' $ ValRef $ IdentVal i, s')
                        Nothing -> do
                            t <- makeTempAndDeclare a'
                            let annTRef = Ann a' $ ValRef $ IdentVal t
                            pure (Ann a' $ Ref $ annTRef,
                                s' ++ [Fix $ V.Assign annTRef r'']))

                let pre = (concat $ map snd $ catMaybes lrs) ++ (concat $ map snd rts)

                -- Second phase: carry out the assignments from left to right
                let asss = map (\(r', e) -> Fix $ V.Assign r' e) $
                        catMaybes $
                            map (fmap swap . sequence) (zip (map fst rts) (map (fmap fst) lrs))

                pure $ pre ++ asss

            _ -> do
                -- A temporary is required for the left side because binary
                -- expressions require values, not refs. At the same time, we
                -- avoid using realizeToVal since we want to assign to this ref
                -- directly.
                (lr@(Ann a' _), ls) <- realizeToRefEx =<< simplifyExpr (head l)

                -- Note that here, we have an assign-op, so what's on the left
                -- can never be a temporary; we don't generate assign-ops ourselves.
                t <- makeTempAndDeclare a'
                let lv = IdentVal t

                let op' = case assignOpToBinOp $ bare op of
                            Nothing -> error "Unknown operator"
                            Just op'' -> gToVBinOp op''

                (rv, rs) <- realizeToVal =<< simplifyExpr (head r)
                pure $ ls
                    ++ rs
                    ++ [Fix $ V.Assign (Ann a' $ ValRef lv) (Ann a' $ Ref lr)]
                    ++ [Fix $ V.Assign lr (Ann a' $ Binary lv op' rv)]


        G.PrintStmt es -> do
            rs <- forM es (\e -> realizeToRef =<< simplifyExpr e)
            pure $ (concat $ map snd rs) ++ [Fix $ V.PrintStmt $ map fst rs]

        G.ReturnStmt me -> case me of
            Nothing -> pure [Fix $ V.ReturnStmt Nothing]
            Just e -> do
                (r, stmts) <- realizeToRef =<< simplifyExpr e
                pure $ stmts ++ [Fix $ V.ReturnStmt $ Just r]

        -- The initializing statement is moved out the if. Any statements
        -- required for the condition occur just before the if, but after the
        -- initializing statement.
        G.IfStmt ini cond thens elses -> do
            ini' <- maybeToListM ini
            (c', cst) <- realizeToCondExpr =<< simplifyExpr cond
            thens' <- flattenBlock thens
            elses' <- pushMaybe $ fmap flattenBlock elses

            pure $ ini' ++ cst ++ [Fix $ V.IfStmt c' thens' elses']

        G.SwitchStmt ini gu cs -> do
            ini' <- maybeToListM ini

            -- Split guard into expression and initializing statements
            gu' <- pushMaybe $ fmap (\e -> realizeToExpr =<< simplifyExpr e) gu
            let guE = fst <$> gu'
            let guS = maybeToList (snd <$> gu')

            let (def, exps) = partition (isDefault . fst) cs
            defCase <- case def of
                [] -> pure []
                (_, b):_ -> flattenBlock b

            -- Every expression in a case head has its own list of statements
            -- in order to compute an expression which is then compared to the
            -- guard.
            exps' <- forM exps $ \(CaseExpr es, b) -> do
                es' <- forM es $ \e -> do
                    (e', s') <- realizeToExpr =<< simplifyExpr e
                    pure $ (e', s')
                b' <- flattenBlock b
                pure (es', b')

            pure $ ini' ++ guS ++ [Fix $ V.SwitchStmt guE exps' defCase]

        G.ForStmt pre cond post bod -> do
            pre' <- maybeToListM pre
            cond' <- pushMaybe $ fmap (\e -> realizeToCondExpr =<< simplifyExpr e) cond
            let condS = maybeToList (snd <$> cond')
            post' <- maybeToListM post
            bod' <- flattenBlock bod

            let condExprStmt = fmap (\(c, _) -> [Fix $ V.CondExprStmt c]) cond'

            pure $ pre' ++ [Fix $ V.ForStmt _ (bod' ++ post')]

        G.IncDecStmt dir e -> do
            -- This is very similar to the assign-op case of the Assignment
            -- statement, except that we create a synthetic right value and
            -- an operator based on the inc/dec direction.
            (r@(Ann a _), s') <- realizeToRefEx =<< simplifyExpr e

            -- As with the assign-op, the expression can never be a temporary.
            t <- makeTempAndDeclare a
            let lv = IdentVal t

            let op = case dir of
                        Increment -> V.Plus
                        Decrement -> V.Minus

            -- We need a synthetic literal with a type that agrees with the type
            -- of the expression.
            lit <- case a of
                    Fix (V.IntType _) -> pure $ Ann a $ V.IntLit 1
                    Fix (V.FloatType _) -> pure $ Ann a $ V.FloatLit 1.0
                    _ -> throwError $ InvariantViolation "IncDec expressions \
                            \should only have type int or float"

            pure $ s'
                ++ [Fix $ V.Assign (Ann a $ ValRef lv) (Ann a $ Ref r)]
                ++ [Fix $ V.Assign r (Ann a $ Binary lv op (V.Literal lit))]

        G.BreakStmt -> pure [Fix V.BreakStmt]

        G.ContinueStmt -> pure [Fix V.ContinueStmt]

        G.Block ss -> flattenBlock ss

        G.EmptyStmt -> pure []

        G.FallthroughStmt ->
            throwError $ InvariantViolation "Unsupported fallthrough statement"

    declareMany :: [G.GlobalId] -> [TySrcAnnExpr] -> Simplify [TyAnnStatement]
    declareMany is es = case es of
        -- If there are no expressions, we just record the declarations, and
        -- don't generate anything.
        [] -> do
            forM is $ \i -> do
                m <- reinterpretGlobalIdEx i
                case m of
                    Nothing -> pure ()
                    Just i' -> do
                        modify $ \s ->
                            s {newDeclarations = i':(newDeclarations s) }
            pure []
        -- If there are expressions, we perform some initialization as well.
        _ -> do
            fmap concat $ forM (zip is es) $ \(i, e) -> do
                declareAndInit i =<< simplifyExpr e

    isDefault c = case c of
        CaseDefault -> True
        _ -> False

-- | Create and declare a temporary variable.
makeTempAndDeclare :: V.Type -> Simplify V.BasicIdent
makeTempAndDeclare ty = do
    t <- makeTemp ty
    modify (\s -> s {newDeclarations = t:(newDeclarations s) })
    pure t

-- | Replaces a Maybe list value by Just its contents or an empty list if it's
-- Nothing.
maybeToList :: Maybe [a] -> [a]
maybeToList = fromMaybe []

-- | A combination of "pushMaybe" and "maybeToList".
maybeToListM :: Monad m => Maybe (m [a]) -> m [a]
maybeToListM x = do
    x' <- pushMaybe x
    pure $ maybeToList x'

-- | Pushes a Maybe of a monadic value inside the monad.
pushMaybe :: Monad m => Maybe (m a) -> m (Maybe a)
pushMaybe = sequence

-- | Flattens a simplified block into one monadic "Simplify" action.
flattenBlock :: [Simplify [TyAnnStatement]] -> Simplify [TyAnnStatement]
flattenBlock b = concat <$> sequence b

-- | Declares the given identifier, and returns the statements required to
-- initialize it.
declareAndInit :: G.GlobalId
                -> [SimpleExprResult]
                -> Simplify [TyAnnStatement]
declareAndInit i e = do
    (e', s') <- realizeToExpr e
    m <- reinterpretGlobalIdEx i
    case m of
        Nothing -> pure $ s' ++ [Fix $ V.ExprStmt e']
        Just i' -> do
            modify $ \s -> s {newDeclarations = i':(newDeclarations s) }
            pure $ s' ++ [Fix $ V.Assign (Ann (gidTy i') $ ValRef $ IdentVal i') e']

-- | Realizes a simplified expression stack and all its temporaries, taking the
-- topmost result into a val. The way this is done is to create a temporary
-- and assign to it whenever the result is not a val already.
realizeToVal :: [SimpleExprResult]
                -> Simplify (TyAnnVal, [TyAnnStatement])
realizeToVal rs = do
    (re, stmts) <- realizeTemps rs
    case re of
        SimpleVal v -> pure (v , stmts)
        _ -> do
            t <- makeTempAndDeclare (typeFromSimple re)
            let i = IdentVal t
            pure (i, stmts ++ [Fix $ V.Assign (Ann (gidTy t) $ ValRef i) (wrapExpr re)])
    where
        wrapExpr e = case e of
            SimpleExpr e' -> e'
            SimpleRef r -> Ann (typeFromSimple e) $ Ref r
            _ -> error "Entirely unexpected"

-- | Realizes a simplified expression stack and all its temporaries, taking the
-- topmost result into a ref (with an appropriate temporary as required).
realizeToRef :: [SimpleExprResult]
                -> Simplify (TyAnnRef, [TyAnnStatement])
realizeToRef rs = do
    (r, stmts) <- realizeTemps rs
    case r of
        SimpleVal v -> pure (Ann (typeFromVal v) $ ValRef v, stmts)
        SimpleRef rf -> pure (rf, stmts)
        SimpleExpr e@(Ann a _) -> do
            t <- makeTempAndDeclare a
            let vt = Ann (gidTy t) $ ValRef $ IdentVal t in
                pure (vt, (Fix $ V.Assign vt e):stmts)

-- | Same as "realizeToRef", but throws an error when the topmost result is an
-- expression. This guarantees that the reference returned is not a temporary,
-- which is required for assignments. Throwing an exception in this case is
-- valid, since something like @(a + b) = 3@ should have been caught earlier on
-- as a non-addressable expression.
realizeToRefEx :: [SimpleExprResult]
                -> Simplify (TyAnnRef, [TyAnnStatement])
realizeToRefEx rs =  do
    (r, stmts) <- realizeTemps rs
    case r of
        SimpleVal v -> pure (Ann (typeFromVal v) $ ValRef v, stmts)
        SimpleRef rf -> pure (rf, stmts)
        SimpleExpr _ ->
            throwError $ InvariantViolation "Cannot realize expression as ref \
                                            \without a temporary"

-- | Realizes a simplified expression stack and all its temporaries, taking the
-- topmost result into a full expression if it was not already the case. No new
-- temporaries are created by this function.
realizeToExpr :: [SimpleExprResult]
                -> Simplify (TyAnnExpr, [TyAnnStatement])
realizeToExpr rs = do
    (re, stmts) <- realizeTemps rs
    case re of
        SimpleVal v -> pure (Ann (typeFromVal v) $ Ref
                           $ Ann (typeFromVal v) $ ValRef v, stmts)
        SimpleRef r -> pure (Ann (typeFromSimple re) $ Ref r, stmts)
        SimpleExpr e -> pure (e, stmts)

-- | If the given expression is a temporary, returns Just its identifier.
-- Otherwise return Nothing.
maybeGetTemp :: TyAnnExpr -> Maybe BasicIdent
maybeGetTemp (Ann a' e) =
    let isTemp = (== "%tmp") . take 4 . stringFromSymbol . gidOrigName in
    let maybeTemp i = if isTemp i then Just i else Nothing in
    case e of
        Ref (Ann _ (ValRef (IdentVal i))) -> maybeTemp i
        Cond (CondRef (Ann _ (ValRef (IdentVal i)))) -> maybeTemp i
        _ -> Nothing

-- | Same as "realizeToExpr", but produces a conditional expression. If the
-- result is in fact a normal expression, an error is thrown. No new temporaries
-- are created by this function.
realizeToCondExpr :: [SimpleExprResult]
                    -> Simplify (TyAnnCondExpr, [TyAnnStatement])
realizeToCondExpr rs = do
    (re, stmts) <- realizeTemps rs
    case re of
        SimpleVal v -> pure (CondRef $ Ann (typeFromVal v) $ ValRef v, stmts)
        SimpleRef r -> pure (CondRef r, stmts)
        SimpleExpr (Ann _ (Cond c)) -> pure (c, stmts)
        -- Assuming that the typechecker did a proper job, it should be
        -- fine to assign this to a temp and check the temp as a condref.
        SimpleExpr e@(Ann a _) -> do
            t <- makeTempAndDeclare a
            pure (CondRef $ Ann a $ ValRef $ IdentVal t ,
                    (Fix $ V.Assign (Ann a $ ValRef $ IdentVal t) e):stmts)

-- | Takes a simplified expression stack and realizes all the temporaries as
-- assign statements, also returning the topmost result on its own. As a
-- side-effect, records the fact that the temporaries should be declared.
realizeTemps :: [SimpleExprResult]
                -> Simplify (SimpleConstituent, [TyAnnStatement])
realizeTemps rs' = do
    let eRes = head rs'
    case eRes of
        Temp _ -> throwError $ InvariantViolation "Unexpected temporary as final result."
        ShortCircuit _ _ _ _ -> realizeShortCircuit eRes
        Result r -> do
            stmts <- forM (reverse $ tail rs') (\ser -> case ser of
                Temp (i, c) -> do
                    modify (\s -> s {newDeclarations = i:(newDeclarations s) })
                    -- Build the expression to assign
                    let cTy = typeFromSimple c
                    let tex = case c of
                                SimpleExpr e -> e
                                SimpleRef ref -> (Ann cTy $ Ref ref)
                                SimpleVal v -> (Ann cTy $ Ref $ Ann cTy $ ValRef v)
                    pure [Fix $ V.Assign (Ann (gidTy i) $ ValRef $ IdentVal i) tex]

                -- realizeShortCircuit already declares the temporary for us.
                ShortCircuit _ _ _ _ -> snd <$> realizeShortCircuit ser

                _ -> throwError $ InvariantViolation "Unexpected value as \
                                                    \intermediary ")
            pure (r, concat stmts)

-- | Realizes a short-circuit expression into a full-fledged control-flow
-- structure that provides the short-circuiting behavior.
realizeShortCircuit :: SimpleExprResult -> Simplify (SimpleConstituent, [TyAnnStatement])
realizeShortCircuit (ShortCircuit t op ls rs) = do
    modify (\s -> s {newDeclarations = t:(newDeclarations s) })

    (le, ls') <- realizeToCondExpr ls
    (re, rs') <- realizeToCondExpr rs
    let tRef = Ann V.boolType $ ValRef $ IdentVal t
    let assl = Fix $ V.Assign tRef (Ann V.boolType $ Cond le)

    -- The type of check differs whether we have a conjunction or disjunction.
    let cond = (case op of
            And -> CondRef tRef
            Or -> UnCond V.LogicalNot (IdentVal t)) :: TyAnnCondExpr
    let bodIf = rs' ++ [Fix $ V.Assign tRef (Ann V.boolType $ Cond re)]

    pure $ (SimpleVal $ IdentVal t, ls' ++ [assl, Fix $ V.IfStmt cond bodIf Nothing])

realizeShortCircuit _ = throwError $ InvariantViolation $ "realizeShortCircuit \
    \called with something that is not a short-circuit"
