{-|
Module      : Language.Vigil.Simplify.Expr
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Simplifications for expressions.
-}

module Language.Vigil.Simplify.Expr
( SimpleExprResult(..)
, SimpleConstituent(..)
, simplifyExpr
) where

import Language.GoLite.Syntax.Types as G
import Language.GoLite.Types as T
import Language.Vigil.Simplify.Core
import Language.Vigil.Syntax as V
import Language.Vigil.Syntax.Basic as V
import Language.Vigil.Syntax.TyAnn as V

import Control.Applicative

data SimpleExprResult
    = Result SimpleConstituent
    | Temp (V.BasicIdent, SimpleConstituent)

-- | Simplifying an expression could result either in a value, a reference, or a
-- Vigil expression. We always try to reduce to the simplest possible form, so
-- for example, the GoLite expression @a@ would become a Vigil value.
data SimpleConstituent
    = SimpleExpr TyAnnExpr
    | SimpleRef TyAnnRef
    | SimpleVal TyAnnVal

-- | Simplifies a full-fledged GoLite expression into a (potentially some) Vigil
-- expressions.
--
-- Generated temporary sub-expressions are accumulated in a stack. The stack is
-- percolated up to the top of the expression tree, with each node potentially
-- adding more onto it.
--
-- Note that it is assumed that after any given simplification, the top of the
-- stack will not be a temporary or a "ValRef". @ValRef@s are created at a later
-- stage.
simplifyExpr :: TySrcAnnExpr -> Simplify [SimpleExprResult]
simplifyExpr = annCata phi where
    phi :: TySrcSpan
        -> TySrcAnnExprF (Simplify [SimpleExprResult])
        -> Simplify [SimpleExprResult]
    phi _ ex =
        case ex of
        G.BinaryOp o l r -> do
            (vl, esl) <- exprAsVal l
            (vr, esr) <- exprAsVal r
            let es = esl ++ esr
            -- Should this be a conditional or a binary expression?
            let o' = bare o in pure $
                (if isComparisonOp o' || isOrderingOp o' || isLogicalOp o' then
                    Result $ SimpleExpr $ Cond $ BinCond vl (gToVCondOp o') vr
                else
                    Result $ SimpleExpr $ Binary vl (gToVBinOp o') vr):es

        G.UnaryOp o e -> do
            (v, e') <- exprAsVal e

            pure $ (case bare o of
                G.LogicalNot -> Result $ SimpleExpr $ Cond $ UnCond V.LogicalNot v
                _ -> Result $ SimpleExpr $ Unary (gToVUnOp $ bare o) v):e'

        G.Conversion ty e -> do
            e' <- e
            let ty' = gToVType ty
            case head e' of
                -- In the case of a ref, promote it directly to a conversion.
                (Result (SimpleRef r)) ->
                    pure $ (Result $ SimpleExpr $ V.Conversion ty' r):(tail e')
                -- In the case of a value, make it into a ref and convert that.
                (Result (SimpleVal v)) ->
                    pure $ (Result
                            $ SimpleExpr
                            $ V.Conversion ty' $ ValRef v)
                            :(tail e')
                -- Otherwise, create a temporary and convert the temporary.
                (Result e''@_) -> do
                    t <- makeTemp ()
                    pure $ (Result
                            $ SimpleExpr
                            $ V.Conversion ty' $ ValRef $ IdentVal t)
                            :(Temp (t, e''))
                            :(tail e')
                _ -> throwError $ InvariantViolation "Unexpected non-result"

        G.Selector e id' -> do
            e' <- e
            case e' of
                ((Result e''):es) -> case e'' of
                    -- If we already have a select ref (e.g. stru.foo.bar),
                    -- we just extend it with the new identifier
                    SimpleRef (SelectRef i is) ->
                        pure $ (Result $ SimpleRef $ SelectRef i
                            (is ++ [gToVIdent $ bare id'])):es
                    -- An ident val: replace by a select ref
                    SimpleVal (IdentVal i) ->
                        pure $ (Result $ SimpleRef $ SelectRef i
                            [gToVIdent $ bare id']):es
                    -- Anything else: create a temp, select into it.
                    _ -> do
                        t <- makeTemp ()
                        pure $ (Result $ SimpleRef $ SelectRef
                            t [gToVIdent $ bare id'])
                            :(Temp (t, e''))
                            :es
                _ -> throwError $ InvariantViolation "Unexpected non-result"

        G.Index eIn eBy' -> do
            eIn' <- eIn

            (vBy, esBy) <- exprAsVal eBy'

            let es = esBy ++ (tail eIn')
            case head eIn' of
                -- If we have an array ref, extend it.
                (Result (SimpleRef (ArrayRef i vs))) ->
                    pure $ (Result $ SimpleRef $ ArrayRef i (vs ++ [vBy])):es
                -- If we have an identifier value, replace it by an array ref.
                (Result (SimpleVal (IdentVal i))) ->
                    pure $ (Result $ SimpleRef $ ArrayRef i [vBy]):es
                -- Otherwise, create a temp and index into it.
                (Result eIn''@_) -> do
                    t <- makeTemp ()
                    pure $ (Result $ SimpleRef $ ArrayRef t [vBy])
                            :(Temp (t, eIn''))
                            :es
                _ -> throwError $ InvariantViolation "Unexpected non-result"

        G.Slice e l h b -> do
            e' <- e
            let ml = exprAsVal <$> l
            let mh = exprAsVal <$> h
            let mb = exprAsVal <$> b

            il <- extractI ml
            ih <- extractI mh
            ib <- extractI mb
            let bounds = (il, ih, ib)

            el <- extractEs ml
            eh <- extractEs mh
            eb <- extractEs mb
            let es = el ++ eh ++ eb ++ (tail e')

            case head e' of
                -- Slicing an existing slice ref: extend it
                (Result (SimpleRef (SliceRef i bs))) ->
                    pure $ (Result $ SimpleRef $ SliceRef i (bs ++ [bounds])):es
                -- Slicing an identifier: replace by a slice ref.
                (Result (SimpleVal (IdentVal i))) ->
                    pure $ (Result $ SimpleRef $ SliceRef i [bounds]):es
                -- Slicing into anything else: create a temp and slice that.
                (Result e''@_) -> do
                    t <- makeTemp ()
                    pure $ (Result $ SimpleRef $ SliceRef t [bounds])
                            :(Temp (t, e''))
                            :es
                _ -> throwError $ InvariantViolation "Unexpected non-result"

        G.Call e _ ps  -> do
            -- Convert top expressions of parameters to values when needed.
            ps' <- forM ps (\cur -> do
                cur' <- cur
                case head cur' of
                    (Result (SimpleVal _)) -> pure $ cur'
                    (Result e'@_) -> do
                        t <- makeTemp ()
                        pure $ (Temp (t, e')):cur'
                    _ -> throwError $ InvariantViolation "Unexpected non-result")

            -- Make top values to be used as arguments to the call.
            let ps'' = map (\cur -> case head cur of
                    (Result (SimpleVal v)) -> v
                    (Temp (t, _)) -> IdentVal t
                    _ -> error "Expected simple value or temp") ps'

            -- If the callee expression is an identifier, use it as is. Otherwise
            -- create a temporary.
            (i, es) <- exprAsId e

            -- At this point ps' has either simple values or temps at the top.
            -- We keep the temps, and throw away the simple values since they
            -- made it directly into the call. For the temps, we merge all their
            -- prior expressions. Note that simple values can't have prior
            -- expressions so it's okay to throw them out.
            es' <- let fil = (\x -> case head x of
                        Result _ -> False
                        _ -> True) in pure $ concat $ filter fil ps'

            pure $ (Result $ SimpleExpr $ V.Call i ps''):(es ++ es')

        G.Literal (Ann a' l) ->
            pure [Result $ SimpleVal $ V.Literal $ (Ann (fst a') $ gToVLit l)]

        G.Variable (T.GlobalId { T.gidOrigName = Ann _ i }) ->
            pure [Result $ SimpleVal $ IdentVal $ gToVIdent i]

        G.TypeAssertion _ _ ->
            throwError $ InvariantViolation "Type assertions are not supported."

    -- Extracts a value from a
    extractI :: Maybe (Simplify (TyAnnVal, a)) -> Simplify (Maybe TyAnnVal)
    extractI x = case x of
        Nothing -> pure Nothing
        Just a -> do
            a' <- a
            pure $ Just $ fst a'

    -- Extracts a stack from a Maybe Simplify value. When the argument is nothing,
    -- an empty list is returned.
    extractEs :: Maybe (Simplify (a, [SimpleExprResult]))
                -> Simplify [SimpleExprResult]
    extractEs x = case x of
        Nothing -> pure []
        Just a -> do
            a' <- a
            pure $ snd a'

    -- Takes the topmost expression of a simple stack to a value. A temporary
    -- is generated if required, and the resulting stack state after is also
    -- returned.
    exprAsVal :: Simplify [SimpleExprResult]
                -> Simplify (TyAnnVal, [SimpleExprResult])
    exprAsVal e = do
        e' <- e
        case head e' of
            Result (SimpleVal v) -> pure (v, tail e')
            Result c -> do
                t <- makeTemp ()
                pure (IdentVal t, (Temp (t, c)):(tail e'))
            Temp _ -> error "Should not be a temporary."

    -- Takes the topmost expression of a simple stack to an identifier. A temporary
    -- is generated if required, and the resulting stack state after is also returned.
    exprAsId :: Simplify [SimpleExprResult]
                -> Simplify (V.BasicIdent, [SimpleExprResult])
    exprAsId e = do
        e' <- e
        case head e' of
            Result (SimpleVal (IdentVal i)) -> pure (i, tail e')
            Result c -> do
                t <- makeTemp ()
                pure (t, (Temp (t, c)):(tail e'))
            Temp _ -> error "Should not be a temporary."

    -- Extreme boilerplate follows.

    gToVType :: TySrcAnnType -> V.TyAnnType
    gToVType = cata psi where
        psi (Ann (tyTy, _) ty) = Fix (Ann tyTy (case ty of
            G.SliceType ty' -> V.SliceType ty'
            G.ArrayType int ty' -> V.ArrayType (Identity $ getConst $ bare int) ty'
            G.NamedType i -> V.NamedType (gToVIdent $ bare i)
            G.StructType fs -> V.StructType $ map (\(i, ty'') -> (gToVIdent $ bare i, ty'')) fs))

    gToVLit g = case g of
        G.IntLit a -> V.IntLit a
        G.FloatLit a -> V.FloatLit a
        G.RuneLit a -> V.RuneLit a
        G.StringLit a -> V.StringLit a

    gToVIdent i = V.Ident $ G.unIdent i

    gToVUnOp o = case o of
        G.Positive -> V.Positive
        G.Negative -> V.Negative
        G.BitwiseNot -> V.BitwiseNot
        _ -> error "unimplemented unsupported unop error"

    gToVCondOp o = case o of
        G.LogicalOr -> V.LogicalOr
        G.LogicalAnd -> V.LogicalAnd
        G.Equal -> V.Equal
        G.NotEqual -> V.NotEqual
        G.LessThan -> V.LessThan
        G.LessThanEqual -> V.LessThanEqual
        G.GreaterThan -> V.GreaterThan
        G.GreaterThanEqual -> V.GreaterThanEqual
        _ -> error "unimplemented unsupported condop error"

    gToVBinOp o = case o of
        G.Plus -> V.Plus
        G.Minus -> V.Minus
        G.Times -> V.Times
        G.Divide -> V.Divide
        G.Modulo -> V.Modulo
        G.ShiftLeft -> V.ShiftLeft
        G.ShiftRight -> V.ShiftRight
        G.BitwiseAnd -> V.BitwiseAnd
        G.BitwiseAndNot -> V.BitwiseAndNot
        G.BitwiseOr -> V.BitwiseOr
        G.BitwiseXor -> V.BitwiseXor
        _ -> error "unimplemented unsupported binop error"
