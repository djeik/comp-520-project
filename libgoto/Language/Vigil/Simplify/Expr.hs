{-|
Module      : Language.Vigil.Simplify.Expr
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Simplifications for expressions.
-}

{-# LANGUAGE ViewPatterns #-}

module Language.Vigil.Simplify.Expr
( SimpleExprResult(..)
, SimpleConstituent(..)
, CircuitOp(..)
, simplifyExpr
, gToVBinOp
, typeFromSimple
, typeFromVal
) where

import Language.GoLite.Syntax.Types as G
import Language.GoLite.Types as T
import Language.Vigil.Simplify.Core
import Language.Vigil.Syntax as V
import Language.Vigil.Syntax.Basic as V
import Language.Vigil.Syntax.TyAnn as V
import Language.Vigil.Types as V

import Language.Common.Pretty

-- | The final result of a simplified expression
data SimpleExprResult
    = Result SimpleConstituent
    -- ^ A normal result.
    | Temp (V.BasicIdent, SimpleConstituent)
    -- ^ A temporary, which will need to receive the value from the given
    -- constituent.
    | ShortCircuit V.BasicIdent CircuitOp [SimpleExprResult] [SimpleExprResult]
    -- ^ A short-circuit. Indicates that special statements need to be generated
    -- for this expression. The provided identifier will be used as a temporary,
    -- the provided operation will determine the conditions of short-circuiting,
    -- and the two lists of results indicate the left and the right expressions,
    -- the right being the one that can be short-circuited.

-- | In a short-circuit, indicates the type of expression that is split.
data CircuitOp
    = And
    | Or

-- | Simplifying an expression could result either in a value, a reference, or a
-- Vigil expression. We always try to reduce to the simplest possible form, so
-- for example, the GoLite expression @a@ would become a Vigil value.
data SimpleConstituent
    = SimpleExpr TyAnnExpr
    | SimpleRef TyAnnRef
    | SimpleVal TyAnnVal

instance Pretty SimpleConstituent where
    pretty a = case a of
        SimpleExpr x -> pretty x
        SimpleRef x -> pretty x
        SimpleVal x -> pretty x

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
    phi a ex =
        case ex of
        G.BinaryOp o l r -> do
            (vl, esl) <- exprAsVal =<< l
            (vr, esr) <- exprAsVal =<< r
            a' <- reinterpretTypeEx $ fst a

            let es = esl ++ esr
            -- Should this be a conditional, short-circuit or binary expression?
            let o' = bare o in
                if isComparisonOp o' || isOrderingOp o' then
                    pure $
                        ( Result
                        $ SimpleExpr
                        $ Ann a' $ Cond
                        $ BinCond vl (gToVCondOp o') vr
                        ):es
                else if isLogicalOp o' then do
                    circOp <- case o' of
                        G.LogicalAnd -> pure And
                        G.LogicalOr -> pure Or
                        _ -> throwError $ InvariantViolation "Unknown logical \
                                                            \operator"

                    t <- makeTemp V.boolType
                    pure [ShortCircuit t circOp
                        ((Result $ SimpleVal vl):esl)
                        ((Result $ SimpleVal vr):esr)]
                else
                    pure $
                        ( Result
                        $ SimpleExpr
                        $ Ann a' $ Binary vl (gToVBinOp o') vr
                        ):es

        G.UnaryOp o e -> do
            (v, e') <- exprAsVal =<< e
            a' <- reinterpretTypeEx $ fst a

            pure $ (case bare o of
                G.LogicalNot -> Result $ SimpleExpr $ Ann a' $ Cond $ UnCond V.LogicalNot v
                _ -> Result $ SimpleExpr $ Ann a' $ Unary (gToVUnOp $ bare o) v):e'

        G.Conversion ty e -> do
            aRes <- reinterpretTypeEx $ fst a

            e' <- e
            let ty' = gToVType ty
            case head e' of
                -- In the case of a ref, promote it directly to a conversion.
                (Result (SimpleRef r)) ->
                    pure $ (Result $ SimpleExpr $ Ann aRes $ V.Conversion ty' r):(tail e')
                -- In the case of a value, make it into a ref and convert that.
                (Result (SimpleVal v)) -> -- <--
                    pure $ (Result
                            $ SimpleExpr $ Ann aRes
                            $ V.Conversion ty' $ Ann (typeFromVal v) $ ValRef v)
                            :(tail e')
                -- Otherwise, create a temporary and convert the temporary.
                (Result e''@(SimpleExpr (Ann aIn _))) -> do
                    t <- makeTemp aIn
                    pure $ (Result
                            $ SimpleExpr $ Ann aRes
                            $ V.Conversion ty' $ Ann aIn $ ValRef $ IdentVal t)
                            :(Temp (t, e''))
                            :(tail e')
                _ -> throwError $ InvariantViolation "Conversion: unexpected non-result"

        G.Selector e id' -> do
            a' <- reinterpretTypeEx $ fst a
            e' <- e

            case e' of
                ((Result e''):es) -> do
                    id'' <- case typeFromSimple e'' of
                        Fix (V.StructType fs) -> getFieldIEx (G.unIdent $ bare id') fs
                        _ -> throwError $ InvariantViolation "Selector with non-struct selectee"

                    case e'' of
                        -- If we already have a select ref (e.g. stru.foo.bar),
                        -- we just extend it with the new identifier
                        SimpleRef (Ann _ (SelectRef i is)) ->
                            pure $ (Result $ SimpleRef $ Ann a' $ SelectRef i
                                (is ++ [id''])):es
                        -- An ident val: replace by a select ref
                        SimpleVal (IdentVal i) ->
                            pure $ (Result $ SimpleRef $ Ann a' $ SelectRef i [id'']):es
                        -- Anything else: create a temp, select into it.
                        _ -> do
                            t <- makeTemp (typeFromSimple e'')
                            pure $ (Result $ SimpleRef $ Ann a' $ SelectRef
                                t [id''])
                                :(Temp (t, e''))
                                :es
                _ -> throwError $ InvariantViolation "Selector: unexpected non-result"

        G.Index eIn eBy' -> do
            a' <- reinterpretTypeEx $ fst a
            eIn' <- eIn

            (vBy, esBy) <- exprAsVal =<< eBy'

            let es = esBy ++ (tail eIn')
            case head eIn' of
                -- If we have an array ref, extend it.
                (Result (SimpleRef (Ann _ (ArrayRef i vs)))) ->
                    pure $ (Result $ SimpleRef $ Ann a' $ ArrayRef i (vs ++ [vBy])):es
                -- If we have an identifier value, replace it by an array ref.
                (Result (SimpleVal (IdentVal i))) ->
                    pure $ (Result $ SimpleRef $ Ann a' $ ArrayRef i [vBy]):es
                -- Otherwise, create a temp and index into it.
                (Result eIn'') -> do
                    t <- makeTemp (typeFromSimple eIn'')
                    pure $ (Result $ SimpleRef $ Ann a' $ ArrayRef t [vBy])
                            :(Temp (t, eIn''))
                            :es
                _ -> throwError $ InvariantViolation "Index: unexpected non-result"

        G.Slice e l h b -> do
            a' <- reinterpretTypeEx $ fst a
            e' <- e

            let ml = fmap (>>= exprAsVal) l
            let mh = fmap (>>= exprAsVal) h
            let mb = fmap (>>= exprAsVal) b

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
                (Result (SimpleRef (Ann _ (SliceRef i bs)))) ->
                    pure $ (Result $ SimpleRef $ Ann a' $ SliceRef i (bs ++ [bounds])):es
                -- Slicing an identifier: replace by a slice ref.
                (Result (SimpleVal (IdentVal i))) ->
                    pure $ (Result $ SimpleRef $ Ann a' $ SliceRef i [bounds]):es
                -- Slicing into anything else: create a temp and slice that.
                (Result e''@(SimpleExpr (Ann aIn _))) -> do
                    t <- makeTemp aIn
                    pure $ (Result $ SimpleRef $ Ann a' $ SliceRef t [bounds])
                            :(Temp (t, e''))
                            :es
                _ -> throwError $ InvariantViolation "Slice: unexpected non-result"

        G.Call e mty ps  -> do
            a' <- reinterpretTypeEx $ fst a
            -- Convert top expressions of parameters to values when needed.
            ps' <- forM ps (\cur -> do
                cur' <- cur
                case head cur' of
                    (Result (SimpleVal _)) -> pure $ cur'
                    (Result e') -> do
                        t <- makeTemp (typeFromSimple e')
                        pure $ (Temp (t, e')):(tail cur')
                    _ -> throwError $ InvariantViolation "Call: unexpected non-result")

            -- Make top values to be used as arguments to the call.
            let ps'' = map (\cur -> case head cur of
                    (Result (SimpleVal v)) -> v
                    (Temp (t, _)) -> IdentVal t
                    _ -> error "Expected simple value or temp") ps'


            -- If a type argument was present, convert it to a regular argument
            -- which is just the storage size of the type.
            tyPs <- case mty of
                Nothing -> pure ps''
                Just (Fix (Ann b _)) -> do
                    sz <- reinterpretTypeEx (fst b)
                    pure $ (V.Literal $ Ann (V.intType V.I8)
                        $ V.IntLit $ storageSize sz):ps''

            -- If the callee expression is an identifier, use it as is. Otherwise
            -- create a temporary.
            (i, es) <- exprAsId =<< e

            -- At this point ps' has either simple values or temps at the top.
            -- We keep the temps, and throw away the simple values since they
            -- made it directly into the call. For the temps, we merge all their
            -- prior expressions. Note that simple values can't have prior
            -- expressions so it's okay to throw them out.
            es' <- let fil = (\x -> case head x of
                        Result _ -> False
                        _ -> True) in pure $ concat $ filter fil ps'

            let inner = case gidTy i of
                    Fix (V.BuiltinType gTy) -> V.InternalCall (case gTy of
                        T.AppendType -> "goappend_slice"
                        T.CapType -> "gocap"
                        T.CopyType -> "gocopy"
                        T.LenType -> case typeFromVal $ head tyPs of
                            ArrayType _ _ -> "golen_array"
                            StringType -> "golen_array"
                            SliceType _ -> "golen_slice"
                        T.MakeType -> "gomake")
                    _ -> V.Call i

            pure $ (Result $ SimpleExpr $ Ann a' $ inner tyPs):(es ++ es')

        G.Literal (Ann a' l) -> do
            a'' <- reinterpretTypeEx (fst a')
            case l of
                G.IntLit n ->
                    pure [Result $ SimpleVal $ V.Literal (Ann a'' (V.IntLit n))]
                G.FloatLit n ->
                    pure [Result $ SimpleVal $ V.Literal (Ann a'' (V.FloatLit n))]
                G.RuneLit n ->
                    pure [Result $ SimpleVal $ V.Literal (Ann a'' (V.RuneLit n))]
                G.StringLit n -> do
                    g <- makeString n
                    pure [Result $ SimpleVal $ IdentVal g]

        G.Variable i -> do
            case maybeSymbol $ bare $ gidOrigName i of
                Nothing -> pure []
                _ -> case reinterpretGlobalId i of
                    Left _ -> throwError $ InvariantViolation "Unrepresentable type"
                    Right x -> do
                        let name = stringFromSymbol (gidOrigName x)
                        if gidTy i == untypedBoolType
                            then if name == "gocode_true"
                                then pure [
                                    Result . SimpleVal . V.Literal $
                                    Ann V.boolType (V.IntLit 1)
                                ]
                                else if name == "gocode_false"
                                then pure [
                                    Result . SimpleVal . V.Literal $
                                    Ann V.boolType (V.IntLit 0)
                                ]
                                else throwError $ InvariantViolation $ "untyped boolean is not true or false: " ++ name
                            else pure [Result $ SimpleVal $ IdentVal x]

        G.TypeAssertion _ _ ->
            throwError $ InvariantViolation "Type assertions are not supported."


    getFieldIEx :: String -> [(String, f)] -> Simplify Int
    getFieldIEx = getFieldIEx' 0 where
        getFieldIEx' :: Int -> String -> [(String, f)] -> Simplify Int
        getFieldIEx' _ _ [] = throwError $ InvariantViolation "Could not find struct field"
        getFieldIEx' n s (x:xs) =
            if fst x == s then pure n else getFieldIEx' (n + 1) s xs

    reinterpretTypeEx :: T.Type ->  Simplify V.Type
    reinterpretTypeEx t = case reinterpretType t of
        Left _ -> throwError $ UnrepresentableType ("reinterpretTypeEx: " ++ show t)
        Right x -> pure x

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
    exprAsVal :: [SimpleExprResult]
                -> Simplify (TyAnnVal, [SimpleExprResult])
    exprAsVal e' = do
        case head e' of
            Result (SimpleVal v) -> pure (v, tail e')
            Result c -> do
                t <- makeTemp (typeFromSimple c)
                pure (IdentVal t, (Temp (t, c)):(tail e'))
            ShortCircuit i _ _ _ -> pure (IdentVal i, e')
            Temp _ -> throwError $ InvariantViolation "Should not be a temporary."

    -- Takes the topmost expression of a simple stack to an identifier. A temporary
    -- is generated if required, and the resulting stack state after is also returned.
    exprAsId :: [SimpleExprResult]
                -> Simplify (V.BasicIdent, [SimpleExprResult])
    exprAsId e' = do
        case head e' of
            Result (SimpleVal (IdentVal i)) -> pure (i, tail e')
            Result c -> do
                t <- makeTemp (typeFromSimple c)
                pure (t, (Temp (t, c)):(tail e'))
            ShortCircuit i _ _ _ -> pure (i, e')
            Temp _ -> throwError $ InvariantViolation "Should not be a temporary."

    -- Extreme boilerplate follows.

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

gToVBinOp :: G.BinaryOp a -> V.BinaryOp a
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

gToVType :: TySrcAnnType -> V.BasicType
gToVType (Fix (Ann a _)) = case reinterpretType $ fst a of
    Left e -> error $ "Unrepresentable type: " ++ e
    Right x -> x

-- | Extracts the type of a simple constituent.
typeFromSimple :: SimpleConstituent -> V.Type
typeFromSimple c = case c of
    SimpleVal v -> typeFromVal v
    SimpleRef (Ann a _) -> a
    SimpleExpr (Ann a _) -> a

-- | Extracts the type of a value
typeFromVal :: TyAnnVal -> V.Type
typeFromVal v = case v of
    IdentVal gid -> gidTy gid
    V.Literal (Ann ty _) -> ty
