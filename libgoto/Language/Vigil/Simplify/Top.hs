{-|
Module      : Language.Vigil.Simplify.Top
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Simplifications for the top-level (globals and function declarations).
-}

{-# LANGUAGE OverloadedStrings #-}

module Language.Vigil.Simplify.Top where

import Data.List ( partition )

import Language.Common.Monad.Traverse

import Language.GoLite.Syntax.Types as G
import qualified Language.GoLite.Types as T
import Language.Vigil.Simplify.Core
import Language.Vigil.Simplify.Expr
import Language.Vigil.Simplify.Stmt
import Language.Vigil.Syntax as V
import Language.Vigil.Syntax.TyAnn
import Language.Vigil.Types
import Language.X86.Mangling

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe ( catMaybes )
import Data.Tuple ( swap )

-- | Simplifies a GoLite package into a Vigil program.
--
-- Global variables are separated from functions, and their initializations are
-- moved to a specific function. Functions themselves have their body simplified.
simplifyPackage :: TySrcAnnPackage -> Simplify TyAnnProgram
simplifyPackage (Package _ decls) = do
    let (globs, funs) = partition isGlob decls
    let globs' = filter isVar globs

    fs <- fmap catMaybes $ forM funs $ \(G.TopLevelFun (G.FunDecl i ps _ bod)) -> do
        modify (\s ->  s { newDeclarations = [] }) -- Reset state of declarations.
        bod' <- forM bod simplifyStmt -- Simplify body

        ps' <- fmap catMaybes $ forM ps $ \(pid, _) -> do
            m <- reinterpretGlobalIdEx pid
            pure $ case m of
                Nothing -> Nothing
                Just i' -> Just $ V.VarDecl i'

        nis' <- gets newDeclarations
        let nvs' = map (swap . fmap V.VarDecl . swap) nis'

        -- Look at the
        forM_ nvs' (\(V.VarDecl i, b) ->
            when (not b) $ modify $ \s -> s { inis = S.insert i $ inis s })

        m <- reinterpretGlobalIdEx i
        case m of
            Nothing -> pure Nothing
            Just i' -> pure $ Just $ V.FunDecl
                { _funDeclName = i'
                , _funDeclArgs = ps'
                , _funDeclVars = map fst nvs'
                , _funDeclBody = concat bod'
                }
    vs <- forM globs' $ \(G.TopLevelDecl (G.VarDecl (G.VarDeclBody is _ es))) -> do
        case es of
            [] -> forM is $ \i -> do
                m <- reinterpretGlobalIdEx i
                case m of
                    Nothing -> pure (Nothing, [])
                    -- Come up with an initializer right here
                    Just i' -> pure (Just $ V.VarDecl i', [Fix $ V.Initialize i'])

            _ -> forM (zip is es) $ \(i, e) -> do
                m <- reinterpretGlobalIdEx i
                (e', s) <- realizeToExpr =<< simplifyExpr e

                pure $ case m of
                    Nothing ->
                        ( Nothing
                        , s ++ [Fix $ V.ExprStmt e']
                        )
                    Just i' ->
                        ( Just $ V.VarDecl i'
                        , s ++ [
                            Fix $ V.Assign (Ann (gidTy i') $ ValRef $ IdentVal i') e'
                        ]
                        )

    ss <- gets strings

    -- create the initialization calls for the string literals
    (vs2, ss') <- fmap unzip $ forM (M.assocs ss) $ \(g, str) -> do
        gi <- makeIdent
            stringType
            (T.symbolFromString $ T.stringFromSymbol (gidOrigName g) ++ "data")
        pure $
            ( ( Just $ V.VarDecl g
              , [Fix $ V.Assign
                  (Ann (gidTy g) $ ValRef $ IdentVal g)
                  (Ann stringType $ V.InternalCall
                    (mangleFuncName "from_cstr")
                    [IdentValD gi]
                  )
                ]
              )
            , (gi, str)
            )

    modify $ \s -> s { strings = M.fromList ss' }

    -- vs: pairs of declarations and their initializing statements
    let vs' = concat vs ++ vs2
    nis <- gets newDeclarations
    let nvs = map (swap . fmap V.VarDecl . swap) nis
    let fInit = V.FunDecl
                { _funDeclName = artificialGlobalId
                    (-1)
                    (mangleFuncName "gocode_init")
                    (funcType [] voidType)
                , _funDeclArgs = []
                , _funDeclVars = (map fst nvs)
                , _funDeclBody = concat $ map snd vs'
                }

    let (main, notMain) = partition
                    (\(V.FunDecl i _ _ _) -> gidOrigName i == "main") (fInit:fs)

    when (length main > 1) (throwError $ InvariantViolation "More than one main")

    pure V.Program
            { _globals = catMaybes $ map fst vs'
            , _funcs = notMain
            , _main = case main of
                [x] -> Just x
                [] -> Nothing
                _ -> error "Laws of physics broken"
            }

    where
        isGlob (TopLevelDecl _) = True
        isGlob _ = False

        isVar (TopLevelDecl (G.VarDecl _)) = True
        isVar _ = False
