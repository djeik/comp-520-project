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
import Language.Vigil.Simplify.Core
import Language.Vigil.Simplify.Expr
import Language.Vigil.Simplify.Stmt
import Language.Vigil.Syntax as V
import Language.Vigil.Syntax.TyAnn
import Language.Vigil.Types

import Data.Maybe ( catMaybes )

-- | Simplifies a GoLite package into a Vigil program.
--
-- Global variables are separated from functions, and their initializations are
-- moved to a specific function. Functions themselves have their body simplified.
simplifyPackage :: TySrcAnnPackage -> Simplify TyAnnProgram
simplifyPackage (Package _ decls) = do
    let (globs, funs) = partition isGlob decls
    let globs' = filter isVar globs

    vs <-
        forM globs' $ \(G.TopLevelDecl (G.VarDecl (G.VarDeclBody is _ es))) -> do
            case es of
                -- TODO: in the case of no initialization, perhaps provide a
                -- default one now?
                [] -> forM is $ \i -> do
                    m <- reinterpretGlobalIdEx i
                    case m of
                        Nothing -> pure (Nothing, [])
                        Just i' -> pure (Just $ V.VarDecl i', [])

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

    -- vs: pairs of declarations and their initializing statements
    let vs' = concat vs
    nis <- gets newDeclarations
    let nvs = map V.VarDecl nis
    let fInit = V.FunDecl
                { _funDeclName = artificialGlobalId (-1) "%init" (funcType [] voidType)
                , _funDeclArgs = []
                , _funDeclVars = nvs
                , _funDeclBody = concat $ map snd vs'
                }

    fs <- fmap catMaybes $ forM funs $ \(G.TopLevelFun (G.FunDecl i ps _ bod)) -> do
        modify (\s ->  s { newDeclarations = [] }) -- Reset state of declarations.
        bod' <- forM bod simplifyStmt -- Simplify body

        ps' <- fmap catMaybes $ forM ps $ \(pid, _) -> do
            m <- reinterpretGlobalIdEx pid
            pure $ case m of
                Nothing -> Nothing
                Just i' -> Just $ V.VarDecl i'

        nis' <- gets newDeclarations
        let nvs' = map V.VarDecl nis'

        m <- reinterpretGlobalIdEx i
        case m of
            Nothing -> pure Nothing
            Just i' -> pure $ Just $ V.FunDecl
                { _funDeclName = i'
                , _funDeclArgs = ps'
                , _funDeclVars = nvs'
                , _funDeclBody = concat bod'
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
