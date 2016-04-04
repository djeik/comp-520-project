{-|
Module      : Language.Vigil.Simplify.Top
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Simplifications for the top-level (globals and function declarations).
-}

module Language.Vigil.Simplify.Top where

import Data.List ( partition )

import Language.Common.Monad.Traverse

import Language.Vigil.Simplify.Core
import Language.Vigil.Simplify.Expr
import Language.Vigil.Simplify.Stmt
import Language.GoLite.Syntax.Types as G
import Language.Vigil.Syntax as V
import Language.Vigil.Syntax.Basic

-- | Simplifies a GoLite package into a Vigil program.
--
-- Global variables are separated from functions, and their initializations are
-- moved to a specific function. Functions themselves have their body simplified.
simplifyPackage :: TySrcAnnPackage -> Simplify BasicProgram
simplifyPackage (Package _ decls) = do
    let (globs, funs) = partition isGlob decls
    let globs' = filter isVar globs

    vs <- forM globs' (\(G.TopLevelDecl (G.VarDecl (G.VarDeclBody is _ es))) ->
        case es of
            -- TODO: we don't know the type that things should be declared with yet.
            -- TODO: in the case of no initialization, perhaps provide a default one now?
            [] -> forM is (\i -> pure (V.VarDecl (gIdToVIdent i) undefined, []))
            _ -> forM (zip is es) (\(i, e) -> do
                let i' = gIdToVIdent i
                (e', s) <- realizeToExpr $ simplifyExpr e
                pure (V.VarDecl i' undefined,
                    s ++ [Fix $ V.Assign (ValRef $ IdentVal i') e'])))

    -- vs: pairs of declarations and their initializing statements
    let vs' = concat vs
    nis <- gets (\s -> newDeclarations s)
    let nvs = map (\d -> V.VarDecl d undefined) nis
    let fInit = V.FunDecl
                { _funDeclName = V.Ident "%init"
                , _funDeclArgs = []
                , _funDeclReturn = undefined -- TODO change to Vigil void
                , _funDeclVars = nvs
                , _funDeclBody = concat $ map snd vs'
                }

    -- TODO single out main.
    fs <- forM funs (\(G.TopLevelFun (G.FunDecl i ps rTy bod)) -> do
        -- Reset the state of declarations.
        modify (\s ->  s { newDeclarations = [] })
        bod' <- forM bod simplifyStmt
                                                -- TODO change to actual VType
        let ps' = map (\(pid, pty) -> V.VarDecl (gIdToVIdent pid) undefined) ps

        nis' <- gets (\s -> newDeclarations s)
        let nvs' = map (\d -> V.VarDecl d undefined) nis'
        pure $ V.FunDecl
                { _funDeclName = gIdToVIdent i
                , _funDeclArgs = ps'
                , _funDeclReturn = undefined -- TODO change to actual VType
                , _funDeclVars = nvs'
                , _funDeclBody = concat bod'
                })

    pure V.Program
            { _globals = map fst vs'
            , _funcs = fInit:fs
            , _main = undefined -- TODO single out main
            }

    where
        isGlob (TopLevelDecl _) = True
        isGlob _ = False

        isVar (TopLevelDecl (G.VarDecl _)) = True
        isVar _ = False