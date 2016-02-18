{-|
Module      : Language.GoLite.Syntax.Sugar.Declaration
Description : Syntax sugar for synthesizing declarations
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

module Language.GoLite.Syntax.Sugar.Declaration where

import Language.GoLite.Syntax.Types

topLevelDecl :: decl -> TopLevelDecl decl funDecl
topLevelDecl = TopLevelDecl

topLevelFun :: funDecl -> TopLevelDecl decl funDecl
topLevelFun = TopLevelFun

varDeclBody :: [ident] -> (Maybe ty) -> [expr] -> VarDecl ident ty expr
varDeclBody = VarDeclBody

typeDeclBody :: ident -> ty -> TypeDecl ident ty
typeDeclBody = TypeDeclBody

typeDecl :: typeDecl -> Declaration typeDecl varDecl
typeDecl = TypeDecl

varDecl :: varDecl -> Declaration typeDecl varDecl
varDecl = VarDecl
