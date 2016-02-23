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

-- | Constructs a top level declaration.
topLevelDecl :: decl -> TopLevelDecl decl funDecl
topLevelDecl = TopLevelDecl

-- | Constructs a top level function.
topLevelFun :: funDecl -> TopLevelDecl decl funDecl
topLevelFun = TopLevelFun

-- | Constructs the body of a variable declaration.
varDeclBody :: [ident] -> (Maybe ty) -> [expr] -> VarDecl ident ty expr
varDeclBody = VarDeclBody

-- | Constructs the body of a type declaration.
typeDeclBody :: ident -> ty -> TypeDecl ident ty
typeDeclBody = TypeDeclBody

-- | Constructs a type declaration.
typeDecl :: typeDecl -> Declaration typeDecl varDecl
typeDecl = TypeDecl

-- | Constructs a variable declaration.
varDecl :: varDecl -> Declaration typeDecl varDecl
varDecl = VarDecl
