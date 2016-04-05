{-|
Module      : Language.GoLite.Syntax.Types
Description : GoLite syntax definitions and instances
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

This module defines the base functors for all declaration types along with
their instances.
-}

{-# LANGUAGE DeriveFunctor #-}

module Language.GoLite.Syntax.Types.Decl where

import Language.GoLite.Pretty

-- | A package declaration.
data Package ident topLevelDecl
    = Package ident [topLevelDecl]
    deriving (Eq, Ord, Read, Show)

-- | Pretty-prints the package declaration followed by all the declarations in
-- the package.
instance (Pretty ident, Pretty decl) => Pretty (Package ident decl) where
    pretty e = case e of
        Package i decls ->
            text "package" <+> pretty i $+$
            vcat (pretty <$> decls)


-- | A top-level declaration.
--
-- All 'Declaration's can be used as top-level declarations, but function
-- declarations can be used /only/ as top-level declarations.
data TopLevelDecl decl funDecl
    = TopLevelDecl decl
    | TopLevelFun funDecl
    deriving (Eq, Ord, Read, Show)

-- | Simply dispatches to the 'Pretty' instances of the contained data.
instance
    ( Pretty decl
    , Pretty funDecl
    ) => Pretty (TopLevelDecl decl funDecl) where

    pretty e = case e of
        TopLevelDecl d -> pretty d
        TopLevelFun f -> pretty f


-- | A declaration.
data Declaration typeDecl varDecl
    = TypeDecl typeDecl
    | VarDecl varDecl
    deriving (Eq, Ord, Read, Show)

-- | Simply dispatches to the 'Pretty' instances of the contained data and
-- appends semicolons.
instance
    ( Pretty typeDecl
    , Pretty varDecl
    ) => Pretty (Declaration typeDecl varDecl) where

    pretty e = case e of
        TypeDecl t -> pretty t <> semi
        VarDecl t -> pretty t <> semi


-- | The body of a variable declaration.
--
-- The identifier list must be nonempty. Either the type annotation or the list
-- of expressions may be omitted, but not both. If the type is omitted, the list
-- or expressions must have the same length as the identifier list.
data VarDecl ident ty expr
    = VarDeclBody [ident] (Maybe ty) [expr]
    deriving (Eq, Ord, Read, Show)

-- | Prints the \"var\" keyword followed by the identifiers comma-separated. If
-- there is a type present it is printed. If there are expressions present, an
-- equals is printed followed by the expressions comma-separated.
instance
    ( Pretty ident
    , Pretty ty
    , Pretty expr
    ) => Pretty (VarDecl ident ty expr) where

    pretty e = case e of
        VarDeclBody idents mty exprs ->
            text "var" <+>
            sep (punctuate comma (pretty <$> idents)) <+>
            pretty mty <>
            (if null exprs
                then empty
                else
                    text " =" <+>
                    sep (punctuate comma (pretty <$> exprs))
            )


-- | A type declaration.
data TypeDecl ident ty
    = TypeDeclBody ident ty
    deriving (Eq, Ord, Read, Show)

-- | Prints the \"type\" keyword followed by the identifier to bind followed by
-- the type to bind the identifier to.
instance
    ( Pretty ident
    , Pretty ty
    ) => Pretty (TypeDecl ident ty) where

    pretty e = case e of
        TypeDeclBody ident ty -> text "type" <+> pretty ident <+> pretty ty


-- | A function declaration.
data FunDecl ident argTy retTy stmt
    = FunDecl ident [(ident, argTy)] retTy [stmt]
    deriving (Eq, Ord, Read, Show)

-- | Prints the \"func\" keyword followed by the function name, its argument
-- list wrapped in parentheses, and the body of statements one per line wrapped
-- in braces.
instance
    ( Pretty ident
    , Pretty argTy
    , Pretty retTy
    , Pretty stmt
    ) => Pretty (FunDecl ident argTy retTy stmt) where

    pretty (FunDecl i args mret body)
        = text "func"
        <+> pretty i
        <+> prettyParens True prettyArgs
        <+> pretty mret
        <+> text "{"
        $+$ nest indentLevel (vcat $
            map pretty body
        )
        $+$ text "}" where
            prettyArgs = sep (punctuate comma (pa <$> args))
            pa (ident, ty) = pretty ident <+> pretty ty
