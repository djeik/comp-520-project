{-|
Module      : Language.Vigil.Syntax
Description : Vigil syntax definitions and instances
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

This module defines the base functors for the entire Vigil syntax. Briefly,
the differences with the GoLite syntax are:
    * Type declarations disappear (only canonical types are used).
    * The top-level is organized in variable and function declarations.
    * Variable declarations have no initializers anymore.
    * Variable declarations inside functions are disallowed, and instead occur
      as a different field of the "FunDecl" data type.
    * Block statements are undone.
    * Assign-operations and IncDec statements are to be replaced by their
      three-address form.
    * Initializers in complex statements are moved to just before the complex
      statement.
    * Expressions are in three-adress form.
    * Conditional expressions are separate from expressions.
    * Postfix operator chaining is only allowed homogeneously and only for some
      postfix operators.
    * Postfix operator chaining is not allowed as part of a more complex
      expression.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Vigil.Syntax
( -- * Syntax definitions
  Program (..)
, VarDecl (..)
, FunDecl (..)
, StatementF (..)
, Expr (..)
, CondExpr (..)
, Ref (..)
, Val (..)
, Literal (..)
, Ident (..)
-- * Basic types
, VigilInt
, VigilFloat
, VigilRune
, VigilString
-- * Operators
, BinaryOp (..)
, BinaryCondOp (..)
, UnaryOp (..)
, UnaryCondOp (..)
-- * Convenience re-exports
, Fix (..)
, Identity (..)
) where

import Data.Functor.Foldable ( Fix(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.String

import Language.Common.Pretty
import Data.Functor.Foldable ( cata )

-- | A Vigil program is simply a list of global variable declarations and
-- function declarations, and optionally a main function.
data Program varDecl funDecl
    = Program
        { _globals :: [varDecl]
        , _funcs :: [funDecl]
        , _main :: (Maybe funDecl)
        }
    deriving (Eq, Show)

instance (Pretty v, Pretty f) => Pretty (Program v f) where
    pretty (Program gs fs m) =
        vcat (map pretty gs) $+$
        vcat (map pretty fs) $+$
        pretty m

-- | A variable declaration is a list of identifiers associated with a type.
-- There are no initializations.
data VarDecl ident
    = VarDecl ident
    deriving (Eq, Ord, Read, Show)

instance (Pretty ident) => Pretty (VarDecl ident) where
    pretty (VarDecl i) = text "var" <+> pretty i

-- | A Vigil function declares all of its variables before any statements.
data FunDecl ident vardecl stmt
    = FunDecl
        { _funDeclName :: ident
        , _funDeclArgs :: [vardecl]
        , _funDeclVars :: [vardecl]
        , _funDeclBody :: [stmt]
        }
    deriving (Eq, Ord, Read, Show)

instance
    ( Pretty ident
    , Pretty vardecl
    , Pretty stmt
    ) => Pretty (FunDecl ident vardecl stmt) where
    pretty (FunDecl n a v b) =
        text "func"
        <+> pretty n
        <+> text "{" $+$
        nest indentLevel (
            text "[["
            <> (hcat $ punctuate comma (map pretty a))
            <> text "]]"
            $+$ vcat (map pretty v)
            $+$ vcat (map pretty b)
        ) $+$
        text "}"

-- | Vigil statements.
data StatementF expr cond ref ident f
    = ExprStmt expr
    | CondExprStmt cond
    -- ^ Expressions are allowed as statements.
    | Initialize ident
    -- ^
    | Assign ref expr
    -- ^ Simple assignments (i.e. not assign-ops) are allowed, but only from an
    -- expression to a ref.
    | PrintStmt [ref]
    -- ^ Only refs can be printed
    | ReturnStmt (Maybe ref)
    -- ^ A return statement can potentially include a ref.
    | IfStmt cond [f] (Maybe [f])
    -- ^ Only conditional expressions are allowed in the guard of a conditional
    -- statement. There is no initializer statement.
    | SwitchStmt
        { switchGuard :: (Maybe expr)
        , switchCases :: [([(expr, [f])], [f])]
        , switchDefaultCase :: [f]
        }
    -- ^ In switch statements, non-default case heads need several lists of
    -- statements. Each original expression of the head is evaluated in turn,
    -- which may require use of temporaries. The final statement in this list
    -- is an expression statement, whose value needs to be compared to the
    -- guard if present.
    --
    -- The defaultCase field is always present. A missing default case is
    -- indicated by an empty statement list.
    | ForStmt (Maybe ([f], cond)) [f]
    | BreakStmt
    | ContinueStmt
    deriving (Eq, Functor, Ord, Read, Show)

instance
    ( Pretty expr
    , Pretty cond
    , Pretty ref
    , Pretty ident
    ) => Pretty (Fix (StatementF expr cond ref ident)) where
    pretty = cata f where
        f ::
            ( Pretty expr
            , Pretty cond
            , Pretty ref
            , Pretty ident
            ) => StatementF expr cond ref ident Doc -> Doc
        f e' = case e' of
            Initialize i -> text "initialize " <+> pretty i
            ExprStmt e -> pretty e
            CondExprStmt c -> pretty c
            Assign r e -> pretty r <+> text "=" <+> pretty e
            PrintStmt rs -> text "print"
                <+> prettyParens True (sep $ punctuate comma (map pretty rs))
            ReturnStmt r -> text "return" <+> pretty r
            IfStmt c thens elses ->
                text "if" <+> pretty c <+> text "{" $+$
                nest indentLevel (
                    vcat (map pretty thens)
                ) $+$
                text "}" $+$
                case elses of
                    Just elses' ->
                        text "else {" $+$
                        nest indentLevel (
                            vcat (map pretty elses')
                        ) $+$
                        text "}"
                    Nothing -> empty
            ForStmt c bod ->
                text "for" <+>
                case c of
                    -- The list of statements as a condition of the for loop
                    -- is artificial.
                    Just (c', cond) ->
                        text "[[" $+$
                        vcat (map pretty c') $+$
                        text "<" <> pretty cond <> text ">" $+$
                        text "]]"
                    Nothing -> empty
                <+> text "{" $+$
                nest indentLevel (
                    vcat (map pretty bod)
                ) $+$
                text "}"
            SwitchStmt gu cs def ->
                text "switch" <+> pretty gu <+> text "{" $+$
                nest indentLevel (
                    vcat (map (\(hs, es) ->
                        text "case" $+$
                        nest indentLevel (
                            vcat (map (\(e, h) ->
                                text "[["
                                <> hsep (punctuate semi (map pretty h))
                                <+> text "|" <+> pretty e
                                <> text "]]") hs)
                            <+> text ":"
                            $+$ vcat (map pretty es)
                        )
                    ) cs) $+$
                    text "default:" $+$
                    nest indentLevel (
                        vcat (map pretty def)
                    )
                )
            BreakStmt -> text "break"
            ContinueStmt -> text "continue"

-- | A Vigil expression is in three-address form.
data Expr ty ref ident val binop unop condexpr a
    = Binary val binop val
    -- ^ A binary expression takes two values and joins them with a binary op.
    | Unary unop val
    -- ^ A unary expression joins a unary op with a value.
    | Ref ref
    -- ^ A ref can be a full-fledged expression on its own.
    | Conversion ty ref
    -- ^ A conversion of a ref can also be an expression.
    | Call ident [val]
    -- ^ A call is an identifier (the callee) and a list of values (the params).
    | Cond condexpr
    -- ^ Conditional expressions are also expressions.
    | InternalCall String [val]
    -- ^ A call to a function in the runtime.
    deriving (Eq, Functor, Ord, Read, Show)

instance
    ( Pretty ty
    , Pretty ref
    , Pretty ident
    , Pretty val
    , Pretty binop
    , Pretty unop
    , Pretty condexpr
    ) => Pretty (Expr ty ref ident val binop unop condexpr a) where
    pretty e' = case e' of
        Binary l o r -> pretty l <+> pretty o <+> pretty r
        Unary o v -> pretty o <> pretty v
        Ref r -> pretty r
        -- The fact that the type has no "external" representation is underscored
        -- by this double bracketing
        Conversion ty r -> text "[[" <> pretty ty <> text "]]"
                        <> prettyParens True (pretty r)
        Call i vs -> pretty i
                  <> prettyParens True (sep $ punctuate comma (map pretty vs))
        Cond e -> pretty e
        InternalCall s vs ->
            text (s ++ "#") <>
            prettyParens True (sep $ punctuate comma (pretty <$> vs))

-- | Conditional expressions are separate from expressions to restrict their use
-- and for codegen considerations
data CondExpr val ref bincondop uncondop
    = CondRef ref
    | BinCond val bincondop val
    | UnCond uncondop val
    deriving (Eq, Functor, Ord, Read, Show)

instance
    ( Pretty val
    , Pretty ref
    , Pretty bincondop
    , Pretty uncondop
    ) => Pretty (CondExpr val ref bincondop uncondop) where
    pretty e = case e of
        CondRef r -> pretty r
        BinCond l o r -> pretty l <+> pretty o <+> pretty r
        UnCond o v -> pretty o <> pretty v

-- | A reference is either a naked value or a series of homogeneous array /
-- selector / slice operations.
data Ref ident selIdent val a
    = ArrayRef ident [val]
    | SelectRef ident [Int]
    | SliceRef ident [(Maybe val, Maybe val, Maybe val)]
    | ValRef val
    deriving (Eq, Functor, Ord, Read, Show)

instance
    ( Pretty ident
    , Pretty selIdent
    , Pretty val
    ) => Pretty (Ref ident selIdent val a) where
    pretty r = case r of
        ArrayRef i vs -> pretty i <> sep (map (prettyBrackets True . pretty) vs)
        SelectRef i is -> pretty i <> sep (punctuate (text ",") (map pretty is))
        SliceRef i bds -> pretty i <> sep (map
            (prettyBrackets True . (\(lo, hi, bn) ->
                pretty lo <>
                colon <>
                pretty hi <>
                colon <>
                pretty bn)) bds)
        ValRef v -> pretty v

-- | A Vigil value is either an identifier or a literal.
data Val ident lit
    = IdentVal ident
    | IdentValD ident
    | Literal lit
    deriving (Eq, Functor, Ord, Read, Show)

instance (Pretty ident, Pretty lit) => Pretty (Val ident lit) where
    pretty v = case v of
        IdentVal i -> pretty i
        Literal l -> pretty l
        IdentValD i -> prettyBrackets True (pretty i)

-- | Arithmetic/integral binary operators.
data BinaryOp a
    = Plus | Minus | Times | Divide | Modulo
    | ShiftLeft | ShiftRight | BitwiseAnd | BitwiseAndNot | BitwiseOr | BitwiseXor
    deriving (Eq, Functor, Ord, Read, Show)

instance Pretty (BinaryOp a) where
    pretty o = case o of
        Plus -> text "+"
        Minus -> text "-"
        Times -> text "*"
        Divide -> text "/"
        Modulo -> text "%"
        ShiftLeft -> text "<<"
        ShiftRight -> text ">>"
        BitwiseAnd -> text "&"
        BitwiseAndNot -> text "&^"
        BitwiseOr -> text "|"
        BitwiseXor -> text "^"

-- | Binary conditional operators (equality, ordering, binary logicals).
data BinaryCondOp a
    = LogicalOr | LogicalAnd | Equal | NotEqual
    | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual
    deriving (Eq, Ord, Read, Show)

instance Pretty (BinaryCondOp a) where
    pretty o = case o of
        LogicalOr -> text "||"
        LogicalAnd -> text "&&"
        Equal -> text "=="
        NotEqual -> text "!="
        LessThan -> text "<"
        LessThanEqual -> text "<="
        GreaterThan -> text ">"
        GreaterThanEqual -> text ">="

-- | Unary operators (unsupported GoLite unary operators are removed).
data UnaryOp a
    = Positive | Negative | BitwiseNot
    deriving (Eq, Ord, Read, Show)

instance Pretty (UnaryOp a) where
    pretty o = case o of
        Positive -> text "+"
        Negative -> text "+"
        BitwiseNot -> text "^"

-- | Unary conditional operators (i.e. not)
data UnaryCondOp a
    = LogicalNot
    deriving (Eq, Ord, Read, Show)

instance Pretty (UnaryCondOp a) where
    pretty o = case o of
        LogicalNot -> text "!"

-- The rest of these definitions are exactly the same as for GoLite.

data Literal a
    = IntLit VigilInt
    | FloatLit VigilFloat
    | RuneLit VigilRune
    deriving (Eq, Functor, Ord, Read, Show)

instance Pretty (Literal a) where
    pretty l = case l of
        IntLit x -> pretty x
        FloatLit x -> pretty x
        RuneLit x -> text $ show x

data Ident a
    = Ident
        { unIdent :: String
        }
    deriving (Eq, Functor, Ord, Read, Show)

instance Pretty (Ident a) where
    pretty (Ident s) = text s

instance IsString (Ident a) where
    fromString = Ident

type VigilInt = Int
type VigilFloat = Double
type VigilRune = Char
type VigilString = String
