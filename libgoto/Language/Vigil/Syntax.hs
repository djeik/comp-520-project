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
, BinaryOp (..)
, TypeF (..)
, Literal (..)
, Ident (..)
-- * Basic types
, VigilInt
, VigilFloat
, VigilRune
, VigilString
-- * Operators
, RelOp (..)
, UnaryOp (..)
-- * Convenience re-exports
, Fix (..)
, Identity (..)
) where

import Data.Functor.Foldable ( Fix(..) )
import Data.Functor.Identity ( Identity(..) )
import Data.String

-- | A Vigil program is simply a list of global variable declarations and
-- function declarations.
data Program varDecl funDecl
    = Program [varDecl] [funDecl]
    deriving (Eq, Show)

-- | A variable declaration is a list of identifiers associated with a type.
-- There are no initializations.
data VarDecl ident ty
    = VarDecl [ident] ty
    deriving (Eq, Show)

-- | A Vigil function declares all of its variables before any statements.
data FunDecl ident argTy retTy varDecl stmt
    = FunDecl ident [(ident, argTy)] retTy [varDecl] [stmt]

-- | Vigil statements.
data StatementF expr cond ref f
    = ExprStmt expr
    -- ^ Expressions are allowed as statements.
    | Assign ref expr
    -- ^ Simple assignments (i.e. not assign-ops) are allowed, but only from an
    -- expression to a ref.
    | PrintStmt ref
    -- ^ Only refs can be printed
    | ReturnStmt (Maybe ref)
    -- ^ A return statement can potentially include a ref.
    | IfStmt cond [f] (Maybe [f])
    -- ^ Only conditional expressions are allowed in the guard of a conditional
    -- statement. There is no initializer statement.
    | SwitchStmt
        { guard :: (Maybe cond)
        , cases :: [([ref], [f])]
        , defaultCase :: Maybe [f]
        }
    | ForStmt (Maybe cond) [f]
    | BreakStmt
    | Continue
    deriving (Eq, Functor, Show)

-- | A Vigil expression is in three-address form.
data Expr ty ref ident val binop unop condexpr
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
    | Cond condexpr -- TODO I'm not sure if this is required - SIMPLE doesn't have it.
    deriving (Eq, Functor, Show)

-- | Conditional expressions are separate from expressions to restrict their use.
data CondExpr val relop
    = CondVal val
    | RelOp val relop val
    deriving (Eq, Functor, Show)

-- | A reference is either a naked value or a series of homogeneous array /
-- selector / slice operations.
data Ref ident val
    = ArrayRef ident [val]
    | SelectRef ident [ident]
    | SliceRef ident [(val, val, val)]
    | ValRef val
    deriving (Eq, Functor, Show)

-- | A Vigil value is either an identifier or a literal.
data Val ident lit
    = IdentVal ident
    | Literal lit
    deriving (Eq, Functor, Show)

-- | Arithmetic/integral binary operators.
data BinaryOp a
    = LogicalOr | LogicalAnd
    | Plus | Minus | Times | Divide | Modulo
    | ShiftLeft | ShiftRight | BitwiseAnd | BitwiseAndNot | BitwiseOr | BitwiseXor
    deriving (Eq, Functor, Ord, Read, Show)

-- | Relation operators (equality and ordering).
data RelOp a
    = Equal | NotEqual
    | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual

-- | Unary operators (unsupported GoLite unary operators are removed).
data UnaryOp a
    = Positive | Negative | BitwiseNot
    | LogicalNot

-- The rest of these definitions are exactly the same as for GoLite.

data TypeF ident int f
    = SliceType f
    | ArrayType int f
    | NamedType ident
    | StructType [(ident, f)]
    deriving (Eq, Functor, Ord, Read, Show)

data Literal a
    = IntLit VigilInt
    | FloatLit VigilFloat
    | RuneLit VigilRune
    | StringLit VigilString
    deriving (Eq, Functor, Show)

data Ident a
    = Ident
        { unIdent :: String
        }
    deriving (Eq, Functor, Show)

instance IsString (Ident a) where
    fromString = Ident

type VigilInt = Int
type VigilFloat = Double
type VigilRune = Char
type VigilString = String

