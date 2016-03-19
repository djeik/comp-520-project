{-|
Module      : Language.GoLite.Syntax.Types
Description : GoLite syntax definitions and instances
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

The core definitions for the GoLite abstract syntax tree.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.GoLite.Syntax.Types
( Package(..)
, TopLevelDecl(..)
, VarDecl(..)
, TypeDecl(..)
, FunDecl(..)
, TypeF(..)
, StatementF(..)
, CaseHead(..)
, Declaration(..)
, ExprF(..)
, Literal(..)
, Ident(..)
, GoInt
, GoFloat
, GoRune
, GoString
  -- * Operators
, BinaryOp(..)
, UnaryOp(..)
, AssignOp(..)
, isComparisonOp
, isLogicalOp
, isArithmeticOp
, isOrderingOp
  -- * Convenience reexports
, Fix(..)
, Identity(..)
) where

import Language.GoLite.Syntax.Types.Decl
import Language.GoLite.Syntax.Types.Type
import Language.GoLite.Syntax.Types.Stmt
import Language.GoLite.Syntax.Types.Expr
import Language.GoLite.Syntax.Types.Ops

import Data.Functor.Foldable ( Fix(..) )
import Data.Functor.Identity ( Identity(..) )
