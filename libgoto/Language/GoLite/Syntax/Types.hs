{-|
Module      : Language.GoLite.Syntax.Types
Description : GoLite syntax definitions and instances
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

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
, BinaryOp(..)
, UnaryOp(..)
, AssignOp(..)
, Literal(..)
, Ident(..)
, GoInt
, GoFloat
, GoRune
, GoString
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
