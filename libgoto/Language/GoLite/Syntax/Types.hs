{-|
Module      : Language.GoLite.Syntax.Types
Description : GoLite syntax definitions and instances
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

The core definitions for the GoLite abstract syntax tree.
-}

{-# LANGUAGE DeriveFunctor #-}

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
, Comment(..)
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

import Language.GoLite.Pretty
import Language.GoLite.Syntax.Types.Decl
import Language.GoLite.Syntax.Types.Type
import Language.GoLite.Syntax.Types.Stmt
import Language.GoLite.Syntax.Types.Expr
import Language.GoLite.Syntax.Types.Ops

import Data.Functor.Foldable ( Fix(..) )
import Data.Functor.Identity ( Identity(..) )
import Text.PrettyPrint

newtype Comment a
    = Comment { unComment :: a }
    deriving (Eq, Functor, Ord, Read, Show)

-- | Prints the contents in a block-comment. This is unsafe in the sense that
-- if the pretty-printed contents contains \"*/\", you're fucked.
instance Pretty a => Pretty (Comment a) where
    pretty e = text "/*" <+> pretty e <+> text "*/"
