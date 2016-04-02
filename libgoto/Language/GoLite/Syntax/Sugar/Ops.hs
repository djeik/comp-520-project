{-|
Module      : Language.GoLite.Syntax.Sugar.Expr
Description : Syntax sugar for synthesizing BinaryOp and UnaryOp expressions
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE TypeFamilies #-}

module Language.GoLite.Syntax.Sugar.Ops where

import Language.GoLite.Syntax.Types

-- | Creates an expression tree for a given binary operator.
binaryOp :: e ~ Fix (ExprF selId id bin un lit ty) => bin -> e -> e -> e
binaryOp op x y = Fix $ BinaryOp op x y

-- | Creates an expression tree for a given unary operator.
unaryOp :: e ~ Fix (ExprF selId id bin un lit ty) => un -> e -> e
unaryOp op x = Fix $ UnaryOp op x
