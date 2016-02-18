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

binaryOp :: e ~ Fix (ExprF id bin un lit ty) => bin -> e -> e -> e
binaryOp op x y = Fix $ BinaryOp op x y

unaryOp :: e ~ Fix (ExprF id bin un lit ty) => un -> e -> e
unaryOp op x = Fix $ UnaryOp op x
