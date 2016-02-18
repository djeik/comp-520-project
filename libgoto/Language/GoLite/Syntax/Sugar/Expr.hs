{-|
Module      : Language.GoLite.Syntax.Sugar.Expr
Description : Syntax sugar for synthesizing expressions
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE TypeFamilies #-}

module Language.GoLite.Syntax.Sugar.Expr
( module Language.GoLite.Syntax.Sugar.Expr
, binaryOp
, unaryOp
) where

import Language.GoLite.Syntax.Types
import Language.GoLite.Syntax.Sugar.Ops (binaryOp, unaryOp)

conversion :: e ~ Fix (ExprF id bin un lit ty) => ty -> e -> e
conversion t e = Fix $ Conversion t e

selector :: e ~ Fix (ExprF id bin un lit ty) => e -> id -> e
selector e i = Fix $ Selector e i

index :: e ~ Fix (ExprF id bin un lit ty) => e -> e -> e
index e i = Fix $ Index e i

slice
    :: e ~ Fix (ExprF id bin un lit ty)
    => e
    -> Maybe e
    -> Maybe e
    -> Maybe e
    -> e
slice e lo hi bound = Fix $ Slice e lo hi bound

typeAssertion :: e ~ Fix (ExprF id bin un lit ty) => e -> ty -> e
typeAssertion e ty = Fix $ TypeAssertion e ty

call :: e ~ Fix (ExprF id bin un lit ty) => e -> Maybe ty -> [e] -> e
call e ty params = Fix $ Call e ty params

literal :: e ~ Fix (ExprF id bin un lit ty) => lit -> e
literal = Fix . Literal

variable :: e ~ Fix (ExprF id bin un lit ty) => id -> e
variable = Fix . Variable
