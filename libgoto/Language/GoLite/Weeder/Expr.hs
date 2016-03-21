{-|
Module      : Language.GoLite.Weeder.Expr
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

"Weeder" traversal definition for expressions.
-}

{-# LANGUAGE TypeFamilies #-}

module Language.GoLite.Weeder.Expr
( weedExpr
) where

import Language.GoLite.Weeder.Core
import Language.GoLite.Weeder.TypeLit

{-| Weeds an expression and its sub-expressions.

    * The blank identifier may not be used in an expression. Cases where this is
      in fact allowed should deal with the blank identifier separately instead
      of calling "weedExpr".
    * The reference, dereference and receive operators are unsupported.
    * Types in an expression are weeeded.
-}
weedExpr :: SrcAnnExpr -> Weeder ()
-- Binary operator: weed both sides.
weedExpr (Fix (Ann _ (BinaryOp _ l r))) = weedExpr l >> weedExpr r

-- Unary operator: weed the operand.
weedExpr (Fix (Ann _ (UnaryOp _ e))) = weedExpr e

-- Conversion: weed the type and operand.
weedExpr (Fix (Ann _ (Conversion ty e))) = weedType ty >> weedExpr e

-- Selector: check that the identifier is not the blank identifier, then weed
-- the operand.
weedExpr (Fix (Ann _ (Selector e i))) = do
    errorOnBlankIdentifier i "cannot refer to blank field"
    weedExpr e

-- Index: weed the indexer and indexee.
weedExpr (Fix (Ann _ (Index e v))) = weedExpr e >> weedExpr v

-- Slice: weed the expression and all slice parts. We don't check that the
-- combinations of Maybes is valid since the parser should reject it.
weedExpr (Fix (Ann _ (Slice e l h b))) = do
    weedExpr e
    void $ pure (weedExpr <$> l)
    void $ pure (weedExpr <$> h)
    void $ pure (weedExpr <$> b)

-- Type assertion: weed the expression and type being asserted.
weedExpr (Fix (Ann a (TypeAssertion _ _))) =
    (reportError $ WeederException a "type assertions are unsupported")

-- Call: check that if a type is present, make is used. Then, weed the callee
-- and params expressions, as well as the type.
weedExpr (Fix (Ann _ (Call callee ty params))) = do
    weedExpr callee
    void $ pure (weedType <$> ty)
    void $ mapM weedExpr params

-- Literals: literally nothing.
weedExpr (Fix (Ann _ (Literal _))) = pure ()

-- Variable: not the blank identifier.
weedExpr (Fix (Ann _ (Variable v))) =
    errorOnBlankIdentifier v "_ cannot be used as a value"