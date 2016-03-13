{-|
Module      : Language.GoLite.Weeder.Expr
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

"Weeder" traversal definition for expressions.
-}

module Language.GoLite.Weeder.Expr
( weedExpr
) where

import Language.GoLite.Weeder.Core
import Language.GoLite.Weeder.TypeLit

{-| Weeds an expression and its sub-expressions.

    * If a call has a type, the callee must be @make@. (Note that the converse,
        if the callee is @make@, then the call must have a type, cannot be
        reliably checked without a symbol table.)
    * The blank identifier may not be used in an expression. Cases where this is
      in fact allowed should deal with the blank identifier separately instead
      of calling "weedExpr".
-}
weedExpr :: SrcAnnExpr -> Weeder ()
-- Binary operator: weed both sides.
weedExpr (Fix (Ann _ (BinaryOp _ l r))) = do
    weedExpr l
    weedExpr r

-- Unary operator: weed the operand.
weedExpr (Fix (Ann _ (UnaryOp _ e))) = weedExpr e

-- Conversion: weed the type and operand.
weedExpr (Fix (Ann _ (Conversion ty e))) = do
    weedType ty
    weedExpr e

-- Selector: check that the identifier is not the blank identifier, then weed
-- the operand.
weedExpr (Fix (Ann _ (Selector e i))) = do
    errorOnBlankIdentifier i "cannot refer to blank field"
    weedExpr e

-- Index: weed the indexer and indexee.
weedExpr (Fix (Ann _ (Index e v))) = do
    weedExpr e
    weedExpr v

-- Slice: weed the expression and all slice parts. We don't check that the
-- combinations of Maybes is valid since the parser should reject it.
weedExpr (Fix (Ann _ (Slice e l h b))) = do
    weedExpr e
    void $ pure (weedExpr <$> l)
    void $ pure (weedExpr <$> h)
    void $ pure (weedExpr <$> b)

-- Type assertion: weed the expression and type being asserted.
weedExpr (Fix (Ann _ (TypeAssertion e ty))) = do
    weedExpr e
    weedType ty

-- Call: check that if a type is present, make is used. Then, weed the callee
-- and params expressions, as well as the type.
weedExpr (Fix (Ann _ (Call callee ty params))) = do
    -- We can check for this here because assigning built-ins is illegal, so
    -- if we have a type in the call, we better be calling something called
    -- "make". This won't catch cases where something else shadows the "make"
    -- built-in but those can be verified later.
    when (isJust ty && not ("make" `isIdAsOperand` callee))
        (let Just ty' = ty in
        reportError (topAnn ty', "type is not an expression"))

    weedExpr callee
    void $ pure (weedType <$> ty)
    void $ mapM weedExpr params

-- Literals: literally nothing.
weedExpr (Fix (Ann _ (Literal _))) = pure ()

-- Variable: not the blank identifier.
weedExpr (Fix (Ann _ (Variable v))) =
    errorOnBlankIdentifier v "_ cannot be used as a value"