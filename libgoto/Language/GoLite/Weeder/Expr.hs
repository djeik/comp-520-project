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

import Data.Functor.Foldable ( cata )

{-| Weeds an expression and its sub-expressions.

    * The blank identifier may not be used in an expression. Cases where this is
      in fact allowed should deal with the blank identifier separately instead
      of calling "weedExpr".
    * The reference, dereference and receive operators are unsupported.
    * Types in an expression are weeeded.
-}
weedExpr :: SrcAnnExpr -> Weeder ()
weedExpr = cata phi where

    -- Unary operator: weed the operator.
    phi (Ann a (UnaryOp o _)) =
        let o' = bare o in
        when (o' == Dereference || o' == Reference || o' == Receive)
            (reportError (a, "Unsupported operator"))

    -- Conversion: weed the type
    phi (Ann _ (Conversion ty _)) = weedType ty

    -- Selector: check that the identifier is not the blank identifier
    phi (Ann _ (Selector _ i)) =
        errorOnBlankIdentifier i "cannot refer to blank field"

    -- Type assertion: unsupported
    phi (Ann a (TypeAssertion _ _)) =
        reportError (a, "Type assertions are unsupported")

    -- Call: weed the type
    phi (Ann _ (Call _ ty _)) = do
        void $ pure (weedType <$> ty)

    -- Variable: not the blank identifier.
    phi (Ann _ (Variable v)) =
        errorOnBlankIdentifier v "_ cannot be used as a value"

    -- Others: nothing
    phi _ = pure ()