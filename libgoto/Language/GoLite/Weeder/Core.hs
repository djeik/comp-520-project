{-|
Module      : Language.GoLite.Weeder.Core
Description :
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Convenience functions and exports for the "Weeder" module.
-}

module Language.GoLite.Weeder.Core
( isDefaultCase
, isIdAsOperand
, errorOnBlankIdentifier
, isJust
, isNothing
  -- * Convenience re-exports
, module Language.Common.Monad.Traverse
, module Language.GoLite.Syntax
, module Language.GoLite.Syntax.SrcAnn
, module Language.GoLite.Weeder.Types
) where

import Data.Maybe ( isJust, isNothing )

import Language.Common.Monad.Traverse
import Language.GoLite.Syntax
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Weeder.Types

-- | Returns True just in case this case is the default case.
isDefaultCase :: (CaseHead e, f) -> Bool
isDefaultCase (CaseDefault, _) = True
isDefaultCase _ = False

-- | Returns True if the expression is exactly the supplied identifier standing
-- alone as an operand.
isIdAsOperand :: String -> SrcAnnExpr -> Bool
isIdAsOperand n (Fix (Ann _ (Variable (Ann _ (Ident n'))))) = n == n'
isIdAsOperand _ _ = False

-- | If the supplied identifier is blank, produces an error with the
-- identifier's position and the given error message.
errorOnBlankIdentifier :: SrcAnnIdent -> String -> Weeder ()
errorOnBlankIdentifier (Ann a (Ident n)) m =
    when (n == "_") (reportError $ WeederException a m)
