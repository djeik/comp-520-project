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
  -- * Convenience re-exports
, module Language.GoLite.Monad.Traverse
, module Language.GoLite.Syntax
, module Language.GoLite.Syntax.SrcAnn
, module Language.GoLite.Weeder.Types
) where

import Language.GoLite.Monad.Traverse
import Language.GoLite.Syntax
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Weeder.Types

-- | Returns True just in case this case is the default case.
isDefaultCase :: (CaseHead e, f) -> Bool
isDefaultCase (CaseDefault, _) = True
isDefaultCase _ = False