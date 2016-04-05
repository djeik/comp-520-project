{-|
Module      : Language.GoLite.Pretty
Description : Pretty-printer definition and combinators.
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Specific definitions for GoLite pretty-printing.
-}

module Language.GoLite.Pretty
( module Language.Common.Pretty
, goLiteStyle
, renderGoLite
) where


import Language.Common.Pretty

-- | Renders a 'Doc' with GoLite-specific settings.
--
-- In particular, line length is set to infinity to avoid dangerous line
-- wrapping that could introduce erroneous semicolons.
renderGoLite :: Doc -> String
renderGoLite = renderStyle goLiteStyle

-- | The rendering style for GoLite 'Doc's.
goLiteStyle :: Style
goLiteStyle = style
    { lineLength = maxBound
    }
