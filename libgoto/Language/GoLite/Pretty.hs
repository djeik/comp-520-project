{-|
Module      : Language.GoLite.Pretty
Description : Pretty-printer definition and combinators.
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

The "Pretty" class is essentially a clone of the "Text.Show.Show" class, but
instead of aiming to output valid Haskell code, "Pretty" aims to output valid
GoLite code.
-}

{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.GoLite.Pretty 
( Pretty(..)
, prettyParens
, prettyBrackets
, prettyBraces
, indentLevel
, goLiteStyle
, renderGoLite
) where

import Control.Applicative ( Const(..) )
import Data.Functor.Identity
import Text.PrettyPrint

-- | Essentially a clone of 'Text.Show.Show'.
class Pretty a where
    prettyPrec :: Int -> a -> Doc
    pretty :: a -> Doc

    pretty = prettyPrec 0
    prettyPrec _ = pretty

instance Pretty Char where
    pretty = char

instance Pretty a => Pretty (Maybe a) where
    prettyPrec d e = case e of
        Just x -> prettyPrec d x
        Nothing -> empty

instance Pretty Doc where
    pretty = id

instance Pretty Int where
    pretty = int

instance Pretty Double where
    pretty = double

instance Pretty a => Pretty (Const a b) where
    pretty = pretty . getConst

deriving instance Pretty a => Pretty (Identity a)

-- | Selects a user-supplied 'Doc'-transforming function if the given boolean
-- is true; else, the identity function for 'Doc' is selected.
prettyIf :: Bool -> (Doc -> Doc) -> (Doc -> Doc)
prettyIf b f = case b of
    True -> f
    False -> id

-- | Wraps a 'Doc' in parentheses if the boolean is true.
prettyParens :: Bool -> Doc -> Doc
prettyParens = flip prettyIf parens

-- | Wraps a 'Doc' in square brackets if the boolean is true.
prettyBrackets :: Bool -> Doc -> Doc
prettyBrackets = flip prettyIf brackets

-- | Wraps a 'Doc' in braces if the boolean is true.
prettyBraces :: Bool -> Doc -> Doc
prettyBraces = flip prettyIf braces

-- | The standard indentation level is four spaces.
indentLevel :: Num a => a
indentLevel = 4

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
