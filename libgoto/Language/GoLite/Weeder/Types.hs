{-|
Module      : Language.GoLite.Typecheck.Types
Description : Definition of the Typecheck monad
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Defines the @Weeder@ monad as an instance of
'Language.GoLite.Monad.Traverse.MonadTraversal'.
The weeder is used to detect errors that are difficult to catch at parsing time.
The errors detected are described in the other submodules of
'Language.GoLite.Weeder'; this module only specifies the types required.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.GoLite.Weeder.Types where

import Data.Void ( Void )

import Text.PrettyPrint

import Language.GoLite.Monad.Traverse
import Language.GoLite.Pretty
import Language.GoLite.Syntax.SrcAnn

-- | Errors that can occur during weeding are tagged with a source position and
-- a string detailing the error.
data WeederException = WeederException
    { sourcePos :: SrcSpan
    , errorMsg :: String
    }
    deriving ( Show )

-- | Shows the source position, followed by a colon, followed by the error.
instance Pretty WeederException where
    pretty e =  (text $ show $ sourcePos e)
            <> text ":"
            <+> (text $ errorMsg e)

-- | Used to declare a custom pretty instance for a list of weeder exceptions.
newtype WeederExceptions = WeederExceptions
    { weederErrors :: [WeederException] }
    deriving ( Show )

-- | Shows every exception on its own line instead of a standard list.
instance Pretty WeederExceptions where
    pretty es = hcat $ punctuate (text "\n") (map pretty $ weederErrors es)

-- | Internal state used by the weeder.
data WeederState
    = WeederState
        { funcHasReturn :: Bool
        -- ^ Specifies whether the function we are in has a declared return type
        -- or not. Outside of functions, the value is undetermined.
        , forLevel :: Int
        -- ^ Indicates the current level of for-loop nesting.
        , switchLevel :: Int
        -- ^ Indicates the current level of switch nesting.
        , weedErrors :: [WeederException]
        -- ^ Errors accumulated so far.
        }

-- | Increases the level of for-loop nesting by one.
incForLevel :: WeederState -> WeederState
incForLevel s = s { forLevel = forLevel s + 1 }

-- | Decreases the level of for-loop nesting by one.
decForLevel :: WeederState -> WeederState
decForLevel s = s { forLevel = forLevel s - 1 }

-- | Increases the level of switch nesting by one.
incSwitchLevel :: WeederState -> WeederState
incSwitchLevel s = s { switchLevel = switchLevel s + 1 }

-- | Decreases the level of switch nesting by one.
decSwitchLevel :: WeederState -> WeederState
decSwitchLevel s = s { switchLevel = switchLevel s - 1 }

-- | The @Weeder@ monad is a traversal that has no dedicated error facilities.
-- Therefore, it does not produce fatal errors (non-fatal errors are accumulated
-- in the state).
newtype Weeder a
    = Weeder { runWeeder :: Traversal Void WeederState a }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadState WeederState
        , MonadError Void
        )

instance MonadTraversal Weeder where
    type TraversalError Weeder = WeederException
    type TraversalState Weeder = WeederState
    type TraversalException Weeder = Void

    reportError e = modify $ \s -> s { weedErrors = e:(weedErrors s) }

    getErrors s = weedErrors s