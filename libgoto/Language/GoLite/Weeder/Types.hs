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

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.GoLite.Weeder.Types where

import Data.Void ( Void )

import Language.GoLite.Monad.Traverse
import Language.GoLite.Syntax.SrcAnn

-- | Errors that can occur during weeding are tagged with a source position and
-- a string detailing the error.
type WeederException = (SrcSpan, String)

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
    deriving (Show)

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