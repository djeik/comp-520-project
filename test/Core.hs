module Core
( isRight
, isLeft
, parseOnly
, r
, int
, stringLit
, withParseOnly
, withWeed
, withParseAndWeed
, TestError ( .. )
, module Control.Monad
, module Test.Hspec
, module Test.Hspec.QuickCheck
, module Test.QuickCheck
, module Gen.Literal
, module Gen.Expr
, module Language.GoLite
, module Language.GoLite.Syntax.Sugar
, module Language.GoLite.Syntax.SrcAnn
, module Language.GoLite.Pretty
) where

import Control.Monad
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ( Positive, label )

import Text.Megaparsec
import Text.Megaparsec.String

import Gen.Literal
import Gen.Expr

import Language.GoLite
import Language.GoLite.Syntax.Sugar
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Pretty
import Language.GoLite.Weeder.Core

data TestError =
      Parse ParseError
    | Weed WeederExceptions
    deriving ( Show )

-- | Checks whether an instance of `Either` is `Right` or not
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- | Checks whether an instance of `Either` is `Left` or not
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | Runs an abitrary parser followed immediately by the end-of-file parser.
parseOnly :: Parser a -> String -> Either ParseError a
parseOnly m = parse (m <* eof) "test"

-- | Keystroke-saving synonym for `Right`.
r :: b -> Either a b
r = Right

-- | Keystroke-saving synonym for `(literal . IntLit)`.
int :: GoInt -> Fix (ExprF id bin un (Literal a) ty)
int = Language.GoLite.Syntax.Sugar.literal . IntLit

-- | Keystroke-saving synonym for `(literal . StringLit)`.
stringLit :: GoString -> Fix (ExprF id bin un (Literal a) ty)
stringLit = Language.GoLite.Syntax.Sugar.literal . StringLit


-- | Given a function to weed an element, weeds it and reinterprets any weeding
-- errors as test errors. When successful, returns its second argument.
withWeed :: (a -> Weeder ()) -> a -> Either TestError a
withWeed f a = do
    let start = WeederState { funcHasReturn = False
                            , forLevel = 0
                            , switchLevel = 0
                            , weedErrors = []}
    let runToState = (runStateT . runExceptT . runTraversal . runWeeder)
    let res = weedErrors $ snd $ runIdentity $ runToState (f a) start
    case res of
        [] -> Right a
        es -> Left $ Weed $ WeederExceptions es

-- | Run a given parser, then transform its result according to a given
-- function. Reinterprets parse errors as "test errors".
withParseOnly :: (a -> Either TestError b) -> Parser a -> String -> Either TestError b
withParseOnly f p s = do
    let res = parseOnly p s
    case res of
        Left e -> Left $ Parse e
        Right a -> f a

-- | Combines "withParseOnly" and "withWeed".
withParseAndWeed :: (a -> Weeder ()) -> Parser a -> String -> Either TestError a
withParseAndWeed w = withParseOnly (withWeed w)
