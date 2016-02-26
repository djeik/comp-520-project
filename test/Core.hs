module Core
( isRight
, isLeft
, parseOnly
, r
, int
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