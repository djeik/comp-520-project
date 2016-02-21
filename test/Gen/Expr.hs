{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Gen.Expr
( exprGen
) where

import Gen.Core
import Gen.Literal

import Test.QuickCheck.Gen ( Gen(MkGen) )

-- | Generates expressions.
exprGen :: Gen BasicExpr
exprGen = sized exprGen' where
    exprGen' 0 = Fix <$> oneof [ liftM Literal arbitrary,
                                liftM Variable arbitrary ]
    exprGen' n =
        let n' = n `div` 2 in
        let recGen = (exprGen' n') in
        -- Types here are "small arbitrary" since we don't want humongous types.
        Fix <$> oneof [
                liftM3 BinaryOp arbitrary recGen recGen,
                liftM2 UnaryOp arbitrary recGen,
                liftM2 Conversion smallArbitrary recGen,
                liftM2 Selector recGen arbitrary,
                liftM2 Index recGen recGen,
                liftM4 Slice recGen
                            (maybeGen recGen)
                            (maybeGen recGen)
                            (maybeGen recGen),
                liftM2 TypeAssertion recGen smallArbitrary,
                -- Since a call with only a named type and no arguments looks
                -- exactly like a call with one argument, we don't generate
                -- calls with types. Those will be tested manually.
                liftM3 Call recGen
                            (MkGen $ \_ _ -> Nothing)
                            (vectorOf n' (exprGen' 0))]

instance Arbitrary BasicBinaryOp where
    arbitrary = elements [ LogicalOr, LogicalAnd, Equal, NotEqual, LessThan
        , LessThanEqual, GreaterThan, GreaterThanEqual, Plus, Minus, BitwiseOr
        , BitwiseXor, Times, Divide, Modulo, ShiftLeft, ShiftRight, BitwiseAnd
        , BitwiseAndNot ]

instance Arbitrary BasicUnaryOp where
    arbitrary = elements [Positive, Negative, LogicalNot, BitwiseNot
        , Dereference, Reference, Receive]

instance Arbitrary BasicExpr where
    arbitrary = exprGen