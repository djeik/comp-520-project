{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Gen.Expr
( exprGen
) where

import Gen.Core
import Gen.Literal ()

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
                -- Don't want to generate a conversion of a variable from a
                -- named type, which is indistinguishable from a call. A type
                -- of size 2 can never be a named type.
                liftM2 Conversion (resize 2 arbitrary) recGen,
                liftM2 Selector recGen arbitrary,
                liftM2 Index recGen recGen,
                -- First type of slice: no max, optional high and low
                liftM4 Slice recGen
                            (maybeGen recGen)
                            (maybeGen recGen)
                            genNothing,
                -- Second type of slice: optional low, required high and max
                liftM4 Slice recGen
                            (maybeGen recGen)
                            (Just <$> recGen)
                            (Just <$> recGen),
                liftM2 TypeAssertion recGen smallArbitrary,
                -- Since a call with only a named type and no arguments looks
                -- exactly like a call with one argument, we don't generate
                -- calls with types. Those will be tested manually.
                liftM3 Call recGen
                            genNothing
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
