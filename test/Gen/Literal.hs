{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Gen.Literal
( hexGenLower
, hexGenMixedCase
, octalGen
, decimalGen
, floatGen
, runeGen
, rawStringGen
, interpStringGen
, identGen
, typeGen
) where

import Gen.Core
import Data.Char ( chr )
import Language.GoLite.Syntax
import Test.QuickCheck.Gen ( Gen(MkGen) )

-- | Generates hexadecimal literals with lower-case digits
hexGenLower :: Gen String
hexGenLower = ("0x" ++) <$> mkDigits where
    mkDigits
        = sized
        . flip replicateM
        . elements
        $ ['0'..'9'] ++ ['a'..'f']

-- | Generates hexadecimal literals with mixed-case digits
hexGenMixedCase :: Gen String
hexGenMixedCase = (++) <$> prefixes <*> mkDigits where
    prefixes = elements ["0x", "0X"]
    mkDigits
        = sized
        . flip replicateM
        . elements
        $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']

-- | Generates decimal literals
decimalGen :: Gen String
decimalGen = (++) <$> start <*> rest where
    start = (:[]) <$> elements ['1'..'9']
    rest = sized . flip replicateM . elements $ ['0'..'9']

-- | Generates octal literals
octalGen :: Gen String
octalGen = ('0':) <$> (sized . flip replicateM . elements $ ['0'..'7'])

-- | Generates floating-point literals
floatGen :: Gen String
floatGen = (\x y -> x ++ "." ++ y) <$> decimalGen <*> decimalGen

-- | Generates rune literals (including escapes)
runeGen :: Gen String
runeGen = (surroundWith "'") <$> oneof [escape, normal] where
    escape = (\c -> "\\" ++ [c]) <$> elements "abfnrtv\\"
    normal = suchThat
                (((:[]) . chr) <$> choose (1, 126)) --Singleton string in 1..126
                (\c -> c /= "\n" && c /= "'" && c /= "\\")

-- | Generates raw string literals.
rawStringGen :: Gen String
rawStringGen = (surroundWith "`") <$> suchThat arbitrary (\s -> '`' `notElem` s)

-- | Generates interpreted string literals
-- A special generator is required to prevent generating invalid escape codes,
-- or strings containing invalid characters.
interpStringGen :: Gen String
interpStringGen
    =   (surroundWith "\"") . concat
    <$> listOf (frequency [(1, escape), (49, normal)]) where
            escape = (\c -> "\\" ++ [c]) <$> elements "abfnrtv\\"
            normal = suchThat
                (((:[]) . chr) <$> choose (33, 126))
                (\c -> c /= "\n" && c /= "\"" && c /= "\\")
                -- We don't want to introduce illegal characters or escape codes

-- | Generates identifiers.
identGen :: Gen String
identGen = (:) <$> start <*> rest where
    start = elements $ '_' : ['a'..'z'] ++ ['A'..'Z']
    rest = sized . flip replicateM . oneof $ [start, elements ['0'..'9']]

-- | Generates types, which may contain other generated types.
typeGen :: Gen BasicType
typeGen = sized typeGen' where
    typeGen' 0 = Fix <$> oneof [
                        liftM NamedType arbitrary,
                        -- "Constant generator" that always creates empty lists
                        liftM StructType (MkGen $ \_ _ -> []) ]

    -- The types become impossibly long if we don't reduce size exponentially.
    typeGen' n = let n' = n `div` 2 in
                    Fix <$> oneof [
                        liftM NamedType arbitrary,
                        liftM SliceType (typeGen' $ n'),
                        liftM2 ArrayType arbitrary (typeGen' n'),
                        liftM StructType (vectorOf n' (fieldGen n'))]

-- | Generates structure fields of a given size
fieldGen :: Int -> Gen ([BasicIdent], BasicType)
fieldGen x = liftM2 (,) (fieldGen' x) (resize 2 arbitrary) where
    fieldGen' 0 = (:[]) <$> (resize 3 arbitrary)
    fieldGen' n = (vectorOf n (resize 3 arbitrary))

instance Arbitrary BasicIdent where
   arbitrary = Ident <$> identGen

instance Arbitrary (Identity GoInt) where
    -- We don't have negative literals, only unary-minus expressions
    arbitrary = Identity <$> abs <$> arbitrarySizedIntegral

instance Arbitrary BasicType where
    arbitrary = typeGen
