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
import Language.GoLite.Lexer.Literal ( commonEscapes, escapedChars )
import qualified Data.Map.Strict as Map -- To used `escapedChars`


import Data.Char ( chr )
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
    escape = (\c -> "\\" ++ [c]) <$> elements "abfnrtv"
    normal = suchThat
                -- Restrict the range of characters to those that are printable.
                (((:[]) . chr) <$> choose (33, 126))
                -- Don't pick a character that's illegal in a rune.
                (\c -> c /= "'" && c /= "\\")

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
            escape = (\c -> "\\" ++ [c]) <$> elements commonEscapes
            normal = suchThat
                (((:[]) . chr) <$> choose (33, 126))
                (\c -> c /= "\n" && c /= "\"" && c /= "\\")
                -- We don't want to introduce illegal characters or escape codes

-- | Generates identifiers.
identGen :: Gen String
identGen = suchThat identGen' notKw where
    identGen' = (:) <$> start <*> rest
    start = elements $ '_' : ['a'..'z'] ++ ['A'..'Z']
    rest = sized . flip replicateM . oneof $ [start, elements ['0'..'9']]
    notKw  = \c -> c `notElem` ["break", "return", "continue", "fallthrough",
            "print", "println", "read", "var", "struct", "type", "if", "else",
            "for", "switch", "case", "default", "package", "func"]

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
    -- Also in order to avoid some errors, we restrict the range to 1024
    arbitrary = Identity <$> ((flip mod) 1024) <$> arbitraryPositiveIntegral

instance Arbitrary BasicType where
    arbitrary = typeGen

instance Arbitrary BasicLiteral where
    -- Use our own generators for strings and runes to ensure we generate valid
    -- escape codes. Also make sure that literals are positive.
    arbitrary = oneof [ liftM IntLit arbitraryPositiveIntegral,
                        liftM FloatLit (abs <$> arbitrary),
                        liftM StringLit (unsurround interpStringGen),
                        liftM RuneLit (fmap runeToChar runeGen)]
        where
            unsurround = fmap (reverse . tail . reverse . tail)
            runeToChar s = let c = s !! 1 in case c of
                '\\' -> escapedChars Map.! (s !! 2)
                _ -> c

-- | Generates an arbitrary positive integer.
arbitraryPositiveIntegral :: Gen GoInt
arbitraryPositiveIntegral = abs <$> arbitraryBoundedIntegral
