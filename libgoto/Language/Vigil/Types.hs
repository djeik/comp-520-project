{-|
Module      : Language.Vigil.SimplifyTraversal
Description : Convert a typechecked GoLite program to a Vigil program
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental

Contains the transformation functions from a typechecked GoLite AST to a Vigil
AST.
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.Vigil.Types
( -- * The Vigil type system
  Type
, TypeF(..)
  -- ** Smart constructors
, boolType
, intType
, floatType
, structType
, sliceType
, funcType
, stringType
, voidType
, builtinType
  -- ** Converting from GoLite
, reinterpretType
, selectorOffsetFor
  -- * Storage
, StorageSize(..)
, IStorage
, FStorage
, ptrStorage
  -- * Global identifiers
, GlobalId
, G.gidNum
, G.gidTy
, G.gidOrigName
, artificialGlobalId
  -- ** Converting from GoLite
, reinterpretGlobalId
) where

import Language.Common.Annotation ( bare )
import Language.Common.Pretty
import qualified Language.Common.GlobalId as Gid
import qualified Language.GoLite.Types as G

import Data.Functor.Foldable ( Fix(..), cata )

-- | Types having an associated storage requirement as a number of bytes needed
-- to represent the data.
class StorageSize a where
    -- | Compute the number of bytes needed to store a value of this type.
    storageSize :: a -> Int

-- | A storage requirement for an integer.
data IStorage
    = I1
    | I2
    | I4
    | I8
    deriving (Eq, Ord, Read, Show)

instance StorageSize IStorage where
    storageSize s = case s of
        I1 -> 1
        I2 -> 2
        I4 -> 4
        I8 -> 8

instance Pretty IStorage where
    pretty = text . show . storageSize

-- | The possible storage requirements for a float.
data FStorage
    = F4
    | F8
    deriving (Eq, Ord, Read, Show)

-- | The storage requirement for a pointer.
ptrStorage :: IStorage
ptrStorage = I8

instance StorageSize FStorage where
    storageSize s = case s of
        F4 -> 4
        F8 -> 8

instance Pretty FStorage where
    pretty = text . show . storageSize

-- | The Vigil type base functor.
data TypeF subTy
    = IntType IStorage
    | FloatType FStorage
    | StructType
        { structSize :: Int
        }
    | ArrayType subTy
    | SliceType subTy
    | FuncType
        { funcArgs :: [subTy]
        , funcRetTy :: subTy
        }
    | StringType
    | VoidType
    | BuiltinType G.BuiltinType
    deriving (Eq, Functor, Ord, Read, Show)

type Type = Fix TypeF

instance Pretty Type where
    pretty (Fix t') = case t' of
        IntType n -> text "int" <> pretty n
        FloatType n -> text "float" <> pretty n
        StructType fs -> text "struct_" <> pretty fs
        ArrayType t -> text "[0]" <> pretty t
        SliceType t -> text "[]" <> pretty t
        FuncType ps r -> text "func"
            <+> prettyParens True (hcat $ punctuate comma (map pretty ps))
            <+> pretty r
        StringType -> text "string"
        VoidType -> text "void"
        BuiltinType _ -> text "builtin"


instance StorageSize (TypeF a) where
    storageSize t = case t of
        IntType s -> storageSize s
        FloatType s -> storageSize s
        StructType { structSize = n } -> n
        ArrayType _ -> storageSize ptrStorage
        SliceType _ -> storageSize ptrStorage
        FuncType {} -> storageSize ptrStorage
        StringType -> storageSize ptrStorage
        BuiltinType _ -> storageSize ptrStorage
        VoidType -> 0

instance StorageSize Type where
    storageSize = cata storageSize

type GlobalId = Gid.GlobalId Type (G.Symbol ())

instance Pretty GlobalId where
    pretty i =
        pretty (Gid.gidOrigName i)
        <> text "_"
        <> pretty (Gid.gidNum i)

boolType :: Type
boolType = Fix $ IntType I1

intType :: IStorage -> Type
intType = Fix . IntType

floatType :: FStorage -> Type
floatType = Fix . FloatType

structType :: Int -> Type
structType = Fix . StructType

arrayType :: Type -> Type
arrayType = Fix . ArrayType

sliceType :: Type -> Type
sliceType = Fix . SliceType

funcType :: [Type] -> Type -> Type
funcType x y = Fix $ FuncType x y

stringType :: Type
stringType = Fix StringType

voidType :: Type
voidType = Fix VoidType

builtinType :: G.BuiltinType -> Type
builtinType = Fix . BuiltinType

reinterpretType :: G.Type -> Either String Type
reinterpretType = cata f where
    f :: G.GoTypeF (Either String Type) -> Either String Type
    f goliteType = case goliteType of
        -- unrepresentable types in Vigil
        G.NilType -> Left "NilType is unrepresentable"
        G.UnknownType -> Left "UnknownType is unrepresentable"
        G.TypeSum _ -> Left "TypeSum is unrepresentable"

        -- discard type aliasing information
        G.AliasType _ m -> m

        -- basic numerical types
        G.IntType _ -> pure $ intType I8
        G.RuneType _ -> pure $ intType I1
        G.FloatType _ -> pure $ floatType F8
        G.BoolType _ -> pure $ intType I1

        -- more complicated types
        G.VoidType -> pure voidType
        G.StringType _ -> pure stringType
        G.FuncType { G.funcTypeArgs = args, G.funcTypeRet = ret } -> do
            args' <- sequence (snd <$> args)
            ret' <- ret
            pure $ funcType args' ret'
        G.Array _ m -> arrayType <$> m
        G.Slice m -> sliceType <$> m
        G.Struct { G.structTypeFields = fields } ->
            structType . sum . map storageSize <$> sequence (snd <$> fields)
        G.BuiltinType b -> pure $ builtinType b

-- | Convert a GoLite global identifier into a Vigil global identifier, with
-- the possibility of failure.
--
-- The failure can occur due to a GoLite type embedded in the identifier that
-- cannot be represented in Vigil. Such a situation arising is however a bug.
--
-- The underlying number of the global identifier is unchanged by this
-- operation.
reinterpretGlobalId :: G.GlobalId -> Either String GlobalId
reinterpretGlobalId g = do
    ty <- reinterpretType (G.gidTy g)
    pure g
        { G.gidTy = ty
        , G.gidOrigName = bare (G.gidOrigName g)
        }

-- | Creates an artificial global ID with the given number, name and type.
-- Warning! Calling this runs the risk of creating a collision in the numbers of
-- global IDs. If that happens, you have no one to blame but yourself.
artificialGlobalId :: Int -> String -> Type -> GlobalId
artificialGlobalId nu on ty =
    Gid.GlobalId
        { Gid.gidNum = nu
        , Gid.gidOrigName = G.symbolFromString on
        , Gid.gidTy = ty
        , Gid.gidOrigin = Gid.Local
        }


-- | Computes the offset of a given identifier in a field list.
selectorOffsetFor :: [(G.Symbol a, Type)] -> String -> Maybe Int
selectorOffsetFor xs i = case splitWhen ((== sym) . fst) xs of
    (_, []) -> Nothing
    (ys, _) -> pure . sum . map (storageSize . snd) $ ys
    where
        sym :: G.Symbol a
        sym = G.symbolFromString i

        splitWhen :: (a -> Bool) -> [a] -> ([a], [a])
        splitWhen _ [] = ([], [])
        splitWhen p (s:ss)
            | p s = ([], s:ss)
            | otherwise = let (ys, zs) = splitWhen p ss in (s:ys, zs)
