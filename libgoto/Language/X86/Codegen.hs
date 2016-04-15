module Language.X86.Codegen where

import qualified Language.Common.GlobalId as Gid
import Language.Common.Pretty as P
import Language.GoLite.Types ( stringFromSymbol )
import Language.Vigil.Compile
import Language.Vigil.Simplify ( StringLitMap )
import Language.Vigil.Syntax
import Language.Vigil.Syntax.TyAnn
import Language.Vigil.Types
import Language.X86.Hardware
import Language.X86.HwAllocator
import Language.X86.HwTranslator
import Language.X86.Lifetime
import Language.X86.Virtual

import qualified Data.Map as M
import Data.Maybe ( fromMaybe )

data CodegenError
    = CodegenInvariantViolation String
    | HardwareTranslationError HardwareTranslationError
    deriving (Eq, Ord, Read, Show)

-- | The fmap 'Left' operator runs a function on the left component of an
-- 'Either', if any.
(<*$>) :: (l -> l') -> Either l r -> Either l' r
f <*$> e = case e of
    Left x -> Left (f x)
    Right x -> Right x

allocateRegisters
    :: Int
    -> VirtualAsm Int ()
    -> Either HardwareTranslationError (HardwareAsm Int ())
allocateRegisters stkSz v
    = runHardwareTranslation
    $ translate pairingMap hwlifetimes stkSz v
    where
        hwlifetimes = foldr f [] pairings
        f (virt, Reg szhwreg _) xs = (szhwreg, lifetimeMap M.! virt) : xs
        f (_, _) xs = xs
        lifetimeMap = computeLifetimes v
        pairings = allocate lifetimeMap
        pairingMap = M.fromList pairings

-- | Compile a type-annotated Vigil program into a full text file that can be
-- assembled by nasm.
codegen :: StringLitMap -> TyAnnProgram -> Either CodegenError Doc
codegen strs (Program { _globals = globals, _funcs = funcs, _main = main }) = do
    funcs' <- mapM genFunc funcs
    main' <- mapM genFunc main

    pure $
        text "section .data" $+$ nest indentLevel (
            vcat (
                map (uncurry genStr) (M.assocs $ strs)
            )
        ) $+$
        text "section .bss" $+$ nest indentLevel (
            vcat (
                map genGlobal globals
            )
        ) $+$
        text "section .text" $+$ nest indentLevel (
            vcat (
                funcs'
            ) $+$
            fromMaybe empty main'
        )

    where
        genFunc :: TyAnnFunDecl -> Either CodegenError Doc
        genFunc func@(FunDecl { _funDeclName = gid }) = do
            hwasm <- HardwareTranslationError <*$> allocateRegisters 0 (runCompiler func)
            pure $
                text (stringFromSymbol (gidOrigName gid)) <> text ":"
                $+$ nest indentLevel (
                    pretty hwasm
                )

        genGlobal :: VarDecl GlobalId -> Doc
        genGlobal (VarDecl (Gid.GlobalId { gidOrigName = name, gidTy = ty }))
            = text (stringFromSymbol name ++ ":")
            <+> text "resb" <+> P.int (storageSize ty)

        genStr :: GlobalId -> String -> Doc
        genStr (Gid.GlobalId { gidOrigName = name }) s
            = text (stringFromSymbol name ++ ": asciiz") <+> text (show s)
