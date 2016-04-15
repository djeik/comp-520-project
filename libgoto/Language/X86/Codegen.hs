module Language.X86.Codegen where

import qualified Language.Common.GlobalId as Gid
import Language.Common.Pretty as P
import Language.GoLite.Types ( stringFromSymbol )
import Language.Vigil.Compile
import Language.Vigil.Simplify ( StringLitMap )
import Language.Vigil.Syntax
import Language.Vigil.Syntax.TyAnn
import Language.Vigil.Types
import Language.X86.Virtual

import qualified Data.Map as M
import Data.Maybe ( fromMaybe )

data CodegenError
    = CodegenInvariantViolation String
    deriving (Eq, Ord, Read, Show)

-- | Compile a type-annotated Vigil program into a full text file that can be
-- assembled by nasm.
codegen :: StringLitMap -> TyAnnProgram -> Either CodegenError Doc
codegen strs (Program { _globals = globals, _funcs = funcs, _main = main }) = do
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
                map genFunc funcs
            ) $+$
            fromMaybe empty (fmap genFunc main)
        )

    where
        genFunc :: TyAnnFunDecl -> Doc
        genFunc func@(FunDecl { _funDeclName = gid })
            = text (stringFromSymbol (gidOrigName gid)) <> text ":"
            $+$ nest indentLevel (
                pretty (runCompiler func :: VirtualAsm Int ())
            )

        genGlobal :: VarDecl GlobalId -> Doc
        genGlobal (VarDecl (Gid.GlobalId { gidOrigName = name, gidTy = ty }))
            = text (stringFromSymbol name ++ ":")
            <+> text "resb" <+> P.int (storageSize ty)

        genStr :: GlobalId -> String -> Doc
        genStr (Gid.GlobalId { gidOrigName = name }) s
            = text (stringFromSymbol name ++ ": asciiz") <+> text (show s)
