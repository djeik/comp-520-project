module Language.X86.Mangling
( mangleFuncName
) where

import System.Info ( os )

mangleFuncName :: String -> String
mangleFuncName s
    | os == "darwin" = '_' : s
    | otherwise = s
