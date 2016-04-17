{-# LANGUAGE ViewPatterns #-}

module Main where

import Language.Cpp.Compile ( runCompiler )
import qualified Language.GoLite as G
import qualified Language.Vigil as V
import Language.GoLite.Pretty hiding ( (<>) )
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Syntax.Typecheck
import Language.GoLite.Typecheck
import Language.GoLite.Typecheck.Types
import Language.X86.Codegen ( codegen )

import Control.Monad ( forM_, when )
import Data.List ( sortBy )
import Data.Ord ( comparing )
import Options.Applicative
import System.Exit ( exitSuccess, exitFailure )

import System.IO
import System.FilePath ( (-<.>) )

data InputFile
    = Stdin
    | FilePath FilePath
    deriving (Eq, Ord, Read)

instance Show InputFile where
    show Stdin = "stdin"
    show (FilePath p) = p

parseInputFile :: String -> InputFile
parseInputFile "-" = Stdin
parseInputFile x = FilePath x

data Goto
    = Goto
        { cmd :: GotoCmd
        -- ^ The actual command
        , oneError :: Bool
        -- ^ Flag to print only the first error.
        , dumpSymTab :: Bool
        -- ^ Whether or not to dump the top frame of the symtab on scope exits
        , ppType :: Bool
        -- ^ Whether or not to pretty-print types
        , dumpVigil :: Bool
        }
    deriving (Eq, Show)

data GotoCmd
    = Pretty
        { filename :: InputFile
        }
    | Cpp
        { filename :: InputFile
        }
    deriving (Eq, Ord, Read, Show)

main :: IO ()
main = execParser cmdParser >>= goto

cmdParser :: ParserInfo Goto
cmdParser
    = info (
        Goto <$> subparser (
            command "pretty" (
                info (
                    Pretty <$> fmap parseInputFile (
                        strArgument (
                            metavar "[FILE]" <>
                            value "-"
                        )
                    )
                ) $
                briefDesc <>
                progDesc "Pretty-prints the input."
            ) <>
            command "cpp" (
                info (
                    Cpp <$> fmap parseInputFile (
                        strArgument (
                            metavar "[FILE]" <>
                            value "-"
                        )
                    )
                ) $
                briefDesc <>
                progDesc "Compiles the input file to C++."
            )
        ) <*>
        ( switch
            ( long "oneError"
              <> help "Specifies that only the first error should be printed."
            )
        ) <*>
        ( switch
            ( long "dumpSymTab"
              <> help "When specified, dumps the top frame of the symbol table \
                    \at each scope exit"
            )
        ) <*>
        ( switch
            ( long "pptype"
              <> help "When specified, pretty-print types in addition to program"
            )
        ) <*>
        ( switch
            ( long "dump-vigil"
                <> help "causes the compiler to dump the vigil code and quit"
            )
        )
    ) $
    progDesc "Compiler for GoLite"

noNewLines :: String -> String
noNewLines = foldr (\a b -> (if a == '\n' then ' ' else a) : b) []

typecheckFile :: Bool -> InputFile -> IO (TySrcAnnPackage, TypecheckState)
typecheckFile oneErr f = do
    ex <- parseGoLiteFile f
    case ex of
        Left e -> do
            hPutStrLn stderr $ show e
            exitFailure
        Right r -> do
            case weedGoLiteProgram oneErr r of
                Just es -> do
                    hPutStrLn stderr $ renderGoLite (pretty es)
                    exitFailure
                Nothing -> do
                    case runTypecheck (G.typecheckPackage r) of
                        (Left fatal, _) -> do
                            print fatal
                            exitFailure
                        (Right p, s) -> do
                            case reverse $ sortBy (comparing typeErrorLocation) (_errors s) of
                                [] -> do
                                    pure (p, s)

                                xs -> do
                                    forM_ (if oneErr then [head xs] else xs) $ \er -> do
                                        putStrLn (renderGoLite (pretty er))
                                    exitFailure

dumpSymbolTable :: InputFile -> TypecheckState -> IO ()
dumpSymbolTable f s = withFile (show f -<.> "symtab") WriteMode $ \h ->
    let ds = reverse $ dumpedScopes s in
    forM_ ds $ \(ss, depth) ->
        hPutStrLn h (renderGoLite $ nest (depth * 4) (pretty ss))

goto :: Goto -> IO ()
goto g =
    let oneErr = oneError g in
    let dumpSyms = dumpSymTab g in
    case cmd g of
        Pretty f -> do
            (p, s) <- typecheckFile oneErr f
            when (dumpSyms) $ dumpSymbolTable f s
            case V.runSimplify (_nextGid s + 1) (V.simplifyPackage p) of
                Left critical -> print critical *> exitFailure
                Right (strings, prog) -> do
                    when (dumpVigil g) $ do
                        putStrLn $ renderGoLite $ pretty prog
                        exitSuccess
                    case codegen strings prog of
                        Right d ->
                            putStrLn .
                            render $
                            d
                        Left (d, e) -> do
                            putStrLn "error:"
                            print e
                            putStrLn "virtual assembly:"
                            putStrLn (render d)
                            exitFailure

        Cpp f -> do
            (p, s) <- typecheckFile oneErr f
            when (dumpSyms) $ dumpSymbolTable f s
            let d = runCompiler p
            putStrLn $ renderGoLite d

weedGoLiteProgram :: Bool -> SrcAnnPackage -> Maybe G.WeederExceptions
weedGoLiteProgram oneErr p =
    case G.weed p of
        [] -> Nothing
        xs -> Just $ G.WeederExceptions (if oneErr then [head xs] else xs)

parseGoLiteFile :: InputFile -> IO (Either G.ParseError SrcAnnPackage)
parseGoLiteFile f = do
    file <- case f of
        Stdin -> getContents
        FilePath f' -> readFile f'
    pure $ parseOnly G.packageP (show f) file

parseOnly :: G.Parser a -> String -> String -> Either G.ParseError a
parseOnly m = G.parse (G.sc >> m <* G.lexeme G.eof)
