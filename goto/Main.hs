{-# LANGUAGE ViewPatterns #-}

module Main where

import qualified Language.GoLite as G
import Language.GoLite.Pretty
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Typecheck

import Control.Monad ( forM_ )
import Options.Applicative
import System.Exit ( exitFailure )

import System.IO ( hPutStrLn, stderr )

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
    = Pretty
        { filename :: InputFile
        }
    | RoundTrip
        { filename :: InputFile
        }
    | PrettyType
        { filename :: InputFile
        }
    deriving (Eq, Ord, Read, Show)

main :: IO ()
main = execParser cmdParser >>= goto

cmdParser :: ParserInfo Goto
cmdParser
    = info (
        subparser (
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
            command "round-trip" (
                info (
                    RoundTrip <$> fmap parseInputFile (
                        strArgument (
                            metavar "[FILE]" <>
                            value "-"
                        )
                    )
                ) $
                briefDesc <>
                progDesc "Checks the pretty-print invariant."
            ) <>
            command "pretty-type" (
                info (
                    PrettyType <$> fmap parseInputFile (
                        strArgument (
                            metavar "[FILE]" <>
                            value "-"
                        )
                    )
                ) $
                briefDesc <>
                progDesc "Typechecks and pretty-prints with type annotations."
            )
        )
    ) $
    progDesc "Compiler for GoLite"

noNewLines :: String -> String
noNewLines = foldr (\a b -> (if a == '\n' then ' ' else a) : b) []

goto :: Goto -> IO ()
goto g = case g of
    Pretty f -> do
        ex <- parseGoLiteFile f
        case ex of
            Left e -> hPutStrLn stderr $ noNewLines $ show e
            Right r -> case weedGoLiteProgram r of
                Just es -> hPutStrLn stderr $ renderGoLite (pretty es)
                Nothing -> putStrLn $ renderGoLite (pretty r)
    PrettyType f -> do
        ex <- parseGoLiteFile f
        case ex of
            Left e -> hPutStrLn stderr $ noNewLines $ show e
            Right r -> case weedGoLiteProgram r of
                Just es -> hPutStrLn stderr $ renderGoLite (pretty es)
                Nothing -> case runTypecheck (G.typecheckPackage r) of
                    (Left fatal, _) -> print fatal
                    (Right _, _errors -> []) -> putStrLn "success"
                    (Right _, _errors -> xs) -> do
                        forM_ xs $ \er -> do
                            putStrLn (renderGoLite (pretty er))
    RoundTrip f -> do
        ex <- parseGoLiteFile f
        case ex of
            Left e -> do
                putStrLn $ "failed to parse input program"
                putStrLn $ noNewLines $ show e
                exitFailure
            Right r -> do
                case weedGoLiteProgram r of
                    Just es -> hPutStrLn stderr $ renderGoLite (pretty es)
                    Nothing -> do
                        let s = renderGoLite (pretty r)
                        case parseOnly G.packageP "<pretty>" s of
                            Left e -> do
                                putStrLn $ "failed to parse pretty-printed program"
                                putStrLn $ noNewLines $ show e
                                putStrLn $ s
                                exitFailure
                            Right r' -> case weedGoLiteProgram r of
                                Just es -> do
                                    putStrLn $ "failed to weed pretty-printed program"
                                    putStrLn $ renderGoLite (pretty es)
                                    putStrLn $ s
                                    exitFailure
                                Nothing -> do
                                    let s' = renderGoLite (pretty r')
                                    case s == s' of
                                        True -> putStrLn "OK"
                                        False -> do
                                            putStrLn "Round-trip failed.\n"
                                            putStrLn "First pretty-print:"
                                            putStrLn s
                                            putStrLn "\nSecond pretty-print:"
                                            putStrLn s'
                                            exitFailure

weedGoLiteProgram :: SrcAnnPackage -> Maybe G.WeederExceptions
weedGoLiteProgram p =
    case G.weed p of
        [] -> Nothing
        xs -> Just $ G.WeederExceptions xs

parseGoLiteFile :: InputFile -> IO (Either G.ParseError SrcAnnPackage)
parseGoLiteFile f = do
    file <- case f of
        Stdin -> getContents
        FilePath f' -> readFile f'
    pure $ parseOnly G.packageP (show f) file

parseOnly :: G.Parser a -> String -> String -> Either G.ParseError a
parseOnly m = G.parse (G.sc >> m <* G.lexeme G.eof)
