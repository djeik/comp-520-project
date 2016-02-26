{-|
Module      : Language.GoLite.Lexer.Keywords
Description : Lexers for GoLite keywords
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

module Language.GoLite.Lexer.Keywords
( -- * Keywords and symbols
  Keyword
, kwBreak
, kwReturn
, kwContinue
, kwFallthrough
, kwPrint
, kwPrintLn
, kwRead
, kwVar
, kwStruct
, kwType
, kwIf
, kwElse
, kwFor
, kwSwitch
, kwCase
, kwDefault
, kwPackage
, kwFunc
, anyKeyword
) where

import Language.GoLite.Lexer.Core
import Language.GoLite.Lexer.Semi

import Control.Monad ( void )

-- | Keywords are simply represented as "String"s.
type Keyword = String

-- | Parses any keyword. Used when dealing with identifiers.
anyKeyword :: Parser ()
anyKeyword = choice (map (void . try)
                            [  kwBreak
                            , kwReturn
                            , kwContinue
                            , kwFallthrough
                            , kwPrint
                            , kwPrintLn
                            , kwRead
                            , kwVar
                            , kwStruct
                            , kwType
                            , kwElse
                            , kwCase
                            , kwDefault
                            , kwPackage
                            , kwFunc ]
                ++ map (void . try) [kwIf, kwFor, kwSwitch])

onlyKeyword :: Keyword -> Parser (HasNewline, Keyword)
onlyKeyword s = lexeme $ string s <* notFollowedBy alphaNumChar

onlyKeywordSemi :: Keyword -> Parser (Semi Keyword)
onlyKeywordSemi = withDetectSemicolon . onlyKeyword

onlyKeywordSemiEx :: Keyword -> Parser (Semi Keyword)
onlyKeywordSemiEx = withDetectExplicitSemicolon . onlyKeyword

-- | Parses the \"break\" keyword, checking for a semicolon.
kwBreak :: Parser (Semi Keyword)
kwBreak = onlyKeywordSemi "break"

-- | Parses the \"return\" keyword, checking for a semicolon.
kwReturn :: Parser (Semi Keyword)
kwReturn = onlyKeywordSemi "return"

-- | Parses the \"continue\" keyword, checking for a semicolon.
kwContinue :: Parser (Semi Keyword)
kwContinue = onlyKeywordSemi "continue"

-- | Parses the \"fallthrough\" keyword, checking for a semicolon.
kwFallthrough :: Parser (Semi Keyword)
kwFallthrough = onlyKeywordSemi "fallthrough"

-- | Parses the \"print\" keyword, checking for an explicit semicolon.
kwPrint :: Parser (Semi Keyword)
kwPrint = onlyKeywordSemiEx "print"

-- | Parses the \"println\" keyword, checking for an explicit semicolon.
kwPrintLn :: Parser (Semi Keyword)
kwPrintLn = onlyKeywordSemiEx "println"

-- | Parses the \"read\" keyword, checking for an explicit semicolon.
kwRead :: Parser (Semi Keyword)
kwRead = onlyKeywordSemiEx "read"

-- | Parses the \"var\" keyword, checking for an explicit semicolon.
kwVar :: Parser (Semi Keyword)
kwVar = onlyKeywordSemiEx "var"

-- | Parses the \"struct\" keyword, checking for an explicit semicolon.
kwStruct :: Parser (Semi Keyword)
kwStruct = onlyKeywordSemiEx "struct"

-- | Parses the \"type\" keyword, checking for an explicit semicolon.
kwType :: Parser (Semi Keyword)
kwType = onlyKeywordSemiEx "type"

-- | Parses the \"if\" keyword. Does not check for a semicolon, since an if
-- statement may contain an optional initializer.
kwIf :: Parser Keyword
kwIf = snd <$> onlyKeyword "if"

-- | Parses the \"else\" keyword, checking for an explicit semicolon.
kwElse :: Parser (Semi Keyword)
kwElse = onlyKeywordSemiEx "else"

-- | Parses the \"for\" keyword. Does not check for a semicolon, since a for
-- statement may contain an optional initializer.
kwFor :: Parser Keyword
kwFor = snd <$> onlyKeyword "for"

-- | Parses the \"switch\" keyword. Does not check for a semicolon, since a
-- switch statement may contain an optional initializer.
kwSwitch :: Parser Keyword
kwSwitch = snd <$> onlyKeyword "switch"

-- | Parses the \"case\" keyword, checking for a semicolon.
kwCase :: Parser (Semi Keyword)
kwCase = onlyKeywordSemiEx "case"

-- | Parses the \"default\" keyword, checking for a semicolon.
kwDefault :: Parser (Semi Keyword)
kwDefault = onlyKeywordSemiEx "default"

-- | Parses the \"package\" keyword, checking for a semicolon.
kwPackage :: Parser (Semi Keyword)
kwPackage = onlyKeywordSemiEx "package"

-- | Parses the \"func\" keyword, checking for a semicolon.
kwFunc :: Parser (Semi Keyword)
kwFunc = onlyKeywordSemiEx "func"
