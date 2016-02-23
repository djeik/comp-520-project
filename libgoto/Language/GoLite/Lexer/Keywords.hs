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
) where

import Language.GoLite.Lexer.Core
import Language.GoLite.Lexer.Semi

-- | Keywords are simply represented as "String"s.
type Keyword = String

-- | Parses the \"break\" keyword, checking for a semicolon.
kwBreak :: Parser (Semi Keyword)
kwBreak = semisym "break"

-- | Parses the \"return\" keyword, checking for a semicolon.
kwReturn :: Parser (Semi Keyword)
kwReturn = semisym "return"

-- | Parses the \"continue\" keyword, checking for a semicolon.
kwContinue :: Parser (Semi Keyword)
kwContinue = semisym "continue"

-- | Parses the \"fallthrough\" keyword, checking for a semicolon.
kwFallthrough :: Parser (Semi Keyword)
kwFallthrough = semisym "fallthrough"

-- | Parses the \"print\" keyword, checking for a semicolon.
kwPrint :: Parser (Semi Keyword)
kwPrint = explicitSemisym "print"

-- | Parses the \"println\" keyword, checking for a semicolon.
kwPrintLn :: Parser (Semi Keyword)
kwPrintLn = explicitSemisym "println"

-- | Parses the \"read\" keyword, checking for a semicolon.
kwRead :: Parser (Semi Keyword)
kwRead = explicitSemisym "read"

-- | Parses the \"var\" keyword, checking for a semicolon.
kwVar :: Parser (Semi Keyword)
kwVar = explicitSemisym "var"

-- | Parses the \"struct\" keyword, checking for a semicolon.
kwStruct :: Parser (Semi Keyword)
kwStruct = explicitSemisym "struct"

-- | Parses the \"type\" keyword, checking for a semicolon.
kwType :: Parser (Semi Keyword)
kwType = explicitSemisym "type"

-- | Parses the \"if\" keyword, checking for a semicolon.
kwIf :: Parser (Semi Keyword)
kwIf = explicitSemisym "if"

-- | Parses the \"else\" keyword, checking for a semicolon.
kwElse :: Parser (Semi Keyword)
kwElse = explicitSemisym "else"

-- | Parses the \"for\" keyword, checking for a semicolon.
kwFor :: Parser (Semi Keyword)
kwFor = explicitSemisym "for"

-- | Parses the \"switch\" keyword, checking for a semicolon.
kwSwitch :: Parser (Semi Keyword)
kwSwitch = explicitSemisym "switch"

-- | Parses the \"case\" keyword, checking for a semicolon.
kwCase :: Parser (Semi Keyword)
kwCase = explicitSemisym "case"

-- | Parses the \"default\" keyword, checking for a semicolon.
kwDefault :: Parser (Semi Keyword)
kwDefault = explicitSemisym "default"

-- | Parses the \"package\" keyword, checking for a semicolon.
kwPackage :: Parser (Semi Keyword)
kwPackage = explicitSemisym "package"

-- | Parses the \"func\" keyword, checking for a semicolon.
kwFunc :: Parser (Semi Keyword)
kwFunc = explicitSemisym "func"
