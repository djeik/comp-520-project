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
kwPrint = semisym "print"

-- | Parses the \"println\" keyword, checking for a semicolon.
kwPrintLn :: Parser (Semi Keyword)
kwPrintLn = semisym "println"

-- | Parses the \"read\" keyword, checking for a semicolon.
kwRead :: Parser (Semi Keyword)
kwRead = semisym "read"

-- | Parses the \"var\" keyword, checking for a semicolon.
kwVar :: Parser (Semi Keyword)
kwVar = semisym "var"
