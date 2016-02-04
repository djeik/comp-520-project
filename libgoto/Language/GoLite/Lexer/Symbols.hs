module Language.GoLite.Lexer.Symbols
( -- * Simple symbols
  comma
, colon
, semicolon
, opIncrement
, opDecrement
-- * Structure symbols
, parens
, squareBrackets
, braces
, closeParen
, closeBracket
, closeBrace
-- * Statement symbols
, shortVarDeclarator
  -- * Assignment operators
, opAssign
, opAssignSimple
, opPlusEq
, opMinusEq
, opBitwiseOrEq
, opBitwiseXorEq
, opTimesEq
, opDivideEq
, opModuloEq
, opShiftLeftEq
, opShiftRightEq
, opBitwiseAndEq
, opBitwiseAndNotEq
) where

import Language.GoLite.Lexer.Core
import Language.GoLite.Lexer.Semi
import Language.GoLite.Syntax

-- | Parses a comma symbol \",\".
comma :: Parser ()
comma = symbol_ ","

-- | Parses a colon symbol \":\".
colon :: Parser ()
colon = symbol_ ":"

-- | Parses the increment symbol \"++\", checking for a semicolon.
opIncrement :: Parser (Semi String)
opIncrement = semisym "++"

-- | Parses the decrement symbol \"--\", checking for a semicolon.
opDecrement :: Parser (Semi String)
opDecrement = semisym "--"

-- | Parses a closing parenthesis \")\", checking for a semicolon.
closeParen :: Parser (Semi String)
closeParen = semisym ")"

-- | Parses a closing bracket \"]\", checking for a semicolon.
closeBracket :: Parser (Semi String)
closeBracket = semisym "]"

-- | Parses a closing brace \"}\", checking for a semicolon.
closeBrace :: Parser (Semi String)
closeBrace = semisym "}"

-- | Parses a short variable declarator \":=\", checking for a semicolon.
shortVarDeclarator :: Parser (Semi String)
shortVarDeclarator = semisym ":="

-- | Creates a parser that will consume the given string and return the
--   appropriate assignment operator, checking for a semicolon.
semiAssignOp :: String -> AssignOp -> Parser (Semi AssignOp)
semiAssignOp s op = do
                    x <- semisym s
                    pure (x $> op)

-- | Parses an assignment operator \"=\", checking for a semicolon.
opAssignSimple :: Parser (Semi AssignOp)
opAssignSimple = semiAssignOp "=" Assign

-- | Parses a plus-equal assignment operator \"+=\", checking for a semicolon.
opPlusEq :: Parser (Semi AssignOp)
opPlusEq = semiAssignOp "+=" PlusEq

-- | Parses a minus-equal assignment operator \"-=\", checking for a semicolon.
opMinusEq :: Parser (Semi AssignOp)
opMinusEq = semiAssignOp "-=" MinusEq

-- | Parses a bitwise-or-equal assignment operator \"|=\", checking for a
-- semicolon.
opBitwiseOrEq :: Parser (Semi AssignOp)
opBitwiseOrEq = semiAssignOp "|=" BitwiseOrEq

-- | Parses a bitwise-xor-equal assignment operator \"^=\", checking for a
-- semicolon.
opBitwiseXorEq :: Parser (Semi AssignOp)
opBitwiseXorEq = semiAssignOp "^=" BitwiseXorEq

-- | Parses a times-equal assignment operator \"*=\", checking for a semicolon.
opTimesEq :: Parser (Semi AssignOp)
opTimesEq = semiAssignOp "*=" TimesEq

-- | Parses a divide-equal assignment operator \"/=\", checking for a semicolon.
opDivideEq :: Parser (Semi AssignOp)
opDivideEq = semiAssignOp "/=" DivideEq

-- | Parses a modulo-equal assignment operator \"%=\", checking for a semicolon.
opModuloEq :: Parser (Semi AssignOp)
opModuloEq = semiAssignOp "%=" ModuloEq

-- | Parses a left-shift-equal assignment operator \"<<=\", checking for a
-- semicolon.
opShiftLeftEq :: Parser (Semi AssignOp)
opShiftLeftEq = semiAssignOp "<<=" ShiftLeftEq

-- | Parses a right-shift-equal assignment operator \">>=\", checking for a
-- semicolon.
opShiftRightEq :: Parser (Semi AssignOp)
opShiftRightEq = semiAssignOp ">>=" ShiftRightEq

-- | Parses a bitwise-and-equal assignment operator \"&=\", checking for a
-- semicolon.
opBitwiseAndEq :: Parser (Semi AssignOp)
opBitwiseAndEq = semiAssignOp "&=" BitwiseAndEq

-- | Parses a bitwise-and-not-equal assignment operator \"&^=\", checking for a
-- semicolon.
opBitwiseAndNotEq :: Parser (Semi AssignOp)
opBitwiseAndNotEq = semiAssignOp "&^=" BitwiseAndNotEq

-- | Parses an assignment operator
opAssign :: Parser (Semi AssignOp)
opAssign = choice
    [ opAssignSimple
    , opPlusEq
    , opMinusEq
    , opBitwiseOrEq
    , opBitwiseXorEq
    , opTimesEq
    , opDivideEq
    , opModuloEq
    , opShiftLeftEq
    , opShiftRightEq
    , opBitwiseAndNotEq
    , opBitwiseAndEq
    ]

-- | Runs a given parser requiring that it be surrounded by matched parentheses
-- "symbol_"s.
parens :: Parser a -> Parser (Semi a)
parens p = do
    symbol_ "("
    q <- p
    s <- closeParen
    pure (s $> q)

-- | Runs a given parses requiring that it be surrounded by matched square
-- bracket "symbol_"s.
squareBrackets :: Parser a -> Parser (Semi a)
squareBrackets p = do
    symbol_ "["
    q <- p
    s <- closeBracket
    pure (s $> q)

-- | Runs a given parses requiring that it be surrounded by matched brace
-- "symbol_"s.
braces :: Parser a -> Parser (Semi a)
braces p = do
    symbol_ "{"
    q <- p
    s <- closeBrace
    pure (s $> q)
