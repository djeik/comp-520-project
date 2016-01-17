module Language.GoLite.Parser.Expression
( expr
) where

import Language.GoLite.Syntax
import Language.GoLite.Lexer

import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Expr

-- | Parses an expression.
expr :: Parser Expr
expr = makeExprParser term table

-- | Parses a basic term of an expression, sufficiently wrapped so as to act as
-- an "Expr" in its own right.
term :: Parser Expr
term = operand <|> (UnaryExpr . PrimaryExpr <$> conversion)

-- | The operator precedence table for expressions.
table :: [[Operator Parser Expr]]
table =
    [ [ postfix (selector <|> index <|> sliceE <|> typeAssertion <|> call)
      ]
    , [ prefix "+" (unary Positive)
      , prefix "-" (unary Negative)
      , prefix "!" (unary LogicalNot)
      , prefix "^" (unary BitwiseNot)
      , prefix "*" (unary Dereference)
      , prefix "&" (unary Reference)
      , prefix "<-" (unary Receive)
      ]
    , [ binary "*" (BinaryExpr Times)
      , binary "/" (BinaryExpr Divide)
      , binary "%" (BinaryExpr Modulo)
      , binary "<<" (BinaryExpr ShiftLeft)
      , binary ">>" (BinaryExpr ShiftRight)
      , binary "&" (BinaryExpr BitwiseAnd)
      , binary "&^" (BinaryExpr BitwiseAndNot)
      ]
    , [ binary "+" (BinaryExpr Plus)
      , binary "-" (BinaryExpr Minus)
      , binary "|" (BinaryExpr BitwiseOr)
      , binary "^" (BinaryExpr BitwiseXor)
      ]
    , [ binary "==" (BinaryExpr Equal)
      , binary "!=" (BinaryExpr NotEqual)
      , binary "<" (BinaryExpr LessThan)
      , binary "<=" (BinaryExpr LessThanEqual)
      , binary ">" (BinaryExpr GreaterThan)
      , binary ">=" (BinaryExpr GreaterThanEqual)
      ]
    , [ binary "&&" (BinaryExpr LogicalAnd)
      ]
    , [ binary "||" (BinaryExpr LogicalOr)
      ]
    ] where
        binary name f
            = InfixL (symbol name >> return f)

        prefix name f
            = Prefix (symbol name >> return f)

        unary op e = UnaryExpr . UnaryOp op $ case e of
            UnaryExpr u -> u
            t -> PrimaryExpr . Operand . ExprOp $ t

        postfix
            = Postfix
            . fmap (foldr1 (flip (.)))
            . some
            . fmap (primaryExpr .)

        primaryExpr
            = UnaryExpr
            . PrimaryExpr

-- | Parses an operand, sufficiently wrapped so as to act as an expression in
-- its own right.
operand :: Parser Expr
operand
    = (wrap . LiteralOp <$> lexeme literal)
    <|> fmap wrap operandName
    <|> parens expr where
        wrap = UnaryExpr . PrimaryExpr . Operand

-- | Parses a named operand in the form of an "OperandNameOp".
operandName :: Parser Operand
operandName = OperandNameOp <$> identifier

-- | Parses a cast expression.
--
-- This parser can be backtracked from until it parses a "type_" followed by an
-- "expr".
conversion :: Parser PrimaryExpr
conversion = do
    t <- try $ do
        t <- type_
        symbol "("
        return t
    e <- expr
    symbol ")"
    return (Conversion t e)

-- | Parses a primary expression in the form of a "Selector".
--
-- This parser can be backtracked from until it parses a dot \".\".
selector :: Parser (Expr -> PrimaryExpr)
selector = do
    try $ symbol "."
    ident <- identifier
    return $ \e -> Selector e ident

-- | Parses a primary expression in the form of an "Index".
--
-- This parser can be backtracked from until the closing square bracket is
-- parsed.
index :: Parser (Expr -> PrimaryExpr)
index = do
    e' <- try $ squareBrackets expr
    return $ \e -> Index e e'

sliceE :: Parser (Expr -> PrimaryExpr)
sliceE = do
    s <- sliceBody
    return $ \e -> Slice e s

-- | Parses the body of a slice operator, specifically the part between the
-- square brackets.
--
-- The reason this function exists separately from "sliceE" is that there are
-- two alternatives for "Slice"s, namely "SliceFromTo" and "SliceFromToStep",
-- both of which can be produced by this parser.
sliceBody :: Parser Slice
sliceBody = try $ squareBrackets (fromToStep <|> fromTo) where
    fromTo = do
        e1 <- try $ do
            e1 <- optional expr
            symbol ":"
            return e1
        e2 <- optional expr
        return (SliceFromTo e1 e2)
    fromToStep = do
        (e1, e2) <- try $ do
            e1 <- optional expr
            symbol ":"
            e2 <- expr
            symbol ":"
            return (e1, e2)

        e3 <- expr
        return (SliceFromToStep e1 e2 e3)

-- | Parses a type assertion.
--
-- This function can be backtracked from until both a dot (\".\") and an
-- opening parenthesis (\"(\") are parsed.
typeAssertion :: Parser (Expr -> PrimaryExpr)
typeAssertion = do
    try $ do
        symbol "."
        symbol "("
    t <- type_
    symbol ")"
    return (\e -> TypeAssertion e t)

-- | Parses a function call argument list.
arguments :: Parser Arguments
arguments = normalArgs <|> typeArgs where
    normalArgs = NormalArguments <$> exprs
    typeArgs = TypeArguments <$> type_ <*> exprs
    exprs = expr `sepBy` symbol ","

-- | Parses the function call postfix operator.
--
-- This parser can be backtracked from until the opening parenthesis (\"(\") of
-- the argument list is parsed.
call :: Parser (Expr -> PrimaryExpr)
call = do
    try $ symbol "("
    args <- arguments
    symbol ")"
    return $ \e -> Call e args
