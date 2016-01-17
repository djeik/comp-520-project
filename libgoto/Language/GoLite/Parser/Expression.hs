module Language.GoLite.Parser.Expression
( expr
) where

import Language.GoLite.Syntax
import Language.GoLite.Lexer

import Control.Monad ( mzero )
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Expr

-- | Parses an expression.
expr :: Parser Expr
expr = makeExprParser term table

term = operand <|> (UnaryExpr . PrimaryExpr <$> conversion)

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

operand
    = (wrap . LiteralOp <$> lexeme literal)
    <|> fmap wrap operandName
    <|> parens expr where
        wrap = UnaryExpr . PrimaryExpr . Operand

operandName = OperandNameOp <$> identifier

conversion = do
    t <- try $ do
        t <- type_
        symbol "("
        return t
    e <- expr
    symbol ")"
    return (Conversion t e)

selector = do
    try $ symbol "."
    id <- identifier
    return $ \e -> Selector e id

index = do
    e' <- try $ squareBrackets expr
    return $ \e -> Index e e'

sliceE = do
    s <- sliceBody
    return $ \e -> Slice e s

sliceBody = squareBrackets (fromToStep <|> fromTo) where
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

typeAssertion = do
    try $ do
        symbol "."
        symbol "("
    t <- type_
    symbol ")"
    return (\e -> TypeAssertion e t)

arguments = normalArgs <|> typeArgs where
    normalArgs = NormalArguments <$> exprs
    typeArgs = TypeArguments <$> type_ <*> exprs
    exprs = expr `sepBy` symbol ","

call = do
    try $ symbol "("
    args <- arguments
    symbol ")"
    return $ \e -> Call e args
