module Language.GoLite.Parser.Expression where

import Language.GoLite.Syntax
import Language.GoLite.Lexer

import Control.Monad ( mzero )
import Data.String ( IsString, fromString )
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Expr

expr :: Parser Expr
expr = makeExprParser term table

table =
    [ [ Postfix (fmap (primaryExpr .) selector)
      , Postfix (fmap (primaryExpr .) index)
      , Postfix (fmap (primaryExpr .) sliceE)
      , Postfix (fmap (primaryExpr .) typeAssertion)
      , Postfix (fmap (primaryExpr .) call)
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
        primaryExpr 
            = UnaryExpr
            . PrimaryExpr

        binary name f
            = InfixL (symbol name >> return f)

        prefix name f
            = Prefix (symbol name >> return f)

        unary op 
            = UnaryExpr
            . UnaryOp op
            . PrimaryExpr
            . Operand
            . ExprOp

identifier :: IsString a => Parser a
identifier = do
    c <- letterChar
    cs <- many alphaNumChar
    return $ fromString (c:cs)

type_ :: Parser Type
type_ = sliceType <|> arrayType <|> namedType where
    sliceType
        = SliceType
        <$> (symbol "[" *> symbol "]" *> type_)
    arrayType
        = ArrayType
        <$> (symbol "[" *> lexeme int_lit <* symbol "]")
        <*> type_
    namedType
        = NamedType <$> lexeme identifier

selector = do
    char '.'
    id <- identifier
    return $ \e -> Selector e id

index = do
    char '['
    e' <- expr
    char ']'
    return $ \e -> Index e e'

sliceE = do
    s <- slice
    return $ \e -> Slice e s

slice = symbol "[" *> (s1 <|> s2) <* symbol "]" where
    s1 = do
        e1 <- optional expr
        symbol ":"
        e2 <- optional expr
        return (SliceFromTo e1 e2)
    s2 = do
        e1 <- optional expr
        symbol ":"
        e2 <- expr
        symbol ":"
        e3 <- expr
        return (SliceFromToStep e1 e2 e3)

typeAssertion = do
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
    symbol "("
    args <- arguments
    symbol ")"
    return $ \e -> Call e args

term = operand <|> (UnaryExpr . PrimaryExpr <$> conversion)

operand = fmap wrap literal <|> fmap wrap operandName <|> parens expr
    where wrap = UnaryExpr . PrimaryExpr . Operand

parens = between (symbol "(") (symbol ")")

operandName = OperandNameOp <$> identifier

conversion = do
    t <- type_
    symbol "("
    e <- expr
    symbol ")"
    return (Conversion t e)

literal = fmap LiteralOp lit where
    lit = choice
        [ fmap IntLit int_lit
        , fmap FloatLit float_lit
        , fmap RuneLit rune_lit
        , fmap StringLit string_lit
        ]
