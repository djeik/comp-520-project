module Language.GoLite.Parser.Expression
( expr
) where

import Language.GoLite.Syntax
import Language.GoLite.Lexer

import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Expr

-- | Parses an expression.
expr :: Parser (Semi Expr)
expr = makeExprParser term table

-- | Parses a basic term of an expression, sufficiently wrapped so as to act as
-- an "Expr" in its own right.
term :: Parser (Semi Expr)
term = operand <|> conversion

-- | The operator precedence table for expressions.
table :: [[Operator Parser (Semi Expr)]]
table =
    [ [ postfix (selector <|> index <|> sliceE <|> typeAssertion <|> call)
      ]
    , [ prefix "+" (UnaryOp Positive)
      , prefix "-" (UnaryOp Negative)
      , prefix "!" (UnaryOp LogicalNot)
      , prefix "^" (UnaryOp BitwiseNot)
      , prefix "*" (UnaryOp Dereference)
      , prefix "&" (UnaryOp Reference)
      , prefix "<-" (UnaryOp Receive)
      ]
    , [ binary "*" (BinaryOp Times)
      , binary "/" (BinaryOp Divide)
      , binary "%" (BinaryOp Modulo)
      , binary "<<" (BinaryOp ShiftLeft)
      , binary ">>" (BinaryOp ShiftRight)
      , binary "&" (BinaryOp BitwiseAnd)
      , binary "&^" (BinaryOp BitwiseAndNot)
      ]
    , [ binary "+" (BinaryOp Plus)
      , binary "-" (BinaryOp Minus)
      , binary "|" (BinaryOp BitwiseOr)
      , binary "^" (BinaryOp BitwiseXor)
      ]
    , [ binary "==" (BinaryOp Equal)
      , binary "!=" (BinaryOp NotEqual)
      , binary "<" (BinaryOp LessThan)
      , binary "<=" (BinaryOp LessThanEqual)
      , binary ">" (BinaryOp GreaterThan)
      , binary ">=" (BinaryOp GreaterThanEqual)
      ]
    , [ binary "&&" (BinaryOp LogicalAnd)
      ]
    , [ binary "||" (BinaryOp LogicalOr)
      ]
    ] where
        binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser (Semi Expr)
        binary name f
            = InfixL $ do
                symbol_ name
                pure $ \e1 e2 -> do
                    x <- e1
                    noSemi
                    y <- e2
                    pure $ f x y

        prefix :: String -> (Expr -> Expr) -> Operator Parser (Semi Expr)
        prefix name f
            = Prefix $ do
                symbol_ name
                pure $ \e -> f <$> e

        postfix :: Parser (Semi Expr -> Semi Expr)
                -> Operator Parser (Semi Expr)
        postfix p
            = Postfix $ do
                qs <- some p :: Parser [Semi Expr -> Semi Expr]
                pure $ foldr1 (\a b -> \c -> do
                    b' <- b c :: Semi Expr
                    noSemi
                    a (pure b')) qs

-- | Parses an operand, sufficiently wrapped so as to act as an expression in
-- its own right.
operand :: Parser (Semi Expr)
operand
    = (fmap Literal <$> lexeme literal)
    <|> (fmap Variable <$> lexeme identifier)
    <|> parens (expr >>= noSemiP)

-- | Parses a cast expression.
--
-- This parser can be backtracked from until it parses a "type_" followed by an
-- "expr".
conversion :: Parser (Semi Expr)
conversion = do
    t <- try $ do
        t <- type_ >>= noSemiP
        symbol "("
        return t
    e <- expr >>= noSemiP
    s <- closeParen
    pure $ do
        s
        pure $ Conversion t e

-- | Parses a primary expression in the form of a "Selector".
--
-- This parser can be backtracked from until it parses a dot \".\".
selector :: Parser (Semi Expr -> Semi Expr)
selector = do
    try $ symbol "."
    ident <- identifier
    pure $ \e -> do
        x <- e
        noSemi
        i <- ident
        pure $ Selector x i

-- | Parses a primary expression in the form of an "Index".
--
-- This parser can be backtracked from until the closing square bracket is
-- parsed.
index :: Parser (Semi Expr -> Semi Expr)
index = do
    e' <- try $ squareBrackets (expr >>= noSemiP)
    pure $ \e -> do
        x <- e
        noSemi
        y <- e'
        pure $ Index x y

sliceE :: Parser (Semi Expr -> Semi Expr)
sliceE = do
    s <- sliceBody
    pure $ \e -> do
        y <- e
        noSemi
        x <- s
        pure $ Slice y x

-- | Parses the body of a slice operator, specifically the part between the
-- square brackets.
--
-- The reason this function exists separately from "sliceE" is that there are
-- two alternatives for "Slice"s, namely "SliceFromTo" and "SliceFromToStep",
-- both of which can be produced by this parser.
sliceBody :: Parser (Semi Slice)
sliceBody = try $ squareBrackets (fromToStep <|> fromTo) where
    fromTo = do
        e1 <- try $ do
            e1 <- optional (expr >>= noSemiP)
            symbol ":"
            return e1
        e2 <- optional (expr >>= noSemiP)
        return (SliceFromTo e1 e2)
    fromToStep = do
        (e1, e2) <- try $ do
            e1 <- optional (expr >>= noSemiP)
            symbol ":"
            e2 <- expr >>= noSemiP
            symbol ":"
            return (e1, e2)

        e3 <- expr >>= noSemiP
        return (SliceFromToStep e1 e2 e3)

-- | Parses a type assertion.
--
-- This function can be backtracked from until both a dot (\".\") and an
-- opening parenthesis (\"(\") are parsed.
typeAssertion :: Parser (Semi Expr -> Semi Expr)
typeAssertion = do
    try $ do
        symbol "."
        symbol "("
    t <- type_
    _ <- closeParen
    pure $ \e -> do
        y <- e
        noSemi 
        x <- t
        pure $ TypeAssertion y x

-- | Parses a function call argument list.
arguments :: Parser Arguments
arguments = normalArgs <|> typeArgs where
    normalArgs = NormalArguments <$> exprs
    typeArgs = TypeArguments <$> (type_ >>= noSemiP) <*> exprs
    exprs = (expr >>= noSemiP) `sepBy` symbol ","

-- | Parses the function call postfix operator.
--
-- This parser can be backtracked from until the opening parenthesis (\"(\") of
-- the argument list is parsed.
call :: Parser (Semi Expr -> Semi Expr)
call = do
    try $ symbol "("
    args <- arguments
    _ <- closeParen
    pure $ \e -> do
        y <- e
        noSemi
        pure $ Call y args
