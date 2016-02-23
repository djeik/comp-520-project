{-|
Module      : Language.GoLite.Parser.Expression
Description : GoLite expression parser
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

module Language.GoLite.Parser.Expression
( expr
) where

import Language.GoLite.Lexer
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Syntax.Types

import Control.Monad (void)

import Text.Megaparsec.Expr

-- | Parses an expression.
expr :: Parser (Semi SrcAnnExpr)
expr = makeExprParser term table

-- | Parses a basic term of an expression.
term :: Parser (Semi SrcAnnExpr)
term = operand <|> conversion

-- | The operator precedence table for expressions.
table :: [[Operator Parser (Semi SrcAnnExpr)]]
table =
    [ [ postfix (selector <|> index <|> sliceE <|> typeAssertion <|> call)
      ]
    , [ prefix "+"  (\f -> UnaryOp (f Positive))
      , prefix "-"  (\f -> UnaryOp (f Negative))
      , prefix "!"  (\f -> UnaryOp (f LogicalNot))
      , prefix "^"  (\f -> UnaryOp (f BitwiseNot))
      , prefix "*"  (\f -> UnaryOp (f Dereference))
      , prefix "&"  (\f -> UnaryOp (f Reference))
      , prefix "<-" (\f -> UnaryOp (f Receive))
      ]
    , [ binary "*"  (\f -> BinaryOp (f Times))
      , binary "/"  (\f -> BinaryOp (f Divide))
      , binary "%"  (\f -> BinaryOp (f Modulo))
      , binary "<<" (\f -> BinaryOp (f ShiftLeft))
      , binary ">>" (\f -> BinaryOp (f ShiftRight))
      , binary "&"  (\f -> BinaryOp (f BitwiseAnd))
      , binary "&^" (\f -> BinaryOp (f BitwiseAndNot))
      ]
    , [ binary "+"  (\f -> BinaryOp (f Plus))
      , binary "-"  (\f -> BinaryOp (f Minus))
      , binary "|"  (\f -> BinaryOp (f BitwiseOr))
      , binary "^"  (\f -> BinaryOp (f BitwiseXor))
      ]
    , [ binary "==" (\f -> BinaryOp (f Equal))
      , binary "!=" (\f -> BinaryOp (f NotEqual))
      , binary "<"  (\f -> BinaryOp (f LessThan))
      , binary "<=" (\f -> BinaryOp (f LessThanEqual))
      , binary ">"  (\f -> BinaryOp (f GreaterThan))
      , binary ">=" (\f -> BinaryOp (f GreaterThanEqual))
      ]
    , [ binary "&&" (\f -> BinaryOp (f LogicalAnd))
      ]
    , [ binary "||" (\f -> BinaryOp (f LogicalOr))
      ]
    ] where
        binary
            :: String
            -> (   (BinaryOp () -> SrcAnnBinaryOp)
                -> SrcAnnExpr
                -> SrcAnnExpr
                -> SrcAnnExprF SrcAnnExpr)
            -> Operator Parser (Semi SrcAnnExpr)
        binary name f
            = InfixL $ do
                (Ann a _) <- try $ withSrcAnnConst $
                    symbol_ name <* notFollowedBy (incDecAssignEnd name)
                pure $ \e1 e2 -> do
                    x <- e1
                    noSemi
                    y <- e2
                    let (s1, s2) = (topAnn x, topAnn y)
                    pure $ Fix $ Ann (SrcSpan (srcStart s1) (srcEnd s2)) $
                        f (Ann a) x y

        prefix
            :: String
            -> (   (UnaryOp () -> SrcAnnUnaryOp)
                -> SrcAnnExpr
                -> SrcAnnExprF SrcAnnExpr)
            -> Operator Parser (Semi SrcAnnExpr)
        prefix name f
            = Prefix $ do
                (Ann a _) <- try $ withSrcAnnConst $ symbol_ name
                pure $ \e -> do
                    x <- e
                    let s = topAnn x
                    pure $ Fix $ Ann (SrcSpan (srcStart a) (srcEnd s)) $
                        f (Ann a) x

        postfix
            :: Parser (Semi SrcAnnExpr -> Semi SrcAnnExpr)
            -> Operator Parser (Semi SrcAnnExpr)
        postfix p
            = Postfix $ do
                qs <- some p :: Parser [Semi SrcAnnExpr -> Semi SrcAnnExpr]
                pure $ foldr1 (\a b -> \c -> do
                    a' <- a c :: Semi SrcAnnExpr
                    noSemi
                    b (pure a')) qs

        -- Succeeds if the next character is part of a longer operator that
        -- isn't expected in the context of an expression (an assignment or
        -- increment/decrement operator).
        incDecAssignEnd :: String -> Parser ()
        incDecAssignEnd name = void (char '=' <|> incDecEnd) where
            incDecEnd = case name of
                "+" -> char '+'
                "-" -> char '-'
                -- If not a plus or minus, we don't care
                -- what the next character is.
                _   -> failure []


-- | Parses an operand, sufficiently wrapped so as to act as an expression in
-- its own right.
operand :: Parser (Semi SrcAnnExpr)
operand
    = (fmap (dup Literal) <$> lexeme literal)
    <|> (fmap (dup Variable) <$> lexeme identifier)
    <|> parens (expr >>= noSemiP) where
        dup f x@(Ann a _) = Fix $ Ann a (f x)

-- | Parses a cast expression.
--
-- This parser can be backtracked from until it parses a "type_" followed by an
-- "expr".
conversion :: Parser (Semi SrcAnnExpr)
conversion = do
    t <- try $ do
        t <- type_ >>= noSemiP
        symbol "("
        return t
    e <- expr >>= noSemiP
    s <- closeParen

    let a = SrcSpan (srcStart (topAnn t)) (srcEnd (topAnn e))

    pure $ do
        s
        pure $ Fix $ Ann a $ Conversion t e

-- | Parses a primary expression in the form of a "Selector".
--
-- This parser can be backtracked from until it parses a dot \".\" followed by
-- an identifier.
selector :: Parser (Semi SrcAnnExpr -> Semi SrcAnnExpr)
selector = do
    ident <- try $ do
        symbol "."
        identifier
    pure $ \e -> do
        x <- e
        noSemi
        i <- ident

        -- the source span of the selector expression starts before the
        -- expression to select in and ends after the identifier used to do the
        -- selection.
        let a = SrcSpan (srcStart (topAnn x)) (srcEnd (ann i))

        pure $ Fix $ Ann a $ Selector x i

-- | Parses a primary expression in the form of an "Index".
--
-- This parser can be backtracked from until the closing square bracket is
-- parsed.
index :: Parser (Semi SrcAnnExpr -> Semi SrcAnnExpr)
index = do
    (Ann b e') <- try $ withSrcAnnF $ squareBrackets (expr >>= noSemiP)
    pure $ \e -> do
        x <- e
        noSemi
        y <- e'

        -- the source span of the index expression starts before the expression
        -- to index in and ends after the square bracket
        let a = SrcSpan (srcStart (topAnn x)) (srcEnd b)

        pure $ Fix $ Ann a $ Index x y

sliceE :: Parser (Semi SrcAnnExpr -> Semi SrcAnnExpr)
sliceE = do
    (Ann b s) <- withSrcAnnF sliceBody

    pure $ \e -> do
        y <- e
        noSemi
        (e1, e2, e3) <- s

        let a = SrcSpan (srcStart (topAnn y)) (srcEnd b)

        pure $ Fix $ Ann a $ Slice y e1 e2 e3

-- | Parses the body of a slice operator, specifically the part between the
-- square brackets.
--
-- The reason this function exists separately from "sliceE" is that there are
-- two alternatives for "Slice"s, namely "SliceFromTo" and "SliceFromToStep",
-- both of which can be produced by this parser.
sliceBody :: Parser (Semi (Maybe SrcAnnExpr, Maybe SrcAnnExpr, Maybe SrcAnnExpr))
sliceBody = try $ squareBrackets (fromToStep <|> fromTo) where
    fromTo :: Parser (Maybe SrcAnnExpr, Maybe SrcAnnExpr, Maybe SrcAnnExpr)
    fromTo = do
        e1 <- try $ do
            e1 <- optional (expr >>= noSemiP)
            symbol ":"
            return e1
        e2 <- optional (expr >>= noSemiP)
        return (e1, e2, Nothing)

    fromToStep :: Parser (Maybe SrcAnnExpr, Maybe SrcAnnExpr, Maybe SrcAnnExpr)
    fromToStep = do
        (e1, e2) <- try $ do
            e1 <- optional (expr >>= noSemiP)
            symbol ":"
            e2 <- expr >>= noSemiP
            symbol ":"
            return (e1, e2)

        e3 <- expr >>= noSemiP
        return (e1, Just e2, Just e3)

-- | Parses a type assertion.
--
-- This function can be backtracked from until both a dot (\".\") and an
-- opening parenthesis (\"(\") are parsed.
typeAssertion :: Parser (Semi SrcAnnExpr -> Semi SrcAnnExpr)
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

        let a = SrcSpan (srcStart (topAnn y)) (srcEnd (topAnn x))

        pure $ Fix $ Ann a $ TypeAssertion y x

-- | Parses a function call argument list.
arguments :: Parser (Maybe SrcAnnType, [SrcAnnExpr])
arguments = noType <|> withType where
    noType = do
        exprs <- (expr >>= noSemiP) `sepBy` symbol ","
        pure (Nothing, exprs)
    withType = do
        ty <- type_ >>= noSemiP
        symbol ","
        exprs <- (expr >>= noSemiP) `sepBy` symbol ","
        pure (Just ty, exprs)


-- | Parses the function call postfix operator.
--
-- This parser can be backtracked from until the opening parenthesis (\"(\") of
-- the argument list is parsed.
call :: Parser (Semi SrcAnnExpr -> Semi SrcAnnExpr)
call = do
    try $ symbol "("
    (ty, exprs) <- arguments
    (Ann b _) <- withSrcAnnF closeParen

    pure $ \e -> do
        y <- e
        noSemi

        let a = SrcSpan (srcStart (topAnn y)) (srcEnd b)

        pure $ Fix $ Ann a $ Call y ty exprs
