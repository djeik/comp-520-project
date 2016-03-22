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
, addressableExpr
) where

import Language.GoLite.Lexer
import Language.GoLite.Syntax.SrcAnn
import Language.GoLite.Syntax.Types

import Data.List ( nub )

import Text.Megaparsec.Expr

-- | Parses an addressable expression
addressableExpr :: Parser (Semi SrcAnnExpr)
addressableExpr = do
    e <- expr
    condUnSemiP e isAddressable "Illegal non-addressable expression"
    pure e

-- | Parses an expression.
expr :: Parser (Semi SrcAnnExpr)
expr = makeExprParser term table

-- | Determines whether an expression is addressable. Variables, dereferences,
-- (known as "pointer indirections" in the spec), selectors on addressable
-- expressions, and indexing operations on addressable expressions are all
-- addressable. No other expression is addressable.
isAddressable :: SrcAnnExpr -> Bool
isAddressable (Fix (Ann _ e)) = case e of
    (Variable _ ) -> True
    (UnaryOp (Ann _ Dereference) _) -> True
    (Selector e' _) -> isAddressable e'
    (Index e' _) -> isAddressable e'
    _ -> False

-- | Parses a basic term of an expression: an operand or conversion, optionally
-- followed by a postfix operator.
term :: Parser (Semi SrcAnnExpr)
term = do
    e <- operand <|> conversion
    withPostfixes (noSemiP e) <|> pure e

-- | Transforms a parser into a new parser that additionally accepts at least
-- one postfix operator.
withPostfixes :: Parser SrcAnnExpr -> Parser (Semi SrcAnnExpr)
withPostfixes e = do
    e' <- e
    e'' <- postfix e'
    withPostfixes (noSemiP e'') <|> pure e''

-- | All the operators that exist in GoLite.
ops :: [String]
ops = nub
    [ "+"
    , "-"
    , "!"
    , "^"
    , "*"
    , "<-"
    , "*"
    , "/"
    , "%"
    , "<<"
    , ">>"
    , "&"
    , "&^"
    , "|"
    , "^"
    , "=="
    , "!="
    , "<"
    , "<="
    , ">"
    , ">="
    , "&&"
    , "||"
    , "++"
    , "--"
    , "="
    , "+="
    , "-="
    , "|="
    , "^="
    , "*="
    , "/="
    , "%="
    , "<<="
    , ">>="
    , "&="
    , "&^="
    ]

-- | All the special characters.
specials :: [Char]
specials = (nub . concat) $ ops

opLexeme :: String -> Parser ()
opLexeme s = lexeme_ $ do
    try (string s)
    notFollowedBy $ do
        cs <- some (oneOf specials)
        if s ++ cs `elem` ops
            then pure ()
            else failure []

-- | The operator precedence table for expressions.
table :: [[Operator Parser (Semi SrcAnnExpr)]]
table =
    [ [ prefix
        [ ("+", \f -> UnaryOp (f Positive))
        , ("-",  \f -> UnaryOp (f Negative))
        , ("!",  \f -> UnaryOp (f LogicalNot))
        , ("^",  \f -> UnaryOp (f BitwiseNot))
        , ("*",  \f -> UnaryOp (f Dereference))
        , ("&",  \f -> UnaryOp (f Reference))
        , ("<-", \f -> UnaryOp (f Receive))
        ]
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
                (Ann a _) <- withSrcAnnConst $ try $ opLexeme name
                pure $ \e1 e2 -> do
                    x <- e1
                    noSemi
                    y <- e2
                    let (s1, s2) = (topAnn x, topAnn y)
                    pure $ Fix $ Ann (SrcSpan (srcStart s1) (srcEnd s2)) $
                        f (Ann a) x y

        prefix
            :: [
                ( String
                , (UnaryOp () -> SrcAnnUnaryOp) -> SrcAnnExpr -> SrcAnnExprF SrcAnnExpr
                )
            ]
            -> Operator Parser (Semi SrcAnnExpr)
        prefix pops
            = Prefix $ do
                let p = choice $ map (\(name, f) -> prefixOp name f ) pops
                qs <- some p :: Parser [Semi SrcAnnExpr -> Semi SrcAnnExpr]
                pure $ foldr1 (\a b c -> do
                    b' <- b c
                    a (pure b')) qs

        prefixOp
            :: String
            -> ((UnaryOp () -> SrcAnnUnaryOp) -> SrcAnnExpr -> SrcAnnExprF SrcAnnExpr)
            -> Parser (Semi SrcAnnExpr -> Semi SrcAnnExpr)
        prefixOp name f = do
            (Ann a _) <- withSrcAnnId $ opLexeme name
            let opAnn un = Ann a un
            pure $ \e -> do
                x@(Fix (Ann b _)) <- e
                let a' = SrcSpan (srcStart a) (srcEnd b)
                pure $ Fix (Ann a' (f opAnn x))

-- | Parses an operand, sufficiently wrapped so as to act as an expression in
-- its own right.
operand :: Parser (Semi SrcAnnExpr)
operand
    = (fmap (dup Literal) <$> literalP)
    <|> (fmap (dup Variable) <$> try identifier)
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

-- | Given a left-hand expression, parses a postfix operator (either a selector,
-- an indexing operation, a slice, a type assertion or a call).
postfix :: SrcAnnExpr -> Parser (Semi SrcAnnExpr)
postfix e = selector e <|> index e <|> sliceE e <|> typeAssertion e <|> call e

-- | Parses a primary expression in the form of a "Selector".
--
-- This parser can be backtracked from until it parses a dot \".\" followed by
-- an identifier.
selector :: SrcAnnExpr -> Parser (Semi SrcAnnExpr)
selector e = do
    ident <- try $ do
        symbol "."
        identifier
    pure $ do
        i <- ident

        -- the source span of the selector expression starts before the
        -- expression to select in and ends after the identifier used to do the
        -- selection.
        let a = SrcSpan (srcStart (topAnn e)) (srcEnd (ann i))

        pure $ Fix $ Ann a $ Selector e i

-- | Parses a primary expression in the form of an "Index".
--
-- This parser can be backtracked from until the closing square bracket is
-- parsed.
index :: SrcAnnExpr -> Parser (Semi SrcAnnExpr)
index e = do
    (Ann b e') <- try $ withSrcAnnF $ squareBrackets (expr >>= noSemiP)
    pure $ do
        y <- e'

        -- the source span of the index expression starts before the expression
        -- to index in and ends after the square bracket
        let a = SrcSpan (srcStart (topAnn e)) (srcEnd b)

        pure $ Fix $ Ann a $ Index e y

sliceE :: SrcAnnExpr -> Parser (Semi SrcAnnExpr)
sliceE e = do
    (Ann b s) <- withSrcAnnF sliceBody

    pure $ do
        (eMin, eMax, eBnd) <- s

        let a = SrcSpan (srcStart (topAnn e)) (srcEnd b)

        pure $ Fix $ Ann a $ Slice e eMin eMax eBnd

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
typeAssertion :: SrcAnnExpr -> Parser (Semi SrcAnnExpr)
typeAssertion e = do
    try $ do
        symbol "."
        symbol "("
    t <- type_
    _ <- closeParen

    pure $ do
        x <- t

        let a = SrcSpan (srcStart (topAnn e)) (srcEnd (topAnn x))

        pure $ Fix $ Ann a $ TypeAssertion e x

-- | Parses a function call argument list.
arguments :: Parser (Maybe SrcAnnType, [SrcAnnExpr])
arguments = withType <|> noType where
    noType = do
        exprs <- (expr >>= noSemiP) `sepBy` symbol ","
        pure (Nothing, exprs)
    withType = do
        ty <- try $ type_ >>= noSemiP
        symbol ","
        exprs <- (expr >>= noSemiP) `sepBy` symbol ","
        pure (Just ty, exprs)


-- | Parses the function call postfix operator.
--
-- This parser can be backtracked from until the opening parenthesis (\"(\") of
-- the argument list is parsed.
call :: SrcAnnExpr -> Parser (Semi SrcAnnExpr)
call e = do
    try $ symbol "("
    (ty, exprs) <- arguments
    (Ann b p) <- withSrcAnnF closeParen

    pure $ do
        _ <- p -- Force semi evaluation on closing paren

        let a = SrcSpan (srcStart (topAnn e)) (srcEnd b)

        pure $ Fix $ Ann a $ Call e ty exprs
