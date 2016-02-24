{-|
Module      : Language.GoLite.Syntax.Types.Stmt
Description : Definitions and instances for statement syntax
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.GoLite.Syntax.Types.Stmt where

import Language.GoLite.Pretty

import Data.Functor.Foldable
import Text.PrettyPrint

-- | GoLite statements. These make up the bodies of functions.
data StatementF decl expr ident assignOp caseHead f
    = DeclStmt decl
    -- ^ Declarations are valid statements.
    | ExprStmt expr
    -- ^ Certain expressions are allowed as simple statements.
    | ShortVarDecl [ident] [expr]
    -- ^ A short variable declaration uses the := operator and omits the type
    -- from the declaration. It consists of a list of identifiers, followed by
    -- the short declaration operator, then a list of expressions. The two lists
    -- are grouped pair-wise to form all the initializations. ShortVarDecl is
    -- semantically different from VarDecl in the fact that it allows variable
    -- redeclaration under the condition that it must also declare a new
    -- variable at the same time.
    | Assignment [expr] assignOp [expr]
    -- ^ An assignment is two lists of expressions separated by an assignment
    -- operator. Possible assignment operators include +=, <<= and =.
    -- A statement like x += y is semantically different from x = x + y in that
    -- x is only evaluated once in the former case..
    | PrintStmt [expr]
    -- ^ Print a list of expressions to standard out, optionally with a
    -- newline.
    | ReturnStmt (Maybe expr)
    -- ^ A return statement may optionally return a value.
    | IfStmt (Maybe f) expr [f] (Maybe [f])
    -- ^ An if-statement consists of an optional initializer, followed by an
    -- expression and a sequence of statements acting as the then-body.
    -- An optional sequence of statements can follow, acting as the else-body.
    -- The else-if construct is simply represented as a nesting of
    -- if-statements within the else-block of another if-statement.
    | SwitchStmt (Maybe f) (Maybe expr) [(caseHead, [f])]
    -- ^ A switch statement consists of an optional initializer and an optional
    -- expression whose value is matched against by the expressions in the
    -- "CaseHead"s in the list of cases.
    | ForStmt (Maybe f) (Maybe expr) (Maybe f) [f]
    -- ^ All loops are represented as for-loops. If all three "Maybe"s are
    -- "Nothing", then we have an infinite loop. If only the "Expr" is present,
    -- then we have what's essentially a while loop. If either of the
    -- statements are present, then we have a bona fide for loop.
    --
    -- In Go, all loops are however written with the keyword @for@, so
    -- syntactically there is little difference.
    --
    -- Note that the statements involved can only be simple statements.
    | BreakStmt
    -- ^ Break out of a loop.
    | ContinueStmt
    -- ^ Jump to the beginning of a loop.
    | FallthroughStmt
    -- ^ Transfer control to the next case clause in a switch statement.
    | Block [f]
    -- ^ An explicit block, which can be used to influence scoping.
    | EmptyStmt
    -- ^ An empty statement. Does not have any semantic value, but can be
    -- inserted by semicolons.
    deriving (Eq, Read, Show, Functor)

-- | Prints a recursive statement structure bottom-up by dispatching to the
-- pretty-printers for any contained data.
--
-- Semicolons are not appended in general to the generated statements in order
-- to facilitate semicolon handling in places where it is necessary (e.g. for
-- loop initializers).
instance
    ( Pretty decl
    , Pretty expr
    , Pretty ident
    , Pretty assignOp
    , Pretty caseHead
    ) => Pretty (Fix (StatementF decl expr ident assignOp caseHead)) where

    pretty = cata f where
        f :: (Pretty a, Pretty b, Pretty c, Pretty d, Pretty e)
          => StatementF a b c d e Doc -> Doc
        f s = case s of
            DeclStmt d -> pretty d
            ExprStmt expr -> pretty expr
            ShortVarDecl ids exprs ->
                sep (punctuate comma (pretty <$> ids)) <+>
                text ":=" <+>
                sep (punctuate comma (pretty <$> exprs))
            Assignment exprs op moreExprs ->
                sep (punctuate comma (pretty <$> exprs)) <+>
                pretty op <+>
                sep (punctuate comma (pretty <$> moreExprs))
            PrintStmt exprs ->
                text "print" <> prettyParens True (
                    sep (punctuate comma (pretty <$> exprs))
                )
            ReturnStmt mexpr ->
                text "return" <+> pretty mexpr
            IfStmt minit cond body mbody ->
                text "if" <+> pretty minit <+> pretty cond <+> text "{" $+$
                nest indentLevel (
                    vcat (pretty <$> body)
                ) $+$
                text "}" $+$
                case mbody of
                    Just elseBody ->
                        text "else {" <+> vcat elseBody
                    Nothing -> empty
            BreakStmt -> text "break"
            ContinueStmt -> text "continue"
            FallthroughStmt -> text "fallthrough"
            SwitchStmt ini expr cases ->
                text "switch" <+>
                (case ini of
                    Just i -> pretty i <> semi
                    Nothing -> empty) <+>
                pretty expr <+>
                text "{" $+$
                nest indentLevel (
                    vcat (
                        map (
                            \(chead, body) ->
                                pretty chead $+$ nest indentLevel (vcat body)
                        ) cases
                    )
                ) $+$
                text "}"
            ForStmt ini expr step body ->
                text "for" <+>
                (case ini of
                    Just i -> pretty i <> semi
                    Nothing -> empty
                ) <+>
                (case expr of
                    Just expr' -> pretty expr' <> semi
                    Nothing -> empty
                ) <+>
                (case step of
                    Just step' -> pretty step'
                    Nothing -> empty
                ) <+>
                text "{" $+$
                nest indentLevel (vcat body) $+$
                text "}"
            Block ss ->
                text "{" $+$
                nest indentLevel (vcat ss) $+$
                text "}"
            EmptyStmt -> empty

-- | The head of a case.
data CaseHead expr
    = CaseDefault
    -- ^ The default case's body is executed if no other case is matched. There
    -- can be at most one default case in any given switch.
    | CaseExpr [expr]
    -- ^ A case's semantics are different according to whether its
    -- corresponding switch has an expression to match against.
    --
    -- If there is an expression to match against, then the expressions given
    -- in the list of the case head are compared for equality against the
    -- switch expression.
    --
    -- Otherwise, if any of the given expressions evaluates true, then the case
    -- is matched and its body is executed.
    deriving (Eq, Read, Show)

-- | Prints a case head, either the text \"default:\" or the \"case\" keyword
-- followed by the expressions to match on comma-separated.
instance Pretty expr => Pretty (CaseHead expr) where
    pretty e = case e of
        CaseDefault -> text "default:"
        CaseExpr exprs ->
            text "case" <+>
            sep (punctuate comma (pretty <$> exprs)) <>
            text ":"
