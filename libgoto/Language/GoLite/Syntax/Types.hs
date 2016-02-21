{-|
Module      : Language.GoLite.Syntax.Types
Description : GoLite syntax definitions and instances
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Language.GoLite.Syntax.Types
( Package(..)
, TopLevelDecl(..)
, VarDecl(..)
, TypeDecl(..)
, FunDecl(..)
, TypeF(..)
, StatementF(..)
, CaseHead(..)
, Declaration(..)
, ExprF(..)
, BinaryOp(..)
, UnaryOp(..)
, AssignOp(..)
, Literal(..)
, Ident(..)
, GoInt
, GoFloat
, GoRune
, GoString
  -- * Convenience reexports
, Fix(..)
, Identity(..)
) where

import Language.GoLite.Precedence
import Language.GoLite.Pretty

import Data.Functor.Identity
import Data.Functor.Foldable
import Data.String
import Text.PrettyPrint

data Package ident topLevelDecl
    = Package ident [topLevelDecl]
    deriving (Eq, Read, Show)

data TopLevelDecl decl funDecl
    = TopLevelDecl decl
    | TopLevelFun funDecl
    deriving (Eq, Read, Show)

-- | The body of a variable declaration.
--
-- The identifier list must be nonempty. Either the type annotation or the list
-- of expressions may be omitted, but not both. If the type is omitted, the list
-- or expressions must have the same length as the identifier list.
data VarDecl ident ty expr
    = VarDeclBody [ident] (Maybe ty) [expr]
    deriving (Eq, Read, Show)

-- | A type declaration.
data TypeDecl ident ty
    = TypeDeclBody ident ty
    deriving (Eq, Read, Show)

-- | A function declaration.
data FunDecl ident ty stmt
    = FunDecl ident [([ident], ty)] (Maybe ty) [stmt]
    deriving (Eq, Read, Show)

-- | A type.
data TypeF ident int f
    = SliceType f
    -- ^ A slice is a compound type, and represent a resizable array of
    -- elements of some other type.
    | ArrayType int f
    -- ^ An arrays is a compound type, and represents a statically-sized array
    -- of elements of some other type.
    | NamedType ident
    -- ^ A named type is the category into which all other types fall. It is
    -- simply an identifier.
    | StructType [([ident], f)]
    deriving (Eq, Read, Show, Functor)

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
    -- semantically different from VarDecl in the fact that only the former can
    -- appear in contexts where a simple statement is expected.
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
    deriving (Eq, Read, Show, Functor)

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

data Declaration typeDecl varDecl
    = TypeDecl typeDecl
    | VarDecl varDecl
    deriving (Eq, Read, Show)

-- UnaryOp Positive
--  :: SrcAnnExpr -> ExprF UnaryOp id bin UnaryOp lit ty SrcAnnExpr

data ExprF id bin un lit ty f
    = BinaryOp bin f f
    | UnaryOp un f
    | Conversion ty f
    | Selector f id
    {-|
    \"An expression of the form

    > a[x]

    denotes the element of the array, pointer to array, slice, string or map a
    indexed by @x@. The value @x@ is called the /index/ or /map key/,
    respectively.

    The following rules apply:

    * If @a@ is not a map

        * The index x must be of integer type or untyped;
        * It is in range if @0 <= x < len(a)@;
        * Otherwise it is out of range a constant index must be non-negative
        and representable by a value of type int.

    * For @a@ of array type A

        A constant index must be in range if x is out of range at run time, a
        run-time panic occurs @a[x]@ is the array element at index x and the type
        of @a[x]@ is the element type of A

    * For @a@ of pointer to array type

        @a[x]@ is shorthand for @(*a)[x]@

    * For @a@ of slice type /S/

        * If @x@ is out of range at run time, a run-time panic occurs.
        * @a[x]@ is the slice element at index @x@ and the type of @a[x]@ is the
        element type of /S/

    * For @a@ of string type

        * A constant index must be in range if the string @a@ is also constant;
        * If @x@ is out of range at run time, a run-time panic occurs;
        * @a[x]@ is the non-constant byte value at index @x@ and the type of @a[x]@
        is byte;
        * @a[x]@ may not be assigned to.

    * For a of map type M

        * @x@'s type must be assignable to the key type of /M/;
        * If the map contains an entry with key @x@, @a[x]@ is the map value with
        key @x@ and the type of @a[x]@ is the value type of /M/;
        * If the map is @nil@ or does not contain such an entry, @a[x]@ is the zero
        value for the value type of /M/;
        * Otherwise @a[x]@ is illegal.
    -}
    | Index
        { indexExpr :: f
        , indexExprValue :: f
        }
    -- | \"Slice expressions construct a substring or slice from a string,
    -- array, pointer to array, or slice. There are two variants: a simple form
    -- that specifies a low and high bound, and a full form that also specifies
    -- a bound on the capacity.\"
    | Slice
        { sliceExpr :: f
        -- ^ The expression to take a slice of.
        , sliceExprLow :: Maybe f
        -- ^ The low index to select from.
        , sliceExprHigh :: Maybe f
        -- ^ The high index to select up to.
        , sliceExprBound :: Maybe f
        -- ^ An upper bound on the capacity of the resulting slice object. No
        -- more than this many elements will be selected.
        }
    | TypeAssertion f ty
    | Call f (Maybe ty) [f]
    | Literal lit
    | Variable id
    deriving (Eq, Read, Show, Functor)

-- | A binary operator.
--
-- Derives Functor so that we can painlessly use 'Language.GoLite.Annotation'.
data BinaryOp a
    = LogicalOr | LogicalAnd
    | Equal | NotEqual | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual
    | Plus | Minus | BitwiseOr | BitwiseXor
    | Times | Divide | Modulo
    | ShiftLeft | ShiftRight | BitwiseAnd | BitwiseAndNot
    deriving (Eq, Read, Show, Functor)

-- | An assignment operator.
--
-- Derives Functor so that we can painlessly use 'Language.GoLite.Annotation'.
data AssignOp a
    = Assign
    | PlusEq | MinusEq | BitwiseOrEq | BitwiseXorEq
    | TimesEq | DivideEq | ModuloEq
    | ShiftLeftEq | ShiftRightEq | BitwiseAndEq | BitwiseAndNotEq
    deriving (Eq, Read, Show, Functor)

-- | A unary operator.
--
-- Derives Functor so that we can painlessly use 'Language.GoLite.Annotation'.
data UnaryOp a
    = Positive
    | Negative
    | LogicalNot
    | BitwiseNot
    | Dereference
    | Reference
    | Receive
    deriving (Eq, Read, Show, Functor)

-- | A literal.
--
-- Derives functor so that we can painlessly use 'Language.GoLite.Annotation'.
data Literal a
    = IntLit GoInt
    | FloatLit GoFloat
    | RuneLit GoRune
    | StringLit GoString
    deriving (Eq, Read, Show, Functor)

-- | Identifiers are just wrapped strings.
data Ident a = Ident String deriving (Eq, Read, Show, Functor)

instance IsString (Ident a) where
    fromString = Ident

type GoInt = Int
type GoFloat = Double
type GoRune = Char
type GoString = String

instance HasPrecedence (BinaryOp a) where
    precedence o = case o of
        LogicalOr -> 0
        LogicalAnd -> 1
        GreaterThanEqual -> 2
        GreaterThan -> 2
        LessThanEqual -> 2
        LessThan -> 2
        NotEqual -> 2
        Equal -> 2
        BitwiseXor -> 3
        BitwiseOr -> 3
        Minus -> 3
        Plus -> 3
        BitwiseAndNot -> 4
        BitwiseAnd -> 4
        ShiftLeft -> 4
        ShiftRight -> 4
        Modulo -> 4
        Divide -> 4
        Times -> 4

instance HasPrecedence (UnaryOp a) where
    precedence o = case o of
        Receive -> 5
        Reference -> 5
        Dereference -> 5
        BitwiseNot -> 5
        LogicalNot -> 5
        Negative -> 5
        Positive -> 5

instance Pretty (AssignOp a) where
    pretty o = case o of
        Assign -> text "="
        PlusEq -> text "+="
        MinusEq -> text "-="
        BitwiseOrEq -> text "|="
        BitwiseXorEq -> text "^="
        TimesEq -> text "*="
        DivideEq -> text "/="
        ModuloEq -> text "%="
        ShiftLeftEq -> text "<<="
        ShiftRightEq -> text ">>="
        BitwiseAndEq -> text "&="
        BitwiseAndNotEq -> text "&^="

instance Pretty (BinaryOp a) where
    pretty o = case o of
        LogicalOr -> text "||"
        LogicalAnd -> text "&&"
        Equal -> text "=="
        NotEqual -> text "!="
        LessThan -> text "<"
        LessThanEqual -> text "<="
        GreaterThan -> text ">"
        GreaterThanEqual -> text ">="
        Plus -> text "+"
        Minus -> text "-"
        BitwiseOr -> text "|"
        BitwiseXor -> text "^"
        Times -> text "*"
        Divide -> text "/"
        Modulo -> text "%"
        ShiftLeft -> text "<<"
        ShiftRight -> text ">>"
        BitwiseAnd -> text "&"
        BitwiseAndNot -> text "&^"

instance Pretty (UnaryOp a) where
    pretty o = case o of
        Positive -> text "+"
        Negative -> text "-"
        LogicalNot -> text "!"
        BitwiseNot -> text "^"
        Dereference -> text "*"
        Reference -> text "&"
        Receive -> text "<-"

instance Pretty (Literal a) where
    pretty l = case l of
        IntLit x -> pretty x
        FloatLit x -> pretty x
        StringLit x -> text $ show x
        RuneLit x -> text $ show x

instance Pretty (Ident a) where
    pretty (Ident s) = text s

instance
    ( Pretty ident
    , Pretty int
    ) => Pretty (Fix (TypeF ident int)) where
    prettyPrec _ = cata f where -- TODO investigate precedence rules for types
        f :: (Pretty ident, Pretty int)
          => TypeF ident int Doc -> Doc
        f e = case e of
            SliceType t -> text "[]" <> t
            ArrayType i t -> prettyBrackets True (pretty i) <> t
            NamedType n -> pretty n
            StructType t ->
                text "struct" <+> prettyBraces True (
                    sep $ map (\(ids, ty) ->
                        sep (punctuate comma (map pretty ids)) <+> ty <> semi
                    ) t
                )

instance
    ( Pretty id
    , Pretty bin
    , Pretty un
    , Pretty lit
    , Pretty ty
    , HasPrecedence bin
    , HasPrecedence un
    ) => Pretty (Fix (ExprF id bin un lit ty)) where
    pretty = snd . cata f where

        -- the F-algebra that we use here keeps track of the precendence levels
        -- of subexpressions so that we can properly parenthesize them in
        -- higher branches of the recursion.
        f ::
            ( Pretty id
            , Pretty bin
            , Pretty un
            , Pretty lit
            , Pretty ty
            , HasPrecedence bin
            , HasPrecedence un
            ) => ExprF id bin un lit ty (Int, Doc) -> (Int, Doc)
        f e = case e of
            BinaryOp op (dl, l) (dr, r) -> (precedence op,) $
                prettyParens (dl > precedence op) l <+>
                pretty op <+>
                prettyParens (dr > precedence op) r
            UnaryOp op (dp, p) -> (precedence op,) $
                pretty op <>
                prettyParens (dp > precedence op) p
            Literal l -> (6, pretty l)
            Variable x -> (6, pretty x)
            Slice (ep, ex) lo hi up -> (6,) $
                prettyParens (ep < 6) ex <>
                prettyBrackets True (
                    pretty (snd <$> lo) <>
                    text ":" <>
                    pretty (snd <$> hi) <>
                    case up of
                        Just u -> text ":" <> pretty (snd u)
                        Nothing -> empty
                )
            Call (fp, fb) ty args -> (6,) $
                prettyParens (fp < 6) (prettyPrec 6 fb) <>
                prettyParens True (
                    case args of
                        [] -> pretty ty
                        s -> case ty of
                            Nothing ->
                                sep $
                                punctuate comma $
                                map (pretty . snd) s
                            Just t ->
                                sep $
                                punctuate comma $
                                pretty t : map (pretty . snd) s
                )
            Conversion ty (_, p) -> (6,) $
                pretty ty <> prettyParens True p
            Selector (ds, s) i -> (6,) $
                prettyParens (ds < 6) s <> text "." <> pretty i
            TypeAssertion (dex, ex) ty -> (6,) $ cat
                [ prettyParens (dex < 6) ex
                , text "."
                , prettyParens True (pretty ty)
                ]
            Index (dex, ex) (_, i) -> (6,) $ cat
                [ prettyParens (dex < 6) ex
                , prettyBrackets True i
                ]

instance
    ( Pretty decl
    , Pretty funDecl
    ) => Pretty (TopLevelDecl decl funDecl) where

    pretty e = case e of
        TopLevelDecl d -> pretty d
        TopLevelFun f -> pretty f

instance
    ( Pretty ident
    , Pretty ty
    , Pretty stmt
    ) => Pretty (FunDecl ident ty stmt) where

    pretty (FunDecl i args mret body)
        = text "func"
        <+> pretty i
        <+> prettyParens True prettyArgs
        <+> pretty mret
        <+> text "{"
        $+$ nest indentLevel (vcat $
            map pretty body
        )
        $+$ text "}" where
            prettyArgs = sep (punctuate comma (pa <$> args))
            pa (ids, ty) = sep (punctuate comma (pretty <$> ids)) <+> pretty ty

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
                text "print" <+>
                sep (punctuate comma (pretty <$> exprs)) <>
                semi
            ReturnStmt mexpr ->
                text "return" <+> pretty mexpr <> semi
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
            BreakStmt -> text "break" <> semi
            ContinueStmt -> text "continue" <> semi
            FallthroughStmt -> text "fallthrough" <> semi
            SwitchStmt ini expr cases ->
                text "switch" <+>
                (case ini of
                    Just i -> pretty i <> semi
                    Nothing -> empty) <+>
                pretty expr <+>
                text "{" <+>
                nest indentLevel (
                    vcat (
                        map (
                            \(chead, body) ->
                                pretty chead $+$ nest indentLevel (vcat body)
                        ) cases
                    )
                ) <>
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
                    Just step' -> pretty step' <> semi
                    Nothing -> empty
                ) <+>
                text "{" $+$
                nest indentLevel (vcat body) $+$
                text "}"

instance
    ( Pretty typeDecl
    , Pretty varDecl
    ) => Pretty (Declaration typeDecl varDecl) where

    pretty e = case e of
        TypeDecl t -> pretty t <> semi
        VarDecl t -> pretty t <> semi

instance
    ( Pretty ident
    , Pretty ty
    ) => Pretty (TypeDecl ident ty) where

    pretty e = case e of
        TypeDeclBody ident ty -> text "type" <+> pretty ident <+> pretty ty

instance
    ( Pretty ident
    , Pretty ty
    , Pretty expr
    ) => Pretty (VarDecl ident ty expr) where

    pretty e = case e of
        VarDeclBody idents mty exprs ->
            text "var" <+>
            sep (punctuate comma (pretty <$> idents)) <+>
            pretty mty <+>
            text "=" <+>
            sep (punctuate comma (pretty <$> exprs))

instance Pretty expr => Pretty (CaseHead expr) where
    pretty e = case e of
        CaseDefault -> text "default :"
        CaseExpr exprs ->
            text "case" <+>
            sep (punctuate comma (pretty <$> exprs)) <>
            text ":"
