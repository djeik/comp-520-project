{-|
Module      : Language.GoLite.Syntax
Description : GoLite syntax definitions
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.GoLite.Syntax
( Package(..)
, TopLevelDecl(..)
, VarDecl(..)
, TypeDecl(..)
, FunDecl(..)
, FunParam
, Type(..)
, Statement(..)
, CaseHead(..)
, Block
, Declaration(..)
, Expr(..)
, BinaryOp(..)
, UnaryOp(..)
, AssignOp(..)
, Literal(..)
, Ident
, GoInt
, GoFloat
, GoRune
, GoString
) where

import Language.GoLite.Precedence
import Language.GoLite.Pretty

import Text.PrettyPrint

data Package
    = Package Ident [TopLevelDecl]
    deriving (Eq, Read, Show)

data TopLevelDecl
    = TopLevelDecl Declaration
    | TopLevelFun FunDecl
    deriving (Eq, Read, Show)

-- | The body of a variable declaration.
--
-- The identifier list must be nonempty. Either the type annotation or the list
-- of expressions may be omitted, but not both. If the type is omitted, the list
-- or expressions must have the same length as the identifier list.
data VarDecl
    = VarDeclBody [Ident] (Maybe Type) [Expr]
    deriving (Eq, Read, Show)

-- | A type declaration.
data TypeDecl
    = TypeDeclBody Ident Type
    deriving (Eq, Read, Show)

-- | A function declaration.
data FunDecl
    = FunDecl Ident [FunParam] (Maybe Type) Block
    deriving (Eq, Read, Show)

-- | A single parameter declaration group for a function.
--
-- A parameter declaration group consists of one or more identifiers given the
-- same type.
type FunParam = ([Ident], Type)

-- | A type.
data Type
    = SliceType Type
    -- ^ A slice is a compound type, and represent a resizable array of
    -- elements of some other type.
    | ArrayType GoInt Type
    -- ^ An arrays is a compound type, and represents a statically-sized array
    -- of elements of some other type.
    | NamedType Ident
    -- ^ A named type is the category into which all other types fall. It is
    -- simply an identifier.
    | StructType [([Ident], Type)]
    deriving (Eq, Read, Show)

-- | GoLite statements. These make up the bodies of functions.
data Statement
    = DeclStmt Declaration
    -- ^ Declarations are valid statements.
    | ExprStmt Expr
    -- ^ Certain expressions are allowed as simple statements.
    | ShortVarDecl [Ident] [Expr]
    -- ^ A short variable declaration uses the := operator and omits the type
    -- from the declaration. It consists of a list of identifiers, followed by
    -- the short declaration operator, then a list of expressions. The two lists
    -- are grouped pair-wise to form all the initializations. ShortVarDecl is
    -- semantically different from VarDecl in the fact that only the former can
    -- appear in contexts where a simple statement is expected.
    | Assignment [Expr] AssignOp [Expr]
    -- ^ An assignment is two lists of expressions separated by an assignment
    -- operator. Possible assignment operators include +=, <<= and =.
    -- A statement like x += y is semantically different from x = x + y in that
    -- x is only evaluated once in the former case..
    | PrintStmt [Expr] Bool
    -- ^ Print a list of expressions to standard out, optionally with a
    -- newline.
    | ReturnStmt (Maybe Expr)
    -- ^ A return statement may optionally return a value.
    | IfStmt (Maybe Statement) Expr Block (Maybe Block)
    -- ^ An if-statement consists of an optional initializer, followed by an
    -- expression and a sequence of statements acting as the then-body.
    -- An optional sequence of statements can follow, acting as the else-body.
    -- The else-if construct is simply represented as a nesting of
    -- if-statements within the else-block of another if-statement.
    | SwitchStmt (Maybe Statement) (Maybe Expr) [(CaseHead, Block)]
    -- ^ A switch statement consists of an optional initializer and an optional
    -- expression whose value is matched against by the expressions in the
    -- "CaseHead"s in the list of cases.
    | ForStmt (Maybe Statement) (Maybe Expr) (Maybe Statement) Block
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
    deriving (Eq, Read, Show)

-- | The head of a case.
data CaseHead
    = CaseDefault
    -- ^ The default case's body is executed if no other case is matched. There
    -- can be at most one default case in any given switch.
    | CaseExpr [Expr]
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

-- | A block is simply a list of statements.
type Block = [Statement]

data Declaration
    = TypeDecl TypeDecl
    | VarDecl VarDecl
    deriving (Eq, Read, Show)

data Expr
    = BinaryOp BinaryOp Expr Expr
    | UnaryOp UnaryOp Expr
    | Conversion Type Expr
    | Selector Expr Ident
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
        { indexExpr :: Expr
        , indexExprValue :: Expr
        }
    -- | \"Slice expressions construct a substring or slice from a string,
    -- array, pointer to array, or slice. There are two variants: a simple form
    -- that specifies a low and high bound, and a full form that also specifies
    -- a bound on the capacity.\"
    | Slice
        { sliceExpr :: Expr
        -- ^ The expression to take a slice of.
        , sliceExprLow :: Maybe Expr
        -- ^ The low index to select from.
        , sliceExprHigh :: Maybe Expr
        -- ^ The high index to select up to.
        , sliceExprBound :: Maybe Expr
        -- ^ An upper bound on the capacity of the resulting slice object. No
        -- more than this many elements will be selected.
        }
    | TypeAssertion Expr Type
    | Call Expr (Maybe Type) [Expr]
    | Literal Literal
    | Variable Ident
    deriving (Eq, Read, Show)

data BinaryOp
    = LogicalOr | LogicalAnd
    | Equal | NotEqual | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual
    | Plus | Minus | BitwiseOr | BitwiseXor
    | Times | Divide | Modulo
    | ShiftLeft | ShiftRight | BitwiseAnd | BitwiseAndNot
    deriving (Eq, Read, Show)

data AssignOp
    = Assign
    | PlusEq | MinusEq | BitwiseOrEq | BitwiseXorEq
    | TimesEq | DivideEq | ModuloEq
    | ShiftLeftEq | ShiftRightEq | BitwiseAndEq | BitwiseAndNotEq
    deriving (Eq, Read, Show)

data UnaryOp
    = Positive
    | Negative
    | LogicalNot
    | BitwiseNot
    | Dereference
    | Reference
    | Receive
    deriving (Eq, Read, Show)

data Literal
    = IntLit GoInt
    | FloatLit GoFloat
    | RuneLit GoRune
    | StringLit GoString
    deriving (Eq, Read, Show)

-- | Identifiers are just strings.
type Ident = String

type GoInt = Int
type GoFloat = Double
type GoRune = Char
type GoString = String

instance HasPrecedence BinaryOp where
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

instance HasPrecedence UnaryOp where
    precedence o = case o of
        Receive -> 5
        Reference -> 5
        Dereference -> 5
        BitwiseNot -> 5
        LogicalNot -> 5
        Negative -> 5
        Positive -> 5

instance Pretty AssignOp where
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

instance Pretty BinaryOp where
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

instance Pretty UnaryOp where
    pretty o = case o of
        Positive -> text "+"
        Negative -> text "-"
        LogicalNot -> text "!"
        BitwiseNot -> text "^"
        Dereference -> text "*"
        Reference -> text "&"
        Receive -> text "<-"

instance Pretty Literal where
    pretty l = case l of
        IntLit x -> pretty x
        FloatLit x -> pretty x
        StringLit x -> text $ show x
        RuneLit x -> pretty x

instance Pretty Type where
    prettyPrec d e = case e of
        SliceType t -> text "[]" <> prettyPrec d t
        ArrayType i t -> prettyBrackets True (pretty i) <> prettyPrec d t
        NamedType n -> text n
        StructType t ->
            text "struct" <+> prettyBraces True (
                sep $ map (\(ids, ty) ->
                    sep (punctuate comma (map text ids)) <+> pretty ty <> semi
                ) t
            )

instance Pretty Expr where
    prettyPrec d e = case e of
        BinaryOp op l r -> prettyParens (d > precedence op) $ prettyInfix op l r
        UnaryOp op p -> prettyParens (d > precedence op) $ prettyPrefix op p
        Literal l -> prettyPrec d l
        Variable x -> text x
        Slice ex lo hi up ->
            prettyPrec 6 ex <>
            prettyBrackets True (
                pretty lo <>
                text ":" <>
                pretty hi <>
                case up of
                    Just u -> text ":" <> pretty u
                    Nothing -> empty
            )
        Call f ty args ->
            prettyPrec 6 f <>
            prettyParens True (
                case args of
                    [] -> pretty ty
                    s -> case ty of
                        Nothing -> sep $ punctuate comma $ map pretty s
                        Just t -> sep $ punctuate comma $ pretty t : map pretty s
            )
        _ -> error "Pretty: Expr"
