{-|
Module      : Language.GoLite.Syntax
Description : GoLite syntax definitions
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE OverloadedStrings #-}

module Language.GoLite.Syntax
( Package(..)
, TopLevelDecl(..)
, VarDecl(..)
, TypeDecl(..)
, TypeDeclBody(..)
, FieldDecl(..)
, FunDecl(..)
, FunParam
, Type(..)
, Statement(..)
, ForHead(..)
, CaseHead(..)
, Block
, Declaration(..)
, Expr(..)
, BinaryOp(..)
, UnaryOp(..)
, Arguments(..)
, Slice(..)
, Literal(..)
, Ident
, GoInt
, GoFloat
, GoRune
, GoString
) where

import Language.GoLite.Precedence
import Language.GoLite.Pretty

data Package
    = Package Ident [TopLevelDecl]
    deriving (Eq, Read, Show)

data TopLevelDecl
    = TopLevelDecl Declaration
    | TopLevelFun FunDecl
    deriving (Eq, Read, Show)

-- | The body of a variable declaration.
--
-- The identifier list and expression list must have the same length and must
-- both be nonempty. Optionally, the bindings can be annotated with a type.
data VarDecl
    = VarDeclBody [Ident] (Maybe Type) [Expr]
    deriving (Eq, Read, Show)

-- | A type declaration.
data TypeDecl
    = TypeDecls [TypeDeclBody]
    deriving (Eq, Read, Show)

-- | A body for a type declaration.
data TypeDeclBody
    = TypeAlias Ident Type
    | StructDecl [FieldDecl]
    deriving (Eq, Read, Show)

-- | A field in a struct.
data FieldDecl
    = FieldDecl [Ident] Type
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
    deriving (Eq, Read, Show)

-- | GoLite statements. These make up the bodies of functions.
data Statement
    = DeclStmt Declaration
    -- ^ Declarations are valid statements.
    | PrintStmt [Expr] Bool
    -- ^ Print a list of expressions to standard out, optionally with a
    -- newline.
    | ExprStmt Expr
    -- ^ Certain expressions are allowed as statements.
    | ReturnStmt (Maybe Expr)
    -- ^ A return statement may optionally return a value.
    | IfStmt (Maybe Statement) Expr Block (Maybe Block)
    -- ^ An if-statement consists of an optional initializer, followed by an
    -- expression and a sequence of statements acting as the then-body.
    -- An optional sequence of statements can follow, acting as the else-body.
    -- The else-if construct is simply represented as a nesting of
    -- if-statements within the else-block of another if-statement.
    | SwitchStmt (Maybe Statement) (Maybe Expr) [(CaseHead, [Statement])]
    -- ^ A switch statement consists of an optional initializer and an optional
    -- expression whose value is matched against by the expressions in the
    -- "CaseHead"s in the list of cases.
    | ForStmt ForHead Block
    -- ^ All loops are represented as for-loops. Different constructors of
    -- "ForHead" give different loop semantics.
    | BreakStmt
    -- ^ Break out of a loop.
    | ContinueStmt
    -- ^ Jump to the beginning of a loop.
    deriving (Eq, Read, Show)

-- | The head of a for-loop determines its semantics.
data ForHead
    = ForCondition Expr
    -- ^ A condition-based for-loop executes its body while the expression
    -- evaluates true.
    | ForClause (Maybe Statement) (Maybe Expr) (Maybe Statement)
    -- ^ An iteration-based for-loop executes its initializer, if any, and
    -- executes its body followed by its iteration-statement while its
    -- condition-expression evaluates true.
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
    | Index Expr Expr
    | Slice Expr Slice
    | TypeAssertion Expr Type
    | Call Expr Arguments
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

data UnaryOp
    = Positive
    | Negative
    | LogicalNot
    | BitwiseNot
    | Dereference
    | Reference
    | Receive
    deriving (Eq, Read, Show)

data Arguments
    = NormalArguments [Expr]
    | TypeArguments Type [Expr]
    deriving (Eq, Read, Show)

data Slice
    = SliceFromTo (Maybe Expr) (Maybe Expr)
    | SliceFromToStep (Maybe Expr) Expr Expr
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

instance Pretty BinaryOp where
    pretty o = case o of
        LogicalOr -> "||"
        LogicalAnd -> "&&"
        Equal -> "=="
        NotEqual -> "!="
        LessThan -> "<"
        LessThanEqual -> "<="
        GreaterThan -> ">"
        GreaterThanEqual -> ">="
        Plus -> "+"
        Minus -> "-"
        BitwiseOr -> "|"
        BitwiseXor -> "^"
        Times -> "*"
        Divide -> "/"
        Modulo -> "%"
        ShiftLeft -> "<<"
        ShiftRight -> ">>"
        BitwiseAnd -> "&"
        BitwiseAndNot -> "&^"

instance Pretty UnaryOp where
    pretty o = case o of
        Positive -> "+"
        Negative -> "-"
        LogicalNot -> "!"
        BitwiseNot -> "^"
        Dereference -> "*"
        Reference -> "&"
        Receive -> "<-"

instance Pretty Literal where
    pretty l = case l of
        IntLit x -> show x
        FloatLit x -> show x
        StringLit x -> show x
        RuneLit x -> show x

instance Pretty Char where
    pretty c = show c
    prettyList s = showString s

instance Pretty Expr where
    prettysPrec d e = case e of
        BinaryOp op l r -> showParen (d > precedence op) $ prettyInfix op l r
        UnaryOp op p -> showParen (d > precedence op) $ prettyPrefix op p
        -- never have to show parens around a literal or a variable
        Literal l -> prettysPrec d l
        Variable x -> showString x
        _ -> error "unknown"
