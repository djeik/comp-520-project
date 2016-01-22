{-# LANGUAGE OverloadedStrings #-}

module Language.GoLite.Syntax where

import Language.GoLite.Precedence
import Language.GoLite.Pretty

import Data.String ( IsString )

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

data TypeDeclBody
    = TypeAlias Ident Type
    | StructDecl [FieldDecl]
    deriving (Eq, Read, Show)

data FieldDecl
    = FieldDecl [Ident] Type
    deriving (Eq, Read, Show)

data FunDecl
    = FunDecl Ident [FunParam] (Maybe Type) Block
    deriving (Eq, Read, Show)

type FunParam = ([Ident], Type)

data Type
    = SliceType Type
    | ArrayType GoInt Type
    | NamedType Ident
    deriving (Eq, Read, Show)

-- | GoLite statements. These make up the bodies of functions.
data Statement
    = DeclStmt Declaration
    -- ^ Declarations are valid statements.
    | PrintStmt [Expr] Bool
    -- ^ Print a list of expressions to standard out, optionally with a
    -- newline.
    | ExprStmt Expr
    | ReturnStmt (Maybe Expr)
    | IfStmt (Maybe Statement) Expr Block (Maybe Statement)
    | SwitchStmt (Maybe Statement) (Maybe Expr) [(CaseHead, [Statement])]
    | ForStmt ForHead Block
    | BreakStmt
    | ContinueStmt
    deriving (Eq, Read, Show)

data ForHead
    = ForCondition Expr
    | ForClause (Maybe Statement) (Maybe Expr) (Maybe Statement)
    deriving (Eq, Read, Show)

data CaseHead
    = CaseDefault
    | CaseExpr [Expr]
    deriving (Eq, Read, Show)

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

keywords :: IsString a => [a]
keywords =
    -- standard Go keywords
    [ "break"
    , "case"
    , "chan"
    , "const"
    , "continue"
    , "default"
    , "defer"
    , "else"
    , "fallthrough"
    , "for"
    , "func"
    , "go"
    , "goto"
    , "if"
    , "import"
    , "interface"
    , "map"
    , "package"
    , "range"
    , "return"
    , "select"
    , "struct"
    , "switch"
    , "type"
    , "var"
    -- special GoLite keywords
    , "int"
    , "float64"
    , "bool"
    , "rune"
    , "string"
    , "print"
    , "println"
    , "append"
    ]

operators :: IsString a => [a]
operators =
    [ "+"
    , "-"
    , "*"
    , "/"
    , "%"
    , "&"
    , "|"
    , "^"
    , "<<"
    , ">>"
    , "&^"
    , "+="
    , "-="
    , "*="
    , "/="
    , "%="
    , "&="
    , "|="
    , "^="
    , "<<="
    , ">>="
    , "&^="
    , "&&"
    , "||"
    , "<-"
    , "++"
    , "-"
    , "=="
    , "<"
    , ">"
    , "="
    , "!"
    , "!="
    , "<="
    , ">="
    , ":="
    , "..."
    , "("
    , ")"
    , "["
    , "]"
    , "{"
    , "}"
    , ","
    , ";"
    , "."
    , ":"
    ]

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
