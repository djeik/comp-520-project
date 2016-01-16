{-# LANGUAGE OverloadedStrings #-}

module Language.GoLite.Syntax where

import Data.String ( IsString )
import Data.Text ( Text )
import qualified Data.Text as T

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
    = UnaryExpr UnaryExpr
    | BinaryExpr BinaryOp Expr Expr
    deriving (Eq, Read, Show)

data BinaryOp
    = LogicalOr | LogicalAnd
    | Equal | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual
    | Plus | Minus | BitwiseOr | BitwiseXor
    | Times | Divide | Modulo
    | ShiftLeft | ShiftRight | BitwiseAnd | BitwiseAndNot
    deriving (Eq, Read, Show)

data UnaryExpr
    = PrimaryExpr PrimaryExpr
    | UnaryOp UnaryOp UnaryExpr
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

data PrimaryExpr
    = Operand Operand
    | Conversion Type Expr
    | Selector Expr Ident
    | Index Expr Expr
    | Slice Expr Slice
    | TypeAssertion Expr Type
    | Call Expr Arguments
    deriving (Eq, Read, Show)

data Arguments
    = NormalArguments [Expr]
    | TypeArguments Type [Expr]
    deriving (Eq, Read, Show)

data Slice
    = SliceFromTo (Maybe Expr) (Maybe Expr)
    | SliceFromToStep (Maybe Expr) Expr Expr
    deriving (Eq, Read, Show)

data Operand
    = LiteralOp Literal
    | OperandNameOp Ident
    | ExprOp Expr
    deriving (Eq, Read, Show)

data Literal
    = IntLit GoInt
    | FloatLit GoFloat
    | RuneLit GoRune
    | StringLit GoString
    deriving (Eq, Read, Show)

-- | Identifiers are just strings.
type Ident = Text

type GoInt = Int
type GoFloat = Double
type GoRune = Char
type GoString = Text

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
