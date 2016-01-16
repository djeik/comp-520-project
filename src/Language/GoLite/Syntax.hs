{-# LANGUAGE OverloadedStrings #-}

module Language.GoLite.Syntax where

import Data.String ( IsString )
import Data.Text ( Text )
import qualified Data.Text as T

data Package
    = Package Ident [TopLevelDecl]

data TopLevelDecl
    = TopLevelDecl Declaration
    | TopLevelFun FunDecl

-- | The body of a variable declaration.
--
-- The identifier list and expression list must have the same length and must
-- both be nonempty. Optionally, the bindings can be annotated with a type.
data VarDecl
    = VarDeclBody [Ident] (Maybe Type) [Expr]

-- | A type declaration.
data TypeDecl
    = TypeDecls [TypeDeclBody]

data TypeDeclBody
    = TypeAlias Ident Type
    | StructDecl [FieldDecl]

data FieldDecl
    = FieldDecl [Ident] Type

data FunDecl
    = FunDecl Ident [FunParam] (Maybe Type) Block

type FunParam = ([Ident], Type)

data Type
    = SliceType Type
    | ArrayType GoInt Type
    | BasicType BasicType
    | NamedType Ident

-- | The five basic types of GoLite.
data BasicType
    = TyInt
    | TyFloat64
    | TyBool
    | TyRune
    | TyString

-- | Identifiers are just strings.
type Ident = Text

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

data ForHead
    = ForCondition Expr
    | ForClause (Maybe Statement) (Maybe Expr) (Maybe Statement)

data CaseHead
    = CaseDefault
    | CaseExpr [Expr]

type Block = [Statement]

data Declaration
    = TypeDecl TypeDecl
    | VarDecl VarDecl

data Expr
    = UnaryExpr UnaryExpr
    | BinaryExpr BinaryOp Expr Expr

data BinaryOp
    = LogicalOr | LogicalAnd
    | Equal | LessThan | LessThanEqual | GreaterThan | GreaterThanEqual
    | Plus | Minus | BitwiseOr | BitwiseXor
    | Times | Divide | Modulo
    | ShiftLeft | ShiftRight | BitwiseAnd | BitwiseNotAnd

data UnaryExpr
    = PrimaryExpr
    | UnaryOp UnaryOp UnaryExpr

data UnaryOp
    = Positive
    | Negative
    | LogicalNot
    | BitwiseNot
    | Dereference
    | Reference
    | Receive

data PrimaryExpr
    = Operand Operand
    | Conversion Type Expr
    | Selector PrimaryExpr Ident
    | Index PrimaryExpr Expr
    | Slice PrimaryExpr Slice
    | TypeAssertion PrimaryExpr Type
    | Call PrimaryExpr [FunParam]

data Slice
    = SliceFromTo (Maybe Expr) (Maybe Expr)
    | SliceFromToStep (Maybe Expr) Expr Expr

data Operand
    = LiteralOp Literal
    | OperandNameOp Ident
    | ExprOp Expr

data Literal
    = IntLit GoInt
    | FloatLit GoFloat
    | RuneLit GoRune
    | StringLit GoString

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
