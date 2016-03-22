{-|
Module      : Language.GoLite.Syntax.Sugar.Statement
Description : Syntax sugar for synthesizing statements
Copyright   : (c) Jacob Errington and Frederic Lafrance, 2016
License     : MIT
Maintainer  : goto@mail.jerrington.me
Stability   : experimental
-}

{-# LANGUAGE TypeFamilies #-}

module Language.GoLite.Syntax.Sugar.Statement where

import Language.GoLite.Syntax.Types

-- | Constructs a statement tree for 'Assignment'.
assignment
    :: [expr]
    -> assignOp
    -> [expr]
    -> Fix (StatementF d expr i assignOp c)
assignment lhss op rhss = Fix $ Assignment lhss op rhss

increment :: expr -> Fix (StatementF d expr i a c)
increment = Fix . IncDecStmt Increment

decrement :: expr -> Fix (StatementF d expr i a c)
decrement = Fix . IncDecStmt Decrement

incdec :: IncDec -> expr -> Fix (StatementF d expr i a c)
incdec i = Fix . IncDecStmt i

-- | Constructs a statement tree for 'PrintStmt'.
printStmt :: [expr] -> Fix (StatementF d expr i a c)
printStmt = Fix . PrintStmt

-- | Constructs a statement tree for 'ReturnStmt'.
returnStmt :: (Maybe expr) -> Fix (StatementF d expr i a c)
returnStmt = Fix . ReturnStmt

-- | Constructs a statement tree for 'IfStmt'.
ifStmt
    :: f ~ Fix (StatementF d e i a c)
    => Maybe f
    -> e
    -> [f]
    -> Maybe [f]
    -> f
ifStmt ini c body ebody = Fix $ IfStmt ini c body ebody

-- | Constructs a statement tree for 'SwitchStmt'.
switchStmt
    :: Maybe (Fix (StatementF d e i a c))
    -> Maybe e
    -> [(c, [Fix (StatementF d e i a c)])]
    -> Fix (StatementF d e i a c)
switchStmt ini expr cases = Fix $ SwitchStmt ini expr cases

-- | Constructs a statement tree for 'ForStmt'.
forStmt
    :: f ~ Fix (StatementF d e i a c)
    => Maybe f
    -> Maybe e
    -> Maybe f
    -> [f]
    -> f
forStmt ini ex step body = Fix $ ForStmt ini ex step body

-- | Constructs a statement tree for 'BreakStmt'.
breakStmt :: Fix (StatementF d e i a c)
breakStmt = Fix BreakStmt

-- | Constructs a statement tree for 'ContinueStmt'.
continueStmt :: Fix (StatementF d e i a c)
continueStmt = Fix ContinueStmt

-- | Constructs a statement tree for 'FallthroughStmt'.
fallthroughStmt :: Fix (StatementF d e i a c)
fallthroughStmt = Fix FallthroughStmt

-- | Constructs a statement tree for 'DeclStmt'.
declStmt :: decl -> Fix (StatementF decl e i a c)
declStmt = Fix . DeclStmt

-- | Constructs a statement tree for 'ExprStmt'.
exprStmt :: expr -> Fix (StatementF d expr i a c)
exprStmt = Fix . ExprStmt

-- | Constructs a statement tree for 'ShortVarDecl'.
shortVarDecl :: [ident] -> [expr] -> Fix (StatementF d expr ident a c)
shortVarDecl is es = Fix $ ShortVarDecl is es

-- | Constructs a statement tree for 'Block'.
block :: f ~ Fix (StatementF d e i a c) => [f] -> f
block ss = Fix $ Block ss

-- | Constructs a statement tree for the empty statement
emptyStmt :: Fix (StatementF d e i a c)
emptyStmt = Fix EmptyStmt