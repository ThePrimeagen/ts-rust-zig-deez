module AST where

import Token

type Ast = [Token] -> Program             -- type synonym. 

newtype Program = Program [Statement]     -- _newtype_ keyword is similar to _type_ but with better compile time guarantees
    deriving (Show, Eq)                   -- It is used for structs with only one field.

data Statement                            -- enum Statement {
    = LetStatement Identifier Expression  --  LetStament(Identifier, Expression)
    | ReturnStatement Expression          --  ReturnStatement(Expression)
    | ExpressionStatement Expression      --  ...
    | BlockStatement Block
    | IllegalStatement
    deriving (Show, Eq)

data Expression
    = IdentExpr Identifier
    | IntLiteral Integer
    | BoolLiteral Bool
    | NegExpr Expression
    | NotExpr Expression
    | AddExpr Expression Expression
    | SubExpr Expression Expression
    | MulExpr Expression Expression
    | DivExpr Expression Expression
    | EqExpr Expression Expression
    | NeExpr Expression Expression
    | LtExpr Expression Expression
    | GtExpr Expression Expression
    | IfExpr Expression Block (Maybe Block)
    | FunctionExpr [Identifier] Block
    | CallExpr Expression [Expression]
    deriving (Show, Eq)

newtype Identifier = Identifier String
    deriving (Show, Eq)

newtype Block = Block [Statement]
    deriving (Show, Eq)
