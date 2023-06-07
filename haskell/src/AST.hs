module AST where

import Token

type Ast = [Token] -> Either String Program

data Program = Program [Statement]
  deriving (Show, Eq)

data Statement
    = LetStatement Identifier Expression
    deriving (Show, Eq)

data Expression
    = IdentExpr Identifier
    deriving (Show, Eq)

data Identifier = Identifier String
  deriving (Show, Eq)
