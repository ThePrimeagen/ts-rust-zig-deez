module TokenTypes
  ( Lexer(..)
  , Token(..)
  , TokenType(..)
  ) where

data Lexer =
  Lexer
    { input        :: String
    , position     :: Int
    , readPosition :: Int
    , ch           :: Char
    }
  deriving (Show)

newtype Token =
  Token
    { tokenType :: TokenType
    }
  deriving (Show, Eq)

data TokenType
  = ILLEGAL
  | EOF
  | IDENT String
  | INT String
  | EQUAL
  | PLUS
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | FUNCTION
  | LET
  deriving (Show, Eq)
