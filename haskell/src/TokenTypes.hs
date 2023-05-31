module TokenTypes
  ( Lexer(..)
  , Token(..)
  , TokenType(..)
  ) where

import Data.ByteString.Char8 (ByteString)

data Lexer =
  Lexer
    { _input        :: ByteString
    , _position     :: Int
    , _readPosition :: Int
    , _ch           :: Char
    }
  deriving (Show)

newtype Token =
  Token
    { _tokenType :: TokenType
    }
  deriving (Show, Eq)

data TokenType
  = ILLEGAL
  | EOF
  | IDENT ByteString
  | INT   ByteString
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