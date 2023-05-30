module TokenTypes
  ( Lexer(..)
  , Token(..)
  , TokenType(..)
  ) where

import qualified Data.ByteString.Char8 as C

data Lexer =
  Lexer
    { _input        :: C.ByteString
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
  | IDENT C.ByteString
  | INT C.ByteString
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
