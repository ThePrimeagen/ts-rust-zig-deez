{-# LANGUAGE OverloadedStrings #-}                                                   -- for ByteString string literals
{-# LANGUAGE TemplateHaskell #-}                                                     -- for makeLenses

module Lexer (
    tokenize
  , Token(..)
  , TokenType(..)
  )
  where

import qualified Data.ByteString.Char8                                           as C -- ByteString is faster than String
import           Data.Char                 (isDigit, isLetter, isSpace)               -- don't know if it actually is in this case
import           TokenTypes                (Lexer(..), Token(..), TokenType(..))
import           Control.Lens              (makeLenses)                               -- unecessary but I wanted to try it
import           Control.Lens.Operators    ((+~), (.~), (^.), (&))                    -- unecessary but I wanted to try it
import           Control.Monad.Trans.State (State, modify, get, evalState)

makeLenses ''Lexer
makeLenses ''Token

tokenize :: C.ByteString -> [Token]
tokenize = evalState getTokens . (\x -> readChar $ Lexer x 0 0 '\0')

getTokens :: State Lexer [Token]
getTokens = do
  token <- getToken
  if   (token^.tokenType) == EOF
  then pure [token]
  else (token:) <$> getTokens

getToken :: State Lexer Token
getToken = do
  modify skipWhitespace
  lexer <- get
  let readToken = (modify readChar >>) . pure . Token
  case lexer^.ch of
    c | isLetter c || c == '_' ->  getKeyword <$> readIdentifier
    c | isDigit c              ->  Token . INT <$> readDigit
    '='  -> readToken EQUAL
    ';'  -> readToken SEMICOLON
    '('  -> readToken LPAREN
    ')'  -> readToken RPAREN
    ','  -> readToken COMMA
    '+'  -> readToken PLUS
    '{'  -> readToken LBRACE
    '}'  -> readToken RBRACE
    '\0' -> readToken EOF
    _    -> readToken ILLEGAL

getKeyword :: C.ByteString -> Token
getKeyword "fn"  = Token FUNCTION
getKeyword "let" = Token LET
getKeyword ident = Token (IDENT ident)

readChar :: Lexer -> Lexer
readChar lexer =
  let c = if   lexer^.readPosition >= C.length (lexer^.input)
          then '\0'
          else (lexer^.input) `C.index` (lexer^.readPosition)
  in lexer & readPosition +~ 1 & ch .~ c

readIdentifier :: State Lexer C.ByteString
readIdentifier = do
  lexer <- get
  if   isLetter (lexer^.ch) || lexer^.ch == '_'
  then modify readChar >> C.cons (lexer^.ch) <$> readIdentifier
  else pure C.empty

readDigit :: State Lexer C.ByteString
readDigit = do
  lexer <- get
  if   isDigit (lexer^.ch)
  then modify readChar >> C.cons (lexer^.ch) <$> readDigit
  else pure C.empty

skipWhitespace :: Lexer -> Lexer
skipWhitespace lexer
  | isSpace $ lexer^.ch = skipWhitespace $ readChar lexer
  | otherwise           = lexer
