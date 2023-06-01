module Lexer (
    tokenize
  , Token(..)
  , TokenType(..)
  )
  where

import Data.ByteString.Char8     (ByteString, cons, empty, (!?))
import Data.Char                 (isDigit, isLetter, isSpace)
import Data.Maybe                (fromMaybe)
import TokenTypes                (Lexer(..), Token(..), TokenType(..))
import Control.Lens              (makeLenses)
import Control.Lens.Operators    ((+~), (.~), (^.), (&))
import Control.Monad.Trans.State (State, modify, get, evalState)

makeLenses ''Lexer
makeLenses ''Token

tokenize :: ByteString -> [Token]
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
    c | isLetter c || c == '_' ->  mapKeyword <$> readIdentifier
    c | isDigit c              ->  Token . INT <$> readDigit
    '='  -> readToken EQUAL
    '+'  -> readToken PLUS
    ','  -> readToken COMMA
    ';'  -> readToken SEMICOLON
    '('  -> readToken LPAREN
    ')'  -> readToken RPAREN
    '{'  -> readToken LBRACE
    '}'  -> readToken RBRACE
    '\0' -> readToken EOF
    _    -> readToken ILLEGAL

mapKeyword :: ByteString -> Token
mapKeyword "fn"     = Token FUNCTION
mapKeyword "let"    = Token LET
mapKeyword ident    = Token (IDENT ident)

readChar :: Lexer -> Lexer
readChar lexer =
  lexer & ch.~ '\0' `fromMaybe` ((lexer^.input) !? (lexer^.readPosition))
    & position.~(lexer^.readPosition)
    & readPosition+~1

readIdentifier :: State Lexer ByteString
readIdentifier = do
  lexer <- get
  if   isLetter (lexer^.ch) || lexer^.ch == '_'
  then modify readChar >> cons (lexer^.ch) <$> readIdentifier
  else pure empty

readDigit :: State Lexer ByteString
readDigit = do
  lexer <- get
  if   isDigit (lexer^.ch)
  then modify readChar >> cons (lexer^.ch) <$> readDigit
  else pure empty

skipWhitespace :: Lexer -> Lexer
skipWhitespace lexer
  | isSpace $ lexer^.ch = skipWhitespace $ readChar lexer
  | otherwise           = lexer
