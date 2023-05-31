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
tokenize = Control.Monad.Trans.State.evalState getTokens . (\x -> readChar $ Lexer x 0 0 '\0')

getTokens :: Control.Monad.Trans.State.State Lexer [Token]
getTokens = do
  token <- getToken
  if   (token^.tokenType) == EOF
  then pure [token]
  else (token:) <$> getTokens

getToken :: Control.Monad.Trans.State.State Lexer Token
getToken = do
  Control.Monad.Trans.State.modify skipWhitespace
  lexer <- Control.Monad.Trans.State.get
  let readToken = (Control.Monad.Trans.State.modify readChar >>) . pure . Token
  case lexer^.ch of
    c | isLetter c || c == '_' ->  mapKeyword <$> readIdentifier
    c | isDigit c              ->  Token . INT <$> readDigit
    '+'  -> readToken PLUS
    '='  -> readToken EQUAL
    ','  -> readToken COMMA
    ';'  -> readToken SEMICOLON
    '('  -> readToken LPAREN
    ')'  -> readToken RPAREN
    '{'  -> readToken LBRACE
    '}'  -> readToken RBRACE
    '\0' -> readToken EOF
    _    -> readToken ILLEGAL

mapKeyword :: ByteString -> Token
mapKeyword "fn"  = Token FUNCTION
mapKeyword "let" = Token LET
mapKeyword ident = Token (IDENT ident)

readChar :: Lexer -> Lexer
readChar lexer = lexer & readPosition+~1 & ch.~ '\0' `fromMaybe` ((lexer^.input) !? (lexer^.readPosition))

readIdentifier :: Control.Monad.Trans.State.State Lexer ByteString
readIdentifier = do
  lexer <- Control.Monad.Trans.State.get
  if   isLetter (lexer^.ch) || lexer^.ch == '_'
  then Control.Monad.Trans.State.modify readChar >> cons (lexer^.ch) <$> readIdentifier
  else pure empty

readDigit :: Control.Monad.Trans.State.State Lexer ByteString
readDigit = do
  lexer <- Control.Monad.Trans.State.get
  if   isDigit (lexer^.ch)
  then Control.Monad.Trans.State.modify readChar >> cons (lexer^.ch) <$> readDigit
  else pure empty

skipWhitespace :: Lexer -> Lexer
skipWhitespace lexer
  | isSpace $ lexer^.ch = skipWhitespace $ readChar lexer
  | otherwise           = lexer
