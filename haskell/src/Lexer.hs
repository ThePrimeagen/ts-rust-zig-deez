module Lexer (
    tokenize
  , Token(..)
  , TokenType(..)
  )
  where

import qualified Data.ByteString.Char8                                           as C
import           Data.Char                 (isDigit, isLetter, isSpace)
import           TokenTypes                (Lexer(..), Token(..), TokenType(..))
import           Control.Lens              (makeLenses)
import           Control.Lens.Operators    ((+~), (.~), (^.), (&))
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

mapKeyword :: C.ByteString -> Token
mapKeyword "fn"  = Token FUNCTION
mapKeyword "let" = Token LET
mapKeyword ident = Token (IDENT ident)

readChar :: Lexer -> Lexer
readChar lexer = lexer & readPosition+~1 & ch.~ c
  where
    c | lexer^.readPosition < C.length (lexer^.input) = (lexer^.input) `C.index` (lexer^.readPosition)
      | otherwise = '\0'

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
