module Lexer
  ( tokenize
  , Token(..)
  , TokenType(..)
  ) where

import Data.Char  (isDigit, isLetter, isSpace)
import TokenTypes (Lexer(..), Token(..), TokenType(..))

tokenize :: String -> [Token]
tokenize = getTokens . (\x -> readChar $ Lexer x 0 0 '\0')

getTokens :: Lexer -> [Token]
getTokens lexer =
  if   tokenType token == EOF
  then [token]
  else token : getTokens lexer'
  where
    (lexer', token) = getToken lexer

getToken :: Lexer -> (Lexer, Token)
getToken lexer =
  let lexer'    = skipWhitespace lexer
      c         = ch lexer'
      readTok t = (readChar lexer', Token t)
  in case c of
    c'| isLetter c' || c' == '_' -> getKeyword <$> readIdentifier lexer'
    c'| isDigit c'               -> Token . INT <$> readDigit lexer'
    '='  -> readTok EQUAL
    ';'  -> readTok SEMICOLON
    '('  -> readTok LPAREN
    ')'  -> readTok RPAREN
    ','  -> readTok COMMA
    '+'  -> readTok PLUS
    '{'  -> readTok LBRACE
    '}'  -> readTok RBRACE
    '\0' -> readTok EOF
    _    -> readTok ILLEGAL

getKeyword :: String -> Token
getKeyword "fn"  = Token FUNCTION
getKeyword "let" = Token LET
getKeyword ident = Token (IDENT ident)

readChar :: Lexer -> Lexer
readChar lexer =
  let c = if   readPosition  lexer >= length (input lexer)
          then '\0'
          else input lexer !! readPosition lexer
  in lexer { readPosition = readPosition lexer + 1, ch = c }

readIdentifier :: Lexer -> (Lexer, String)
readIdentifier lexer
  | isLetter c || c == '_' = (c:) <$> readIdentifier (readChar lexer)
  | otherwise              = (lexer, "")
  where
    c = ch lexer

readDigit :: Lexer -> (Lexer, String)
readDigit lexer
  | isDigit c = (c:) <$> readDigit (readChar lexer)
  | otherwise = (lexer, "")
  where
    c = ch lexer

skipWhitespace :: Lexer -> Lexer
skipWhitespace lexer
  | isSpace $ ch lexer = skipWhitespace $ readChar lexer
  | otherwise          = lexer
