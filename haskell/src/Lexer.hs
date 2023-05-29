{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Lexer
  ( token,
    parseLine,
    Token (..),
  )
where

import Data.Char (isDigit)
import Parser
  ( ParserState (..),
    ParserT,
    char,
    endOfLine,
    letter,
    many1,
    manyTill,
    peek,
    satisfy,
    skipWhile,
  )
import Relude.Unsafe qualified as Unsafe
import Prelude hiding (many, takeWhile)

data Token
  = Ident String
  | IntLiteral Integer
  | Assign
  | Plus
  | Minus
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Function
  | Let
  | Bang
  | Asterisk
  | Slash
  | LessThan
  | GreaterThan
  | BoolLiteral Bool
  | If
  | Else
  | Return
  | EqSymbol
  | NotEqSymbol
  deriving (Show, Eq)

parseLine :: ParserT Char Void [Token]
parseLine = do
  manyTill
    (skipWhile (\s -> s == ' ') *> token <* skipWhile (\s -> s == ' '))
    endOfLine

token :: ParserT Char Void Token
token =
  assignOrEq
    <|> plus
    <|> comma
    <|> semicolon
    <|> lparen
    <|> rparen
    <|> rbrace
    <|> lbrace
    <|> identifier
    <|> integer
    <|> minus
    <|> bangOrNotEq
    <|> asterisk
    <|> slash
    <|> lessThan
    <|> greaterThan

matchReservedWords :: String -> Token
matchReservedWords reservedWord =
  case reservedWord of
    "let" -> Let
    "fn" -> Function
    "true" -> BoolLiteral True
    "false" -> BoolLiteral False
    "if" -> If
    "else" -> Else
    "return" -> Return
    indentifier -> Ident indentifier

integer :: ParserT Char Void Token
integer = do
  integer <- Unsafe.read <$> many1 (satisfy isDigit)
  pure $ IntLiteral integer

identifier :: ParserT Char Void Token
identifier = do
  chars <- many1 letter
  pure $ matchReservedWords chars

assignOrEq :: ParserT Char e Token
assignOrEq = do
  c <- peek
  _assign <- char '='
  case c of
    Nothing -> pure Assign
    Just c
      | c == '=' -> do
          modify (\state -> state {input = drop 1 state.input})
          pure EqSymbol
      | otherwise -> pure Assign

plus :: ParserT Char e Token
plus = char '+' $> Plus

minus :: ParserT Char e Token
minus = char '-' $> Minus

comma :: ParserT Char e Token
comma = char ',' $> Comma

semicolon :: ParserT Char e Token
semicolon = char ';' $> Semicolon

lparen :: ParserT Char e Token
lparen = char '(' $> LParen

rparen :: ParserT Char e Token
rparen = char ')' $> RParen

lbrace :: ParserT Char e Token
lbrace = char '{' $> LBrace

rbrace :: ParserT Char e Token
rbrace = char '}' $> RBrace

bangOrNotEq :: ParserT Char e Token
bangOrNotEq = do
  c <- peek
  _bang <- char '!'
  case c of
    Nothing -> pure Bang
    Just c
      | c == '=' -> do
          modify (\state -> state {input = drop 1 state.input})
          pure NotEqSymbol
      | otherwise -> pure Bang

asterisk :: ParserT Char e Token
asterisk = char '*' $> Asterisk

slash :: ParserT Char e Token
slash = char '/' $> Slash

lessThan :: ParserT Char e Token
lessThan = char '<' $> LessThan

greaterThan :: ParserT Char e Token
greaterThan = char '>' $> GreaterThan
