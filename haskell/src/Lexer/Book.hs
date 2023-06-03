{-# LANGUAGE RecordWildCards #-}

module Lexer.Book where

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe)
import Token (Token (..), Tokenizer, identToken, isIdentChar)

type Input = ByteString

data Lexer = Lexer
    { input :: Input
    , position :: Int
    , readPosition :: Int
    , ch :: Char
    }

tokenize :: Tokenizer
tokenize = go . advance . newLexer . BS.pack
  where
    go lexer = case nextToken lexer of
        (Eof, _) -> [Eof]
        (token, lexer') -> token : go lexer'

newLexer :: Input -> Lexer
newLexer input =
    Lexer
        { input = input
        , position = 0
        , readPosition = 0
        , ch = '\0'
        }

nextToken :: Lexer -> (Token, Lexer)
nextToken lexer = (token, lexer'')
  where
    lexer' = skipWhitespace lexer
    (token, lexer'') =
        case ch lexer' of
            '{' -> (LSquirly, advance lexer')
            '}' -> (RSquirly, advance lexer')
            '(' -> (LParen, advance lexer')
            ')' -> (RParen, advance lexer')
            ',' -> (Comma, advance lexer')
            ';' -> (Semicolon, advance lexer')
            '+' -> (Plus, advance lexer')
            '-' -> (Minus, advance lexer')
            '!' ->
                if peek lexer' == '='
                    then (NotEqual, advance $ advance lexer')
                    else (Bang, advance lexer')
            '>' -> (GreaterThan, advance lexer')
            '<' -> (LessThan, advance lexer')
            '*' -> (Asterisk, advance lexer')
            '/' -> (Slash, advance lexer')
            '=' ->
                if peek lexer' == '='
                    then (Equal, advance $ advance lexer')
                    else (Assign, advance lexer')
            '\0' -> (Eof, lexer')
            c
                | isIdentChar c ->
                    let (str, lexer''') = readIdent lexer'
                     in (identToken str, lexer''')
            c
                | isDigit c ->
                    let (str, lexer''') = readInt lexer'
                     in (Int str, lexer''')
            _ -> (Illegal, advance lexer')

peek :: Lexer -> Char
peek Lexer{..} = fromMaybe '\0' $ input BS.!? readPosition

advance :: Lexer -> Lexer
advance lexer@Lexer{..} =
    lexer
        { position = readPosition
        , readPosition = readPosition + 1
        , ch = peek lexer
        }

{- Implementation details -}

skipWhitespace :: Lexer -> Lexer
skipWhitespace = skipWhile isSpace

readIdent :: Lexer -> (String, Lexer)
readIdent = readWhile isIdentChar

readInt :: Lexer -> (String, Lexer)
readInt = readWhile isDigit

readWhile :: (Char -> Bool) -> Lexer -> (String, Lexer)
readWhile p lexer@Lexer{..} =
    if p ch
        then
            let (str, lexer') = readWhile p $ advance lexer
             in (ch : str, lexer')
        else ("", lexer)

skipWhile :: (Char -> Bool) -> Lexer -> Lexer
skipWhile p lexer@Lexer{..} =
    if p ch
        then skipWhile p $ advance lexer
        else lexer
