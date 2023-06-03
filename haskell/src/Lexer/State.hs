{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Lexer.State where

import Control.Monad.State (State, evalState, gets, modify)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit, isLetter, isSpace)
import Data.Function (fix)
import Data.Maybe (fromMaybe)
import Token (Token (..), Tokenizer, identToken)

type Input = ByteString

data Lexer = Lexer
    { input :: Input
    , position :: Int
    , readPosition :: Int
    , ch :: Char
    }

type LexerT = State Lexer

tokenize :: Tokenizer
tokenize = evalState (advance >> go) . newLexer . BS.pack
  where
    go = do
        nextToken >>= \case
            Eof -> pure [Eof]
            token -> (token :) <$> go

newLexer :: Input -> Lexer
newLexer input =
    Lexer
        { input = input
        , position = 0
        , readPosition = 0
        , ch = '\0'
        }

nextToken :: LexerT Token
nextToken = do
    skipWhitespace
    current >>= \case
        '{' -> LSquirly <$ advance
        '}' -> RSquirly <$ advance
        '(' -> LParen <$ advance
        ')' -> RParen <$ advance
        ',' -> Comma <$ advance
        ';' -> Semicolon <$ advance
        '+' -> Plus <$ advance
        '-' -> Minus <$ advance
        '!' ->
            peek >>= \case
                '=' -> NotEqual <$ advance <* advance
                _ -> Bang <$ advance
        '>' -> GreaterThan <$ advance
        '<' -> LessThan <$ advance
        '*' -> Asterisk <$ advance
        '/' -> Slash <$ advance
        '=' ->
            peek >>= \case
                '=' -> Equal <$ advance <* advance
                _ -> Assign <$ advance
        '\0' -> Eof <$ advance
        c | isIdentChar c -> identToken <$> readIdent
        c | isDigit c -> Int <$> readInt
        _ -> Illegal <$ advance

peek :: LexerT Char
peek = do
    input <- gets input
    readPosition <- gets readPosition
    pure $ fromMaybe '\0' $ input BS.!? readPosition

current :: LexerT Char
current = gets ch

advance :: LexerT ()
advance = modify $ \lexer@Lexer{..} ->
    lexer
        { input = input
        , ch = fromMaybe '\0' $ input BS.!? readPosition
        , position = readPosition
        , readPosition = readPosition + 1
        }

skipWhitespace :: LexerT ()
skipWhitespace = skipWhile isSpace

readIdent :: LexerT String
readIdent = readWhile isIdentChar

readInt :: LexerT String
readInt = readWhile isDigit

isIdentChar :: Char -> Bool
isIdentChar c = isLetter c || c == '_'

readWhile :: (Char -> Bool) -> LexerT String
readWhile p = fix $ \loop ->
    current >>= \case
        x | p x -> advance >> (x :) <$> loop
        _ -> pure ""

skipWhile :: (Char -> Bool) -> LexerT ()
skipWhile p = fix $ \loop ->
    current >>= \case
        x | p x -> advance >> loop
        _ -> pure ()
