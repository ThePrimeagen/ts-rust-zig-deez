{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Lexer.Lens where

import Control.Lens (makeLenses)
import Control.Lens.Combinators (use)
import Control.Lens.Operators ((.=), (<<+=))
import Control.Monad.State (State, evalState)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit, isLetter, isSpace)
import Data.Function (fix)
import Data.Maybe (fromMaybe)
import Token (Token (..), Tokenizer, identToken)

type Input = ByteString

data Lexer = Lexer
    { _input :: Input
    , _position :: Int
    , _readPosition :: Int
    , _ch :: Char
    }

makeLenses ''Lexer

type LexerT = State Lexer

tokenize :: Tokenizer
tokenize = evalState (advance >> go) . newLexer . BS.pack
  where
    go = do
        nextToken >>= \case
            Eof -> pure [Eof]
            token -> (token :) <$> go

newLexer :: Input -> Lexer
newLexer i =
    Lexer
        { _input = i
        , _position = 0
        , _readPosition = 0
        , _ch = '\0'
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
    readPos <- use readPosition
    buffer <- use input
    pure $ fromMaybe '\0' (buffer BS.!? readPos)

current :: LexerT Char
current = use ch

advance :: LexerT ()
advance = do
    -- Increment `readPosition` by 1 and return the old value
    -- readPosition++
    readPos <- readPosition <<+= 1
    buffer <- use input
    ch .= fromMaybe '\0' (buffer BS.!? readPos)
    position .= readPos

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
