{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Lexer.State where

import Control.Monad (void)
import Control.Monad.State (State, evalState, gets, modify)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isSpace)
import Data.Function (fix)
import Data.Functor (($>))
import Token (Token (..), Tokenizer, identToken)

data Lexer = Lexer
    { input :: ByteString
    , ch :: Char
    , position :: Int
    }

type LexerState = State Lexer

tokenize :: Tokenizer
tokenize = evalState lexer . newLexer . BS.pack
  where
    lexer = do
        t <- nextToken
        if t == Eof
            then pure [t]
            else do
                ts <- lexer
                pure (t : ts)

newLexer :: ByteString -> Lexer
newLexer input = Lexer{..}
  where
    ch = if BS.null input then '\0' else BS.head input
    position = 1

nextToken :: LexerState Token
nextToken = do
    skipWhitespace
    c <- gets ch
    case c of
        '{' -> advance $> LSquirly
        '}' -> advance $> RSquirly
        '(' -> advance $> LParen
        ')' -> advance $> RParen
        ',' -> advance $> Comma
        ';' -> advance $> Semicolon
        '+' -> advance $> Plus
        '-' -> advance $> Minus
        '!' -> do
            c' <- peek
            if c' == '='
                then advance *> advance $> NotEqual
                else advance $> Bang
        '>' -> advance $> GreaterThan
        '<' -> advance $> LessThan
        '*' -> advance $> Asterisk
        '/' -> advance $> Slash
        '=' -> do
            c' <- peek
            if c' == '='
                then advance *> advance $> Equal
                else advance $> Assign
        '\0' -> advance $> Eof
        c' | c' `elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_" -> do
            identToken . BS.unpack <$> readIdent
        c' | c' `elem` ['0' .. '9'] -> do
            Int . BS.unpack <$> readInt
        _ -> advance $> Illegal

peek :: LexerState Char
peek = do
    input <- gets input
    position <- gets position
    pure $ if position >= BS.length input then '\0' else BS.index input position

advance :: LexerState ()
advance = modify $ \lexer@Lexer{..} ->
    lexer
        { input = input
        , ch = if position >= BS.length input then '\0' else BS.index input position
        , position = position + 1
        }

readWhile :: (Char -> Bool) -> LexerState ByteString
readWhile p = fix $ \loop ->
    gets ch >>= \case
        x | p x -> do
            advance
            BS.cons x <$> loop
        _ -> pure BS.empty

skipWhitespace :: LexerState ()
skipWhitespace = void (readWhile isSpace)

readIdent :: LexerState ByteString
readIdent = readWhile (`elem` ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "_")

readInt :: LexerState ByteString
readInt = readWhile (`elem` ['0' .. '9'])
