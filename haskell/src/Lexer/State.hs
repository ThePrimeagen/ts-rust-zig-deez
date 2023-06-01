{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Lexer.State where

import Control.Monad.State (State, evalState, gets, modify)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit, isLetter, isSpace)
import Data.Function (fix)
import Token (Token (..), Tokenizer, identToken)

type Input = ByteString

{- | The Lexer data structure, where `input` is the program that we need to tokenize,
`ch` is the character that we are currenly over, and `position` is the read position.
-}
data Lexer = Lexer
    { input :: Input
    , ch :: Char
    , position :: Int
    }

type LexerT = State Lexer

tokenize :: Tokenizer
tokenize = evalState (advance >> lexer) . newLexer . BS.pack
  where
    lexer =
        nextToken >>= \case
            Eof -> pure [Eof]
            t -> (t :) <$> lexer

newLexer :: Input -> Lexer
newLexer input = Lexer input '\0' 0

nextToken :: LexerT Token
nextToken = do
    skipWhitespace
    gets ch >>= \case
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
    position <- gets position
    pure $ if position >= BS.length input then '\0' else BS.index input position

advance :: LexerT ()
advance = modify $ \lexer@Lexer{..} ->
    lexer
        { input = input
        , ch = if position >= BS.length input then '\0' else BS.index input position
        , position = position + 1
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
    gets ch >>= \case
        x | p x -> advance >> (x :) <$> loop
        _ -> pure ""

skipWhile :: (Char -> Bool) -> LexerT ()
skipWhile p = fix $ \loop ->
    gets ch >>= \case
        x | p x -> advance >> loop
        _ -> pure ()
