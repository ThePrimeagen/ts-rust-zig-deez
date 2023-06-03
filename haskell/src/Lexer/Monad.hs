{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Lexer.Monad where

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit, isSpace)
import Data.Function (fix)
import Data.Maybe (fromMaybe)
import Token (Token (..), Tokenizer, identToken, isIdentChar)

type Input = ByteString

data Lexer = Lexer
    { input :: Input
    , position :: Int
    , readPosition :: Int
    , ch :: Char
    }

-- | Basically a `State Lexer` monad.
newtype LexerT a = LexerT {runLexer :: Lexer -> (a, Lexer)}

instance Functor LexerT where
    fmap f (LexerT m) = LexerT $ \lexer ->
        let (a, lexer') = m lexer
         in (f a, lexer')

instance Applicative LexerT where
    pure a = LexerT (a,)
    (<*>) (LexerT mf) (LexerT ma) = LexerT $ \lexer ->
        let (f, lexer') = mf lexer
            (a, lexer'') = ma lexer'
         in (f a, lexer'')

instance Monad LexerT where
    (LexerT m) >>= k = LexerT $ \lexer ->
        let (a, lexer') = m lexer
         in runLexer (k a) lexer'

tokenize :: Tokenizer
tokenize = fst . runLexer (advance >> go) . newLexer . BS.pack
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
peek = LexerT $ \lexer@Lexer{..} -> (fromMaybe '\0' $ input BS.!? readPosition, lexer)

current :: LexerT Char
current = LexerT $ \lexer@Lexer{..} -> (ch, lexer)

advance :: LexerT ()
advance = LexerT $ \lexer@Lexer{..} ->
    ( ()
    , lexer
        { position = readPosition
        , readPosition = readPosition + 1
        , ch = fromMaybe '\0' $ input BS.!? readPosition
        }
    )

{- Implementation details -}

skipWhitespace :: LexerT ()
skipWhitespace = skipWhile isSpace

readIdent :: LexerT String
readIdent = readWhile isIdentChar

readInt :: LexerT String
readInt = readWhile isDigit

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
