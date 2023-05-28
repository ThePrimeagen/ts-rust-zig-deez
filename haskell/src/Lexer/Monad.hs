{-# LANGUAGE LambdaCase #-}

module Lexer.Monad where

import Control.Monad (ap)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char qualified as Char
import Data.Function (fix)
import Token (Token (..), Tokenizer, identToken)

newtype Position = Position Int
    deriving (Eq, Show)

-- | Basically a `ReaderT ByteString (State Position)` monad.
newtype Lexer a = Lexer {runLexer :: ByteString -> Position -> (a, Position)}
    deriving (Functor)

instance Applicative Lexer where
    pure a = Lexer $ \_ position -> (a, position)
    (<*>) = ap

instance Monad Lexer where
    m >>= k = Lexer $ \input position ->
        let (a, nextPosition) = runLexer m input position
         in runLexer (k a) input nextPosition

tokenize :: Tokenizer
tokenize input = fst $ runLexer lexer (BS.pack input) (Position 0)
  where
    lexer = do
        t <- nextToken
        if t == Eof
            then pure [t]
            else do
                ts <- lexer
                pure (t : ts)

advance :: Lexer ()
advance = Lexer $ \_ (Position pos) -> ((), Position (pos + 1))

peek :: Lexer (Maybe Char)
peek = Lexer $ \input (Position pos) -> (BS.indexMaybe input pos, Position pos)

{- | Read one `Char` at the current position and advance to the next position.
Returns `Nothing` if the position reached the end of the input.
-}
readChar :: Lexer (Maybe Char)
readChar = peek <* advance

{- | Skip many whitespace `Char`s and return the first non space `Char`
or `Nothing` if the input is exhausted.
-}
skipWhitespace :: Lexer (Maybe Char)
skipWhitespace =
    readChar >>= \case
        Nothing -> pure Nothing
        Just c
            | Char.isSpace c -> skipWhitespace
            | otherwise -> pure (Just c)

nextToken :: Lexer Token
nextToken =
    skipWhitespace >>= \case
        Nothing -> pure Eof
        Just c -> case c of
            '=' ->
                peek >>= \case
                    Just '=' -> Equal <$ advance
                    _ -> pure Assign
            '+' -> pure Plus
            '-' -> pure Minus
            '!' ->
                peek >>= \case
                    Just '=' -> NotEqual <$ advance
                    _ -> pure Bang
            '*' -> pure Asterisk
            '/' -> pure Slash
            '<' -> pure LessThan
            '>' -> pure GreaterThan
            ',' -> pure Comma
            ';' -> pure Semicolon
            '(' -> pure LParen
            ')' -> pure RParen
            '{' -> pure LSquirly
            '}' -> pure RSquirly
            x
                | isIdentChar x -> readIdent x
                | Char.isDigit x -> readInt x
                | otherwise -> pure Illegal

readIdent :: Char -> Lexer Token
readIdent c = identToken . (c :) <$> readWhile isIdentChar

readInt :: Char -> Lexer Token
readInt c = Int . (c :) <$> readWhile Char.isDigit

readWhile :: (Char -> Bool) -> Lexer String
readWhile p = fix $ \loop ->
    peek >>= \case
        Just x | p x -> do
            advance
            xs <- loop
            pure (x : xs)
        _ -> pure ""

isIdentChar :: Char -> Bool
isIdentChar c = Char.isLetter c || c == '_'
