{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
-- |- language extensions for easier struct/record manipulation.
-- |- and lambdas definition

{-
                          README
----------------------------------------------------------
This Lexer is implemented using the Book's algorithm, but
unlike src/Lexer/Monad.hs, it uses the State Monad 
implemented in the transformers library. This approach is
better than implement your own instances.

- This implementation is book compliant :)
- src/Lexer/Parsec.hs is the most idiomatic the technique

-}

module Lexer.State where

import Control.Monad.State (State, evalState, gets, modify)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit, isLetter, isSpace)
import Data.Function (fix)
import Data.Maybe (fromMaybe)
import Token (Token (..), Tokenizer, identToken)

type Input = ByteString -- A type synonym. The _type_ keyword makes a difference in the compile time guarantees. No need to explain

data Lexer = Lexer      -- Define a record. They must be introduce with _data_ keyword, instead of _type_ keyword as before
    { input :: Input
    , position :: Int
    , readPosition :: Int
    , ch :: Char
    }

type LexerT = State Lexer -- A type synonym. A LexerT (T at the end is convention), is just the State monad with the Lexer state 

tokenize :: Tokenizer
tokenize = evalState (advance >> go) . newLexer . BS.pack -- evalState runs the state monad.
  where
    go = do
        nextToken >>= \case           -- run nextToken and then (>>=) pattern match the result:
            Eof -> pure [Eof]         -- Finish if the nextToken is Eof
            token -> (token :) <$> go -- continue executing otherwhise. (token :) means prepend the token to the recursive call.

newLexer :: Input -> Lexer -- Create a new lexer for the given input
newLexer input =
    Lexer
        { input = input
        , position = 0
        , readPosition = 0
        , ch = '\0'
        }

nextToken :: LexerT Token
nextToken = do        -- do notation is syntactic sugar for monad shenanigans. It reads like imperative programming
    skipWhitespace    -- self explains
    current >>= \case -- read (>>=) like 'and then'. Hence, get the current character _and then_ pattern macth the result.
        '{' -> LSquirly <$ advance -- (a <$ b) means, return a but execute the side effects in b. Simple explanation with not functors involved
        '}' -> RSquirly <$ advance -- Compare this function to src/Lexer/Book.hs/nextToken. This is somewhat simpler because we are
        '(' -> LParen <$ advance   -- using the functor/applicative/monad interfaces.
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
        c | isIdentChar c -> identToken <$> readIdent  -- This | is for pattern match with a boolean condition.
        c | isDigit c -> Int <$> readInt
        _ -> Illegal <$ advance

-- Notice the functions below are simpler than those equivalent in src/Lexer/Monad.hs
-- This is because we are using the state monad interface, as opose to just using the monad interface.
peek :: LexerT Char
peek = do
    input <- gets input               -- gets is part of the state monad interface. Allows you to grab a state projection
    readPosition <- gets readPosition
    pure $ fromMaybe '\0' $ input BS.!? readPosition

current :: LexerT Char
current = gets ch

advance :: LexerT ()
advance = modify $ \lexer@Lexer{..} -> -- Modify allow for modifing the state for a given function
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
