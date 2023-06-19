{-# LANGUAGE RecordWildCards #-}
-- |- This is a language extension for easier struct/record manipulation.

{-
                          README
----------------------------------------------------------
This Lexer is implemented using the Book's algorithm. We
define a struct (in Haskell named a Record type) and
simply we update its state as we parse the input string.

Notice that this implementation, despite of being good 
enough, it isn't very idiomatic. Normally you would use the
state monad to update everything ergonomically.

- This implementation is book compliant :)
- src/Lexer/Parsec.hs is the most idiomatic the technique

-}

module Lexer.Book where

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit, isSpace)
import Data.Maybe (fromMaybe)
import Token (Token (..), Tokenizer, identToken, isIdentChar)

type Input = ByteString -- A type synonym. The _type_ keyword makes a difference in the compile time guarantees. No need to explain

data Lexer = Lexer      -- Define a record. They must be introduce with _data_ keyword, instead of _type_ keyword as before
    { input :: Input
    , position :: Int
    , readPosition :: Int
    , ch :: Char
    }

tokenize :: Tokenizer                        -- A Tokenizer is a type synonym for a function from String to [Token]
tokenize = go . advance . newLexer . BS.pack -- pack the String into a ByteString, then initialize newLexer, then advance, then execute go
  where
    go lexer = case nextToken lexer of       -- The go function makes the recursive call, consuming the input list
        (Eof, _) -> [Eof]
        (token, lexer') -> token : go lexer' -- The notation lexer and lexer' is convention. It could be lexer1 and lexer2.

newLexer :: Input -> Lexer -- Create a new lexer for the given input
newLexer input =
    Lexer
        { input = input
        , position = 0
        , readPosition = 0
        , ch = '\0'
        }

nextToken :: Lexer -> (Token, Lexer) -- 
nextToken lexer = (token, lexer'') -- return a token and the updated lexer (defined below)
  where                            -- the _where_ keyword let you define variables after the function body. Usefull for readability
    lexer' = skipWhitespace lexer  -- skipWhitespace returns a new lexer in which the position has been increased
    (token, lexer'') =             -- As before, notation lexer, lexer', lexer'' is convention. No special meaning
        case ch lexer' of          -- pattern match on the lexer' character. Return the token and advance the lexer
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
                    then (NotEqual, advance $ advance lexer') -- Notice NotEqual consumes two characters, hence two calls to advance
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
peek Lexer{..} = fromMaybe '\0' $ input BS.!? readPosition -- Rust equiv: lexer.input.get(lexer.readPosition).unwrap_or('\0')
--                              |- The dollar is function application with low preference. Hence f $ g x is the same as f (g x)

advance :: Lexer -> Lexer
advance lexer@Lexer{..} = -- notation Lexer{..} makes the fields in the Lexer struct available as variables with the name of the field
    lexer
        { position = readPosition         -- Return the lexer updating the position. Notice that in Haskell you can not
        , readPosition = readPosition + 1 -- update a value, you just return a new value with the values updated, which isn't the same
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
