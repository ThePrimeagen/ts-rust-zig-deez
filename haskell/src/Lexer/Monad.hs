{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
-- |- language extensions for easier struct/record manipulation.
-- |- and lambdas definition

{-
                          README
----------------------------------------------------------
This Lexer is implemented using the Book's algorithm, but
unlike src/Lexer/Book.hs, it implements the infamous monad
type class. Type classes are Haskell for Rust's traits.
(actually viceversa :P )

Notice that normally would use the already-implemented 
state monad in transformers library, whereas in this file
the state monad is implemented manually. So this is more
complicated than it should.

- This implementation is book compliant :)
- src/Lexer/Parsec.hs is the most idiomatic the technique

-}

module Lexer.Monad where

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isDigit, isSpace)
import Data.Function (fix)
import Data.Maybe (fromMaybe)
import Token (Token (..), Tokenizer, identToken, isIdentChar)

type Input = ByteString -- A type synonym. The _type_ keyword makes a difference in the compile time guarantees. No need to explain

data Lexer = Lexer      -- Define a record. They must be introduce with _data_ keyword, instead of _type_ keyword as before
    { input :: Input
    , position :: Int
    , readPosition :: Int
    , ch :: Char
    }

-- | Basically a `State Lexer` monad.
newtype LexerT a = LexerT {runLexer :: Lexer -> (a, Lexer)} 
-- the _newtype_ keyword has better compile time behaviour than _type_ (above), and better performance than _data_ (above)
-- but it is more limited than _data_ and more boilerplaty than _type_.

instance Functor LexerT where    -- Implement instances for all needed type classes. Rust equiv: impl Functor for LexerT
    fmap f (LexerT m) = LexerT $ \lexer ->
        let (a, lexer') = m lexer
         in (f a, lexer')

instance Applicative LexerT where -- Blah
    pure a = LexerT (a,)
    (<*>) (LexerT mf) (LexerT ma) = LexerT $ \lexer ->
        let (f, lexer') = mf lexer
            (a, lexer'') = ma lexer'
         in (f a, lexer'')

instance Monad LexerT where -- Blah, Blah
    (LexerT m) >>= k = LexerT $ \lexer ->
        let (a, lexer') = m lexer
         in runLexer (k a) lexer'

tokenize :: Tokenizer
tokenize = fst . runLexer (advance >> go) . newLexer . BS.pack -- Similar to src/Lexer/Book.hs
  where
    go = do
        nextToken >>= \case           -- run nextToken and then (>>=) pattern match the result:
            Eof -> pure [Eof]         -- Finish if the nextToken is Eof
            token -> (token :) <$> go -- continue executing otherwhise. (token :) means prepend the token to the recursive call.

newLexer :: Input -> Lexer  -- init new lexer at position 0
newLexer input =
    Lexer
        { input = input
        , position = 0
        , readPosition = 0
        , ch = '\0'
        }

nextToken :: LexerT Token
nextToken = do            -- do notation is syntactic sugar for monad shenanigans. It reads like imperative programming
    skipWhitespace        -- self explains
    current >>= \case     -- read (>>=) like 'and then'. Hence, get the current character _and then_ pattern macth the result.
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
                '=' -> NotEqual <$ advance <* advance -- <* is similar to <$. Essentially we are running advance effect twice.
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
        c | isIdentChar c -> identToken <$> readIdent -- This | is for pattern match with a boolean condition.
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
        , ch = fromMaybe '\0' $ input BS.!? readPosition -- Rust equiv: lexer.input.get(lexer.readPosition).unwrap_or('\0')
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
