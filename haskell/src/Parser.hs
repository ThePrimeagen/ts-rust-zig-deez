{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Parser
  ( ParserT,
    Error (..),
    ParserState (..),
    runParser,
    satisfy,
    char,
    string,
    oneOf,
    many1,
    letter,
    manyTill,
    whitespace,
    skipMany,
    endOfLine,
    peek,
    skipWhile,
    takeWhile,
    takeTill,
    newline,
    anyChar,
  )
where

import Data.Char (isLetter, isSpace)
import Prelude hiding (takeWhile, traceShow)

data Error i e
  = EndOfInput
  | Illegal i
  | CustomError e
  | Empty
  deriving (Eq, Show)

newtype ParserT i e a = ParserT
  { runParserT :: StateT (ParserState i) (Either [Error i e]) a
  }

data ParserState i = ParserState
  { input :: [i],
    position :: Int,
    ch :: Maybe i
  }
  deriving (Show, Eq)

defaultParserState :: [i] -> ParserState i
defaultParserState i =
  ParserState
    { input = i,
      position = 0,
      ch = listToMaybe i
    }

runParser ::
  forall i e a.
  ParserT i e a ->
  [i] ->
  Either [Error i e] (a, ParserState i)
runParser parser i = runStateT (runParserT parser) (defaultParserState i)

instance Functor (ParserT i e) where
  fmap :: (a -> b) -> ParserT i e a -> ParserT i e b
  fmap f (ParserT parser) = ParserT $ fmap f parser

instance Applicative (ParserT i e) where
  pure :: a -> ParserT i e a
  pure a = ParserT $ pure a

  (<*>) :: ParserT i e (a -> b) -> ParserT i e a -> ParserT i e b
  ParserT fn <*> ParserT parser = ParserT $ StateT $ \state -> do
    (atob, state) <- runStateT fn state
    (a, state) <- runStateT parser state
    pure (atob a, state)

instance Monad (ParserT i e) where
  (>>=) :: ParserT i e a -> (a -> ParserT i e b) -> ParserT i e b
  ParserT a >>= b = ParserT $ a >>= runParserT . b

instance (Eq i, Eq e) => Alternative (ParserT i e) where
  empty :: ParserT i e a
  empty = ParserT $ StateT $ \_state -> Left [Empty]

  ParserT leftParser <|> ParserT rightParser = ParserT $ StateT $ \state ->
    case runStateT leftParser state of
      Left err ->
        case runStateT rightParser state of
          Left err' -> Left $ uniq $ err <> err'
          Right (output, state) -> Right (output, state)
      Right (output, state) -> Right (output, state)
    where
      uniq :: (Eq i, Eq e) => [Error i e] -> [Error i e]
      uniq [] = []
      uniq (e : es) = e : uniq ((filter (/= e) es))

instance MonadState (ParserState i) (ParserT i e) where
  get = ParserT get
  put = ParserT <<< put

satisfy :: (i -> Bool) -> ParserT i e i
satisfy pred = ParserT $ do
  parserState <- get
  case parserState.input of
    [] -> lift $ Left [EndOfInput]
    c : cs
      | pred c -> do
          modify
            ( \parserState ->
                parserState
                  { input = cs,
                    position = parserState.position + 1,
                    ch = listToMaybe parserState.input
                  }
            )

          lift $ Right c
      | otherwise -> lift $ Left [Illegal c]

char :: Eq i => i -> ParserT i e i
char i = satisfy (== i)

string :: Eq i => [i] -> ParserT i e [i]
string characters = traverse char characters

oneOf :: Eq i => [i] -> ParserT i e i
oneOf str = satisfy (\char -> char `elem` str)

many1 :: Alternative f => f a -> f [a]
many1 parser = liftA2 (:) parser (many parser)

letter :: ParserT Char e Char
letter = satisfy (\c -> isLetter c || c == '_')

manyTill :: Alternative f => f a -> f b -> f [a]
manyTill parser end = scan
  where
    scan =
      (end *> pure []) <|> liftA2 (:) parser scan

skipMany :: Alternative f => f a -> f ()
skipMany parser = scan
  where
    scan = (parser *> scan) <|> pure ()

peek :: ParserT i e (Maybe i)
peek = ParserT $ do
  parserState <- get
  case parserState.input of
    [] -> do
      modify (\state -> state {input = []})
      pure Nothing
    a : b : rest -> do
      modify (\state -> state {input = a : b : rest})
      pure $ Just b
    rest -> do
      modify (\state -> state {input = rest})
      pure Nothing

whitespace :: Eq e => ParserT Char e String
whitespace = many1 (satisfy isSpace)

endOfLine :: ParserT Char Void ()
endOfLine = (char '\n' >> pure ()) <|> (string "\r\n" >> pure ())

newline :: ParserT Char Void Char
newline = char '\n'

skipWhile :: (Char -> Bool) -> ParserT Char e ()
skipWhile pred = do
  parserState <- get
  go parserState.input
  where
    go [] = pure ()
    go (c : cs) =
      when (pred c && cs /= mempty) $ do
        modify (\state -> state {input = cs})
        go cs

takeWhile :: (Char -> Bool) -> ParserT Char e String
takeWhile pred = do
  parserState <- get
  go parserState.input
  where
    go s@[] = pure s
    go (c : cs) =
      if pred c
        then do
          modify (\state -> state {input = [c]})
          go cs
        else do
          modify (\state -> state {input = cs})
          pure [c]

takeTill :: (Char -> Bool) -> ParserT Char e String
takeTill pred = takeWhile (not <<< pred)

anyChar :: ParserT i e i
anyChar = satisfy (const True)
