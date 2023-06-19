{-# LANGUAGE OverloadedStrings #-}
-- |- language extension for better string manipulation.

{-
                          README
----------------------------------------------------------
This Lexer is implemented using the megaparsec library.
This library is a batteries include parsing library which
has all sort of combinators and utilities for writing your
own parsers and lexer. 

The inner implementation is surprisingly similar to that in
the Book, but it uses Haskell's abstractions capabilities to
make a very simple to use library.

- This implementation is somewhat? book compliant
- is the most idiomatic the technique!

-}


module Lexer.Parsec where

import Data.Char (isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, choice, empty, eof, errorBundlePretty, many, parse, takeWhile1P, try, (<|>))
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L
import Token (Token (..), Tokenizer, identToken, isIdentChar)

type Input = Text

type Lexer = Parsec Void Input -- A lexer is a Parsec with no user defined error (hence Void) and input type Text.

tokenize :: Tokenizer
tokenize input = case parse tokensP "" $ T.pack input of -- runs the tokensP parser
    Left err -> error ("lexer should not fail" ++ errorBundlePretty err) -- Left and Rigth are Rust equiv to 
    Right tokens -> tokens ++ [Eof]                                      -- Result::Err and Result::Ok
  where
    tokensP = sc *> many nextToken <* eof -- The tokensP parser is the space consumer, then many tokens and then the end of file

nextToken :: Lexer Token
nextToken =
    choice  -- It tries every parser in order, notice that symbol consumes input string, that isn't convinient if we just want to look ahead
        [ LSquirly <$ symbol "{" -- read "return LSquirly if you can parse symbol {"
        , RSquirly <$ symbol "}" -- read "return RSquirly if you can parse symbol }"
        , LParen <$ symbol "("   -- etc...
        , RParen <$ symbol ")"
        , Comma <$ symbol ","
        , Semicolon <$ symbol ";"
        , Plus <$ symbol "+"
        , Minus <$ symbol "-"
        , try (NotEqual <$ symbol "!=") <|> (Bang <$ symbol "!") -- when we find ! we branch. try is sort of a look ahead: It doesn't consume input if fails. 
        , GreaterThan <$ symbol ">"
        , LessThan <$ symbol "<"
        , Asterisk <$ symbol "*"
        , Slash <$ symbol "/"
        , try (Equal <$ symbol "==") <|> (Assign <$ symbol "=") -- Again, when we find = we branch.
        , identToken <$> identP -- <$> means apply function identToken within the parser identP. Hence conver fn to FunctionToken, etc...
        , Int <$> intP
        , Illegal <$ lexeme anySingle -- anySingle returns the next lexeme in the string. At this point we now it is an invalid lexeme.
        ]

identP :: Lexer String
identP = T.unpack <$> lexeme (takeWhile1P Nothing isIdentChar) -- consume the string while it is a valid identifier character.

intP :: Lexer String
intP = T.unpack <$> lexeme (takeWhile1P Nothing isDigit) -- consume the string while it is a valid identifier interger character.

sc :: Lexer ()
sc = L.space space1 empty empty -- space consumer. Parse at least one space, and we don't allow line comments nor bock comments, hence empty empty. 

lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme sc -- utility function. It tells you how to parse a lexeme. Essentially it uses the space consumer to deal with space before and after

symbol :: Text -> Lexer Text
symbol = L.symbol sc -- utility function. Essentially it uses the space consumer to deal with space before and after the symbol

-- Notice the lexer is barely 40 lines of code.