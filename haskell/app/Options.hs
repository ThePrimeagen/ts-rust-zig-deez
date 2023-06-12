{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Options where

import Lexer.Basic qualified
import Lexer.Book qualified
import Lexer.Lens qualified
import Lexer.Monad qualified
import Lexer.Parsec qualified
import Lexer.State qualified

import Parser.Parsec qualified

import Options.Applicative

import AST
import Token

data Options = Options
    { lexer :: Tokenizer
    , parser :: Ast
    }

options :: Parser Options
options = do
    lexer <- chooseLexer <$> strOption (long "lexer" <> metavar "LEXER" <> showDefault <> value "parsec" <> help "The type of the lexer")
    parser <- chooseParser <$> strOption (long "parser" <> metavar "PARSER" <> showDefault <> value "parsec" <> help "The type of the parser")
    pure Options{..}

getOptions :: IO Options
getOptions = execParser opts
  where
    opts =
        info
            (options <**> helper)
            ( fullDesc
                <> progDesc "Interpret a program in Monkey Lang"
                <> header "monkey - an interpreter for Monkey Lang"
            )

chooseLexer :: String -> Tokenizer
chooseLexer "basic" = Lexer.Basic.tokenize
chooseLexer "book" = Lexer.Book.tokenize
chooseLexer "monad" = Lexer.Monad.tokenize
chooseLexer "state" = Lexer.State.tokenize
chooseLexer "parsec" = Lexer.Parsec.tokenize
chooseLexer "lens" = Lexer.Lens.tokenize
chooseLexer _ = error "Unknown lexer: use basic, book, monad, state, parsec or lens"

chooseParser :: String -> Ast
chooseParser "parsec" = Parser.Parsec.ast
chooseParser _ = error "Unknown parser: use parsec"
