{-# LANGUAGE LambdaCase #-}
module Parser.Parsec where

import AST
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (lookAhead), many, Parsec, anySingle, between, choice, optional, parse, satisfy, sepBy, sepBy1, sepEndBy, sepEndBy1, try, eof)
import Token

type Parser = Parsec Void [Token]

ast :: Ast
ast s = case parse programP "" s of
    Left e -> Left $ show e
    Right a -> Right a

programP :: Parser Program
programP = Program <$> many statementP <* eof

statementP :: Parser Statement
statementP = choice
    [ do
        name <- tokenP Let >> identP
        expr <- tokenP Assign >> expressionP
        return $ LetStatement (Identifier name) expr
    ]

termP :: Parser Expression
termP = choice
    [ IdentExpr <$> (Identifier <$> identP)
    ]

expressionP :: Parser Expression
expressionP = termP


tokenP :: Token -> Parser Token
tokenP t = satisfy (== t)

identP :: Parser String
identP = try $ do
    anySingle >>= \case
        (Ident t) -> return t
        _ -> fail "Expected identifier"

numberP :: Parser String
numberP = try $ do
    anySingle >>= \case
        (Int t) -> return t
        _ -> fail "Expected integer"
