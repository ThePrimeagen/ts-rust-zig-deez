{-# LANGUAGE LambdaCase #-}

module Parser.Parsec where

import AST
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, between, choice, manyTill, optional, parse, satisfy, sepBy, sepEndBy, try, withRecovery, (<|>))
import Token

type Parser = Parsec Void [Token]

ast :: Ast
ast s = case parse programP "" s of
    Left e -> Left $ show e
    Right a -> Right a

programP :: Parser Program
programP = Program <$> manyTill statementP' (tokenP Eof)

statementP' :: Parser Statement
statementP' = withRecovery recover (statementP <* tokenP Semicolon)
  where
    recover = const $ IllegalStatement <$ manyTill anySingle (tokenP Semicolon)

statementP :: Parser Statement
statementP =
    choice
        [ do
            name <- tokenP Let >> identP
            expr <- tokenP Assign >> expressionP
            return $ LetStatement (Identifier name) expr
        , do
            expr <- tokenP Return >> expressionP
            return $ ReturnStatement expr
        , ExpressionStatement <$> expressionP
        , BlockStatement <$> blockP
        ]

termP :: Parser Expression
termP =
    choice
        [ IdentExpr <$> (Identifier <$> identP)
        , IntLiteral . read <$> numberP
        , BoolLiteral <$> ((True <$ tokenP TrueTok) <|> (False <$ tokenP FalseTok))
        , between (tokenP LParen) (tokenP RParen) expressionP
        , do
            cond <- tokenP If >> expressionP
            thenBlock <- blockP
            elseBlock <- optional $ tokenP Else >> blockP
            return $ IfExpr cond thenBlock elseBlock
        , do
            params <- tokenP Function >> between (tokenP LParen) (tokenP RParen) ((Identifier <$> identP) `sepBy` tokenP Comma)
            FunctionExpr params <$> blockP
        ]

expressionP :: Parser Expression
expressionP = makeExprParser termP table
  where
    table =
        [
            [ Postfix (flip CallExpr <$> between (tokenP LParen) (tokenP RParen) (expressionP `sepBy` tokenP Comma))
            ]
        ,
            [ Prefix (NegExpr <$ tokenP Minus)
            , Prefix (NotExpr <$ tokenP Bang)
            ]
        ,
            [ InfixL (MulExpr <$ tokenP Asterisk)
            , InfixL (DivExpr <$ tokenP Slash)
            ]
        ,
            [ InfixL (AddExpr <$ tokenP Plus)
            , InfixL (SubExpr <$ tokenP Minus)
            ]
        ,
            [ InfixL (EqExpr <$ tokenP Equal)
            , InfixL (NeExpr <$ tokenP NotEqual)
            , InfixL (LtExpr <$ tokenP LessThan)
            , InfixL (GtExpr <$ tokenP GreaterThan)
            ]
        ]

blockP :: Parser Block
blockP = Block <$> between (tokenP LSquirly) (tokenP RSquirly) (statementP `sepEndBy` tokenP Semicolon)

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
