{-# LANGUAGE LambdaCase #-}

module Parser.Parsec where
{-
   Notice that we use the same megaparsec library as in src/Lexer/Parsec.hs to build the Parser.
   This is the power of megaparsec. Converting from Text to [Token] is the lexer, and converting
   from [Token] -> Program is the Parser. We use the same underliying type and combinators.
-}

import AST
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Void (Void)
import Text.Megaparsec (Parsec, anySingle, between, choice, manyTill, optional, parse, satisfy, sepBy, sepEndBy, withRecovery, (<|>))
import Token

type Input = [Token]

type Parser = Parsec Void Input

ast :: Ast
ast s = case parse programP "" s of
    Left err -> error ("parser should not fail" ++ show err)
    Right p -> p

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
expressionP = makeExprParser termP table -- megaparsec utility to parse operators with precedence
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
-- simple a block is a bunch of statemts separated by Semicolon between tokens LSquirly and RSquirly, 

tokenP :: Token -> Parser Token
tokenP t = satisfy (== t)

identP :: Parser String
identP =
    satisfy isIdent >>= \case
        (Ident t) -> return t
        _ -> fail "Expected identifier"
  where
    isIdent (Ident _) = True
    isIdent _ = False

numberP :: Parser String
numberP =
    satisfy isNumber >>= \case
        (Int t) -> return t
        _ -> fail "Expected number"
  where
    isNumber (Int _) = True
    isNumber _ = False
