from typing import Union
from lexer.lexer import Lexer, TokenType
from lexer.my_token import Token
from enum import Enum
from parser.my_ast import Expression, ExpressionStatement, Identifier, LetStatement, Program, ReturnStatement, Statement
from dataclasses import dataclass


class PRECEDENCE(Enum):
    LOWEST = 1
    EQUALS = 2
    LESSGREATER = 3
    SUM = 4
    PRODUCT = 5
    PREFIX = 6
    CALL = 7


class prefixParseFn:
    def __call__(self) -> Expression:
        ...


class infixParseFn:
    def __call__(self, inp: Expression) -> Expression:
        ...


@ dataclass
class Parser:
    lexer: Lexer
    curToken:  Token
    errors: list[str]

    prefixParseFns: dict[TokenType, prefixParseFn]
    infixParseFns: dict[TokenType, infixParseFn]

    peekToken: Token = None

    def __init__(self, lexer) -> None:
        self.lexer = lexer
        self.errors = []
        self.prefixParseFns = {}
        self.infixParseFns = {}
        self.nextToken()
        self.nextToken()

        self.registerPrefix(TokenType.IDENT, self.parseIdentifier)

    def nextToken(self):
        self.curToken = self.peekToken
        self.peekToken = self.lexer.nextToken()

    def parseIdentifier(self) -> Expression:
        return Identifier(Token=self.curToken,
                          Value=self.curToken.Literal)

    def ParseProgram(self) -> Program:
        program = Program([])
        while self.curToken.Type != TokenType.EOF:
            stmt = self.parseStatement()
            program.Statements.append(stmt)
            self.nextToken()
        return program

    def parseStatement(self) -> Union[Statement, None]:
        if self.curToken.Type == TokenType.LET:
            return self.parseLetStatement()
        if self.curToken.Type == TokenType.RETURN:
            return self.parseReturnStatement()
        else:
            return self.parseExpressionStatement()

    def parseExpressionStatement(self) -> ExpressionStatement:
        stmt_token = self.curToken
        stmt_expression = self.parseExpression(PRECEDENCE.LOWEST.value)
        if self.peekTokenIs(TokenType.SEMICOLON):
            self.nextToken()
        return ExpressionStatement(Token=stmt_token, Expression=stmt_expression)

    def parseExpression(self, precedence: int) -> Union[Expression, None]:
        prefix = self.prefixParseFns[self.curToken.Type]

        if prefix is None:
            return None

        leftExp = prefix()

        return leftExp

    def parseLetStatement(self) -> Union[LetStatement, None]:

        if not self.expectPeek(TokenType.IDENT):
            return None

        stmt_token = self.curToken
        stmt_name = Identifier(Token=self.curToken,
                               Value=self.curToken.Literal)
        if not self.expectPeek(TokenType.ASSIGN):
            return None

        while not self.curTokenIs(TokenType.SEMICOLON):
            self.nextToken()
        return LetStatement(Token=stmt_token, Name=stmt_name, Value=Expression())

    def parseReturnStatement(self) -> ReturnStatement:
        stmt_token = self.curToken
        self.nextToken()

        while not self.curTokenIs(TokenType.SEMICOLON):
            self.nextToken()

        return ReturnStatement(Token=stmt_token, ReturnValue=Expression())

    def curTokenIs(self, t: TokenType) -> bool:
        return self.curToken.Type == t

    def peekTokenIs(self, t: TokenType) -> bool:
        return self.peekToken.Type == t

    def expectPeek(self, t: TokenType) -> bool:
        if self.peekTokenIs(t):
            self.nextToken()
            return True
        self.peekError(t)
        return False

    def peekError(self, t: TokenType):
        msg = f"Expected next token to be {t.value}, got {self.peekToken.Type.value}"
        self.errors.append(msg)

    def registerPrefix(self, tokenType: TokenType, fn: prefixParseFn):
        self.prefixParseFns[tokenType] = fn

    def registerInfix(self, tokenType: TokenType, fn: infixParseFn):
        self.infixParseFns[tokenType] = fn
