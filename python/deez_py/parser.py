from enum import (
    Enum,
    IntEnum,
)
from typing import (
    Callable,
)

import deez_py.deez_ast as ast
import deez_py.token_types as TokenType

from .lexer import (
    Lexer,
)
from .tokens import (
    Token,
)

prefix_parse_func = Callable[[], ast.Expression]
infix_parse_func = Callable[[ast.Expression], ast.Expression]


class Precedence(IntEnum):
    LOWEST = 1
    EQUALS = 2  # ==
    LESSGREATER = 3  # > or <
    SUM = 4  # +
    PRODUCT = 5  # *
    PREFIX = 6  # -X or !X
    CALL = 7  # myFunction(X)


precedences = {
    TokenType.EQUAL: Precedence.EQUALS,
    TokenType.NOTEQUAL: Precedence.EQUALS,
    TokenType.LESSTHAN: Precedence.LESSGREATER,
    TokenType.GREATERTHAN: Precedence.LESSGREATER,
    TokenType.PLUS: Precedence.SUM,
    TokenType.MINUS: Precedence.SUM,
    TokenType.SLASH: Precedence.PRODUCT,
    TokenType.ASTERISK: Precedence.PRODUCT,
    TokenType.LPAREN: Precedence.CALL,
}


def token_type_to_precedence(type):
    if type in precedences:
        return precedences[type]
    return Precedence.LOWEST


class Parser:
    def __init__(self, lexer: Lexer) -> None:
        self.lexer: Lexer = lexer
        self.current_token: Token = None
        self.peek_token: Token = None
        self.errors: list[str] = []

        self.prefix_parse_funcs: dict[str, prefix_parse_func] = {}
        self.infix_parse_funcs: dict[str, infix_parse_func] = {}

        self.register_prefix(TokenType.IDENT, self.parse_identifier)
        self.register_prefix(TokenType.INT, self.parse_integer_literal)
        self.register_prefix(TokenType.BANG, self.parse_prefix_expression)
        self.register_prefix(TokenType.MINUS, self.parse_prefix_expression)
        self.register_prefix(TokenType.TRUE, self.parse_boolean)
        self.register_prefix(TokenType.FALSE, self.parse_boolean)
        self.register_prefix(TokenType.LPAREN, self.parse_grouped_expression)
        self.register_prefix(TokenType.IF, self.parse_if_expression)
        self.register_prefix(TokenType.FUNCTION, self.parse_function_literal)

        self.register_infix(TokenType.PLUS, self.parse_infix_expression)
        self.register_infix(TokenType.MINUS, self.parse_infix_expression)
        self.register_infix(TokenType.SLASH, self.parse_infix_expression)
        self.register_infix(TokenType.ASTERISK, self.parse_infix_expression)
        self.register_infix(TokenType.EQUAL, self.parse_infix_expression)
        self.register_infix(TokenType.NOTEQUAL, self.parse_infix_expression)
        self.register_infix(TokenType.LESSTHAN, self.parse_infix_expression)
        self.register_infix(TokenType.GREATERTHAN, self.parse_infix_expression)
        self.register_infix(TokenType.LPAREN, self.parse_call_expression)

        self.next_token()
        self.next_token()

    def next_token(self) -> None:
        self.current_token = self.peek_token
        self.peek_token = self.lexer.get_next_token()

    def register_prefix(self, token_type, func: prefix_parse_func) -> None:
        self.prefix_parse_funcs[token_type] = func

    def register_infix(self, token_type, func: infix_parse_func) -> None:
        self.infix_parse_funcs[token_type] = func

    def parse_program(self) -> ast.Program:
        program = ast.Program()

        while self.current_token.type != TokenType.EOF:
            stmt = self.parse_statement()
            if stmt:
                program.statements.append(stmt)
            self.next_token()

        return program

    def parse_identifier(self) -> ast.Expression:
        return ast.Identifier(
            token=self.current_token, value=self.current_token.literal
        )

    def parse_integer_literal(self) -> ast.Expression | None:
        int_lit = ast.IntegerLiteral(token=self.current_token)
        if not self.current_token.literal.isdigit():
            msg = "Could not parse %s as digit" % self.current_token.literal
            self.errors.append(msg)
            return None

        int_lit.value = int(self.current_token.literal)
        return int_lit

    def parse_boolean(self) -> ast.Expression:
        return ast.Boolean(
            token=self.current_token,
            value=(self.current_token.type == TokenType.TRUE),
        )

    def parse_grouped_expression(self) -> ast.Expression | None:
        self.next_token()

        exp = self.parse_expression(Precedence.LOWEST)

        if not self.expect_peek(TokenType.RPAREN):
            return None

        return exp

    def parse_if_expression(self) -> ast.Expression:
        expression = ast.IfExpression(token=self.current_token)

        if not self.expect_peek(TokenType.LPAREN):
            return None

        self.next_token()
        expression.condition = self.parse_expression(Precedence.LOWEST)

        if not self.expect_peek(TokenType.RPAREN):
            return None

        if not self.expect_peek(TokenType.LSQUIRLY):
            return None

        expression.consequence = self.parse_block_statement()

        if self.peek_token_is(TokenType.ELSE):
            self.next_token()
            if not self.expect_peek(TokenType.LSQUIRLY):
                return None

            expression.Alternative = self.parse_block_statement()

        return expression

    def parse_block_statement(self) -> ast.BlockStatement:
        block = ast.BlockStatement(token=self.current_token, statements=[])

        self.next_token()
        while (
            self.current_token.type != TokenType.RSQUIRLY
            and self.current_token.type != TokenType.EOF
        ):
            stmt = self.parse_statement()
            if stmt is not None:
                block.statements.append(stmt)
            self.next_token()

        return block

    def parse_function_literal(self) -> ast.Expression:
        func = ast.FunctionLiteral(token=self.current_token, parameters=[])

        if not self.expect_peek(TokenType.LPAREN):
            return None

        func.parameters = self.parse_function_parameters()

        if not self.expect_peek(TokenType.LSQUIRLY):
            return None

        func.body = self.parse_block_statement()

        return func

    def parse_function_parameters(self) -> list[ast.Identifier]:
        identifiers: list[ast.Identifier] = []

        if self.peek_token_is(TokenType.RPAREN):
            self.next_token()
            return identifiers

        self.next_token()

        ident = ast.Identifier(
            token=self.current_token, value=self.current_token.literal
        )
        identifiers.append(ident)

        while self.peek_token_is(TokenType.COMMA):
            self.next_token()
            self.next_token()
            ident = ast.Identifier(
                token=self.current_token, value=self.current_token.literal
            )
            identifiers.append(ident)

        if not self.expect_peek(TokenType.RPAREN):
            return []

        return identifiers

    def parse_prefix_expression(self) -> ast.Expression:
        expression = ast.PrefixExpression(
            token=self.current_token, operator=self.current_token.literal
        )

        self.next_token()
        expression.right = self.parse_expression(Precedence.PREFIX)
        return expression

    def parse_call_expression(self, left: ast.Expression) -> ast.Expression:
        expression = ast.CallExpression(
            token=self.current_token, function=left, arguments=[]
        )
        expression.arguments = self.parse_call_arguments()
        return expression

    def parse_call_arguments(self) -> list[ast.Expression]:
        args: list[ast.Expression] = []

        if self.peek_token_is(TokenType.RPAREN):
            self.next_token()
            return args

        self.next_token()
        args.append(self.parse_expression(Precedence.LOWEST))

        while self.peek_token_is(TokenType.COMMA):
            self.next_token()
            self.next_token()
            args.append(self.parse_expression(Precedence.LOWEST))

        if not self.expect_peek(TokenType.RPAREN):
            return []

        return args

    def parse_infix_expression(self, left: ast.Expression) -> ast.Expression:
        expression = ast.InfixExpression(
            token=self.current_token,
            operator=self.current_token.literal,
            left=left,
        )

        precedence = self.current_precedence()
        self.next_token()
        expression.right = self.parse_expression(precedence)

        return expression

    def parse_statement(self) -> ast.Statement:
        match self.current_token.type:
            case TokenType.LET:
                return self.parse_let_statement()
            case TokenType.RETURN:
                return self.parse_return_statement()
            case _:
                return self.parse_expression_statement()

    def parse_let_statement(self) -> ast.LetStatement:
        stmt = ast.LetStatement(token=self.current_token)

        if not self.expect_peek(TokenType.IDENT):
            return None

        stmt.name = ast.Identifier(
            token=self.current_token, value=self.current_token.literal
        )

        if not self.expect_peek(TokenType.ASSING):
            return None

        self.next_token()
        stmt.value = self.parse_expression(Precedence.LOWEST)
        if self.peek_token_is(TokenType.SEMICOLON):
            self.next_token()

        return stmt

    def parse_return_statement(self) -> ast.ReturnStatement:
        stmt = ast.ReturnStatement(token=self.current_token)

        self.next_token()

        stmt.return_value = self.parse_expression(Precedence.LOWEST)

        if self.peek_token_is(TokenType.SEMICOLON):
            self.next_token()

        return stmt

    def parse_expression_statement(self) -> ast.ExpressionStatement:
        stmt = ast.ExpressionStatement(token=self.current_token)

        stmt.expression = self.parse_expression(Precedence.LOWEST)

        if self.peek_token_is(TokenType.SEMICOLON):
            self.next_token()

        return stmt

    def parse_expression(
        self, precedence: Precedence
    ) -> ast.Expression | None:
        if self.current_token.type not in self.prefix_parse_funcs:
            self.no_prefix_error(self.current_token.type)
            return None

        prefix = self.prefix_parse_funcs[self.current_token.type]

        left_exp = prefix()
        while (
            not self.peek_token_is(TokenType.SEMICOLON)
            and precedence < self.peek_precedence()
        ):
            if self.peek_token.type not in self.infix_parse_funcs:
                return left_exp
            infix = self.infix_parse_funcs[self.peek_token.type]
            self.next_token()
            left_exp = infix(left_exp)
        return left_exp

    def peek_precedence(self) -> Precedence:
        return token_type_to_precedence(self.peek_token.type)

    def current_precedence(self) -> Precedence:
        return token_type_to_precedence(self.current_token.type)

    def peek_token_is(self, type) -> bool:
        return self.peek_token.type == type

    def expect_peek(self, type) -> bool:
        if self.peek_token_is(type):
            self.next_token()
            return True
        self.peek_error(type)
        return False

    def peek_error(self, type) -> None:
        msg = "Expected next token to be %s, got %s instead" % (
            type,
            self.peek_token.type,
        )
        self.errors.append(msg)

    def no_prefix_error(self, type) -> None:
        msg = "No prefix parse function for %s found " % type
        self.errors.append(msg)
