from typing import Callable
from enum import Enum, auto
import monkey.ast as ast
import monkey.token_types as TokenType
from monkey.lexer import Lexer
from monkey.token import Token

prefix_parse_func = Callable[[], ast.Expression]
infix_parse_func = Callable[[ast.Expression], ast.Expression]

class Precedence(Enum):
    LOWEST = auto()
    EQUALS = auto()  # ==
    LESSGREATER = auto()  # > or <
    SUM = auto()  # +
    PRODUCT = auto()  # *
    PREFIX = auto()  # -X or !X
    CALL = auto()  # myFunction(X)

class Parser:
    def __init__(self, lexer: Lexer) -> None:
        self.lexer: Lexer = lexer
        self.current_token: Token = None
        self.peek_token: Token = None
        self.errors: list[str] = []

        self.prefix_parse_funcs : dict[str, prefix_parse_func] = {}
        self.infix_parse_funcs : dict[str, infix_parse_func] = {}

        self.register_prefix(TokenType.IDENT, self.parse_identifier)
        self.register_prefix(TokenType.INT, self.parse_integer_literal)
        self.register_prefix(TokenType.BANG, self.parse_prefix_expression)
        self.register_prefix(TokenType.MINUS, self.parse_prefix_expression)

        self.next_token()
        self.next_token()

    def next_token(self) -> None:
        self.current_token = self.peek_token
        self.peek_token = self.lexer.get_next_token()

    def register_prefix(self, token_type, func : prefix_parse_func) -> None:
        self.prefix_parse_funcs[token_type] = func
    
    def register_infix(self, token_type, func : infix_parse_func) -> None:
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
        return ast.Identifier(token=self.current_token, value= self.current_token.literal)
    
    def parse_integer_literal(self) -> ast.Expression | None:
        int_lit = ast.IntegerLiteral(token=self.current_token)
        if not self.current_token.literal.isdigit():
            msg = f"Could not parse {self.current_token.literal} as digit"
            self.errors.append(msg)
            return None

        int_lit.value = int(self.current_token.literal)
        return int_lit
    
    def parse_prefix_expression(self) -> ast.Expression:
        expression = ast.PrefixExpression(token=self.current_token, operator= self.current_token.literal)

        self.next_token()
        expression.right = self.parse_expression(Precedence.PREFIX)
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
        
        stmt.name = ast.Identifier(token= self.current_token, value= self.current_token.literal)

        if not self.expect_peek(TokenType.ASSING):
            return None
        
        # Skip until semicolon
        while self.current_token.type != TokenType.SEMICOLON:
            self.next_token()

        return stmt
    
    def parse_return_statement(self) -> ast.ReturnStatement:
        stmt = ast.ReturnStatement(token=self.current_token)
        
        self.next_token()
        while self.current_token.type != TokenType.SEMICOLON:
            self.next_token()
        
        return stmt
    
    def parse_expression_statement(self) -> ast.ExpressionStatement:
        stmt = ast.ExpressionStatement(token=self.current_token)

        stmt.expression = self.parse_expression(Precedence.LOWEST)

        self.expect_peek(TokenType.SEMICOLON)

        return stmt
    
    def parse_expression(self, precedence: Precedence) -> ast.Expression:
        prefix = self.prefix_parse_funcs.get(self.current_token.type, None)

        if not prefix:
            return None
        
        left_exp = prefix()
        return left_exp

    
    def peek_token_is(self, type ) -> bool:
        return self.peek_token.type == type

    def expect_peek(self,type) -> bool:
        if self.peek_token_is(type):
            self.next_token()
            return True
        self.peek_error(type)
        return False
    
    def peek_error(self, type) -> None:
        msg = "Expected next token to be %s, got %s instead" % (type, self.peek_token.type)
        self.errors.append(msg)

