import monkey.ast as ast
import monkey.token_types as TokenType
from monkey.lexer import Lexer
from monkey.token import Token


class Parser:
    def __init__(self, lexer: Lexer) -> None:
        self.lexer: Lexer = lexer
        self.current_token: Token = None
        self.peek_token: Token = None
        self.errors: list[str] = []
        self.next_token()
        self.next_token()

    def next_token(self) -> None:
        self.current_token = self.peek_token
        self.peek_token = self.lexer.get_next_token()
    
    def parse_program(self) -> ast.Program:
        program = ast.Program()

        while self.current_token.type != TokenType.EOF:
            stmt = self.parse_statement()
            if stmt:
                program.statements.append(stmt)
            self.next_token()
        
        return program

    
    def parse_statement(self) -> ast.Statement:
        match self.current_token.type:
            case TokenType.LET:
                return self.parse_let_statement()
            case _:
                return None
            
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

