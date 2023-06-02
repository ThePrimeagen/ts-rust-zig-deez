import monkey.token_types as TokenType
from monkey.token import Token, FIXED_TOKENS, RESERVED_KEYWORDS


def is_letter(ch):
    return ch.isalpha() or ch == "_"


class Lexer:
    def __init__(self, input):
        self.input = input
        self.input_length = len(self.input)
        self.position = 0
        self.read_position = 0
        self.ch = ""
        self.read_char()

    def read_char(self) -> None:
        self.ch = self.peek_char()
        self.position = self.read_position
        self.read_position += 1

    def peek_char(self) -> str:
        if self.read_position >= self.input_length:
            return "\0"
        return self.input[self.read_position]

    def get_next_token(self) -> Token:
        self.skip_whitespace()

        # I dont wanna write if else for each character
        tok = None
        if self.ch in FIXED_TOKENS:
            tok = FIXED_TOKENS[self.ch]
        match tok:
            case Token(TokenType.ASSING):
                # PEEK
                if self.peek_char() == "=":
                    self.read_char()
                    tok = FIXED_TOKENS["=="]
            case Token(TokenType.BANG):
                # PEEK
                if self.peek_char() == "=":
                    self.read_char()
                    tok = FIXED_TOKENS["!="]
            case None:
                if is_letter(self.ch):
                    ident = self.read_ident()
                    if ident in RESERVED_KEYWORDS:
                        return RESERVED_KEYWORDS[ident]
                    return Token(TokenType.IDENT, ident)
                elif self.ch.isdigit():
                    return Token(TokenType.INT, self.read_int())
                tok = Token(TokenType.ILLEGAL, TokenType.ILLEGAL)

        self.read_char()
        return tok

    def read_int(self) -> str:
        pos = self.position

        while self.ch.isdigit():
            self.read_char()

        return self.input[pos : self.position]

    def read_ident(self) -> str:
        pos = self.position

        while is_letter(self.ch):
            self.read_char()

        return self.input[pos : self.position]

    def skip_whitespace(self):
        while self.ch.isspace():
            self.read_char()
