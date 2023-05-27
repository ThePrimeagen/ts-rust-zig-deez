import tokens
from tokens import Token, FIXED_TOKENS, RESERVED_KEYWORDS


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
    
    def new_token(self,a,b):
        return Token(a,b)

    def get_next_token(self) -> Token:
        self.skip_whitespace()

        # I dont wanna write if else for each character
        tok = FIXED_TOKENS.get(self.ch,None)
        match tok:
            case Token(tokens.ASSING):
                #PEEK
                if self.peek_char() == "=":
                    tok = FIXED_TOKENS.get("==")
            case Token(tokens.BANG):
                #PEEK
                if self.peek_char() == "=":
                    tok = FIXED_TOKENS.get("!=")
            case None:
                if is_letter(self.ch):
                    indent = self.read_ident()
                    tok = RESERVED_KEYWORDS.get(indent,Token(tokens.IDENT,indent))
                    return tok
                elif self.ch.isdigit():
                    return Token(tokens.INT, self.read_int())
                tok = Token(tokens.ILLEGAL,tokens.ILLEGAL)
        
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
