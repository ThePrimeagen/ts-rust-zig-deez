from tokens import Token, TokenType, char_to_token_type


keywords = {
    'fn' : Token(TokenType.Function, 'fn'),
    'let': Token(TokenType.Let, 'let')
}

def is_letter(ch):
    return ch.isalpha() or ch == '_'

class Lexer:
    def __init__(self, input):
        self.input = input
        self.input_length = len(self.input)
        self.position = 0
        self.read_position = 0
        self.ch = ''
        self.read_char()

    def read_char(self) -> None:
        if self.read_position >=  self.input_length:
            self.ch = '\0'
        else:
            self.ch = self.input[self.read_position]
        
        self.position = self.read_position
        self.read_position += 1
        

    def get_next_token(self) -> Token:
        self.skip_whitespace()
        
        # I dont wanna write if else for each character
        tok_type = char_to_token_type.get(self.ch, None)
        if tok_type is not None:
            tok = Token(tok_type, tok_type.value)
            self.read_char()
            return tok

        if is_letter(self.ch):
            indent = self.read_ident()
            return keywords.get(indent, Token(TokenType.Ident, indent))
        
        if self.ch.isdigit():
            return Token(TokenType.Int, self.read_int())
        
        return Token(TokenType.Illegal, self.ch)


    def read_int(self) -> str:
        pos = self.position
        
        while self.ch.isdigit():
            self.read_char()
        
        return self.input[pos:self.position]

    def read_ident(self) -> str:
        pos = self.position
        
        while is_letter(self.ch):
            self.read_char()

        return self.input[pos:self.position]

    def skip_whitespace(self):
        while(self.ch.isspace()):
            self.read_char()