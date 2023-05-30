from lexer.my_token import TokenType, keywords, token_dict, Token
from dataclasses import dataclass


@dataclass
class Lexer:
    input: str
    position: int = 0
    readPosition: int = 0
    ch: str = ""
    stop: int = 1

    def __init__(self, input: str) -> None:
        self.input = input
        self.readChar()

    def peekChar(self):
        if self.readPosition >= len(self.input):
            return 0
        else:
            return self.input[self.readPosition]

    def readChar(self):
        if self.readPosition >= len(self.input):
            self.stop = 0
            self.ch = ""
        else:
            self.ch = self.input[self.readPosition]
        self.position = self.readPosition
        self.readPosition += 1

    def skipWhitespace(self):
        while self.ch.isspace():
            self.readChar()

    def nextToken(self) -> Token:
        self.skipWhitespace()
        if self.stop == 0:
            tok = Token(TokenType.EOF, "")
        elif token_dict.get(self.ch, False):
            tok = Token(token_dict[self.ch], self.ch)
        elif self.ch == '=':
            if self.peekChar() == '=':
                self.readChar()
                tok = Token(TokenType.EQ, '==')
            else:
                tok = Token(TokenType.ASSIGN, self.ch)
        elif self.ch == '!':
            if self.peekChar() == '=':
                self.readChar()
                tok = Token(TokenType.NOT_EQ, '!=')
            else:
                tok = Token(TokenType.BANG, self.ch)

        elif self.ch.isalpha():
            literal = self.readIdentifier()
            return Token(lookUpIdent(literal), literal)
        elif self.ch.isnumeric():
            tok_type = TokenType.INT
            literal = self.readNumber()
            return Token(tok_type, literal)
        else:
            tok = Token(TokenType.ILLEGAL, self.ch)
        self.readChar()
        return tok

    def readIdentifier(self):
        position = self.position

        while isLetter(self.ch):
            self.readChar()
        return self.input[position:self.position]

    def readNumber(self):
        position = self.position
        while isDigit(self.ch):
            self.readChar()

        return self.input[position:self.position]


def isLetter(ch: str):
    return 'a' <= ch <= 'z' or 'A' <= ch <= 'Z' or ch == '_'


def isDigit(ch: str):
    return '0' <= ch <= '9'


def lookUpIdent(ident) -> TokenType:
    return keywords[ident] if ident in keywords else TokenType.IDENT
