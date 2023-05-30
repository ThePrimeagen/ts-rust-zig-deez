from lexer.my_token import TokenType, Token
from lexer.lexer import Lexer
PROMPT = ">> "


def Start():
    while True:
        print(PROMPT, end="")
        scanned = input()

        if scanned == "":
            continue

        lexer = Lexer(scanned)
        print(scanned)
        while True:
            tok: Token = lexer.nextToken()
            print(tok)
            if tok.Type == TokenType.EOF:
                break
