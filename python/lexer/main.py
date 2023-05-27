from lexer import Lexer

if __name__ == '__main__':
    input = '=+(){},;'
    lex = Lexer(input)
    for c in input:
        print(lex.get_next_token())

    print(lex.get_next_token())