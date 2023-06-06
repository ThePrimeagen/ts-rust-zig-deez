from deez_py import (
    Lexer,
)

input_str = "=+(){},;"
lex = Lexer(input_str)
for c in input_str:
    print(lex.get_next_token())

print(lex.get_next_token())
