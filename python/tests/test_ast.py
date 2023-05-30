import unittest
from parser.my_ast import Program, LetStatement, Identifier
from lexer.my_token import Token, TokenType
# from parser import Program


class Ast_Test_String(unittest.TestCase):
    def test_string(self):

        program: Program = Program(
            Statements=[
                LetStatement(
                    Token=Token(Type=TokenType.LET, Literal="let"),
                    Name=Identifier(
                        Token=Token(Type=TokenType.IDENT, Literal="myVar"),
                        Value="myVar"
                    ),
                    Value=Identifier(
                        Token=Token(
                            Type=TokenType.IDENT,
                            Literal="anotherVar"),
                        Value="anotherVar"))
            ]
        )
        print(str(program))
        assert str(
            program) == "let myVar = anotherVar;", f"program.String() wrong. got={str(program)}"
