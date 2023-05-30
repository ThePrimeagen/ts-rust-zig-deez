from lexer.my_token import Token, TokenType
from lexer.lexer import Lexer
import unittest


class LexerTesting(unittest.TestCase):
    def testNextToken(self):
        test_input = "=+(){},;"

        tests: list[Token] = [
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.LBRACE, "{"),
            Token(TokenType.RBRACE, "}"),
            Token(TokenType.COMMA, ","),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.EOF, ""),
        ]

        lexer = Lexer(test_input)
        print("Starting")
        for tt in tests:
            tok = lexer.nextToken()

            # print(tok.Type, tt.Type)
            if tok.Type != tt.Type:
                assert tok.Type == tt.Type, f"Expected {tt.Type}, got {tok.Type}"
            if tok.Literal != tt.Literal:
                assert tok.Literal == tt.Literal, f"Expected {tt.Literal}, got {tok.Literal}"

    def testNextToken_complex(self):
        test_input = """let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);"""

        tests: list[Token] = [
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENT, "five"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.INT, "5"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENT, "ten"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.INT, "10"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENT, "add"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.FUNCTION, "fn"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.IDENT, "x"),
            Token(TokenType.COMMA, ","),
            Token(TokenType.IDENT, "y"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.LBRACE, "{"),
            Token(TokenType.IDENT, "x"),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.IDENT, "y"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.RBRACE, "}"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENT, "result"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.IDENT, "add"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.IDENT, "five"),
            Token(TokenType.COMMA, ","),
            Token(TokenType.IDENT, "ten"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.EOF, ""),
        ]
        print("Starting 2")
        lexer = Lexer(test_input)
        for tt in tests:
            tok = lexer.nextToken()
            if tok.Type != tt.Type:
                assert tok.Type == tt.Type, f"Expected {tt.Type}, got {tok.Type}"
            if tok.Literal != tt.Literal:
                assert tok.Literal == tt.Literal, f"Expected {tt.Literal}, got {tok.Literal}"

    def testNextToken_new_operators(self):
        test_input = """let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;
10 == 10;
10 != 9;"""

        tests: list[Token] = [
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENT, "five"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.INT, "5"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENT, "ten"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.INT, "10"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENT, "add"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.FUNCTION, "fn"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.IDENT, "x"),
            Token(TokenType.COMMA, ","),
            Token(TokenType.IDENT, "y"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.LBRACE, "{"),
            Token(TokenType.IDENT, "x"),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.IDENT, "y"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.RBRACE, "}"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENT, "result"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.IDENT, "add"),
            Token(TokenType.LPAREN, "("),
            Token(TokenType.IDENT, "five"),
            Token(TokenType.COMMA, ","),
            Token(TokenType.IDENT, "ten"),
            Token(TokenType.RPAREN, ")"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.BANG, "!"),
            Token(TokenType.MINUS, "-"),
            Token(TokenType.SLASH, "/"),
            Token(TokenType.ASTERISK, "*"),
            Token(TokenType.INT, "5"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.INT, "5"),
            Token(TokenType.LT, "<"),
            Token(TokenType.INT, "10"),
            Token(TokenType.GT, ">"),
            Token(TokenType.INT, "5"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.INT, "10"),
            Token(TokenType.EQ, "=="),
            Token(TokenType.INT, "10"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.INT, "10"),
            Token(TokenType.NOT_EQ, "!="),
            Token(TokenType.INT, "9"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.EOF, ""),
        ]

        print("Starting 3")
        lexer = Lexer(test_input)
        for tt in tests:
            tok = lexer.nextToken()
            # print(i, tok.Type, tt.Type)
            if tok.Type != tt.Type:
                assert tok.Type == tt.Type, f"Expected {tt.Type}, got {tok.Type}"
            if tok.Literal != tt.Literal:
                assert tok.Literal == tt.Literal, f"Expected {tt.Literal}, got {tok.Literal}"
