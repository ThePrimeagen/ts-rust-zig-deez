import unittest

from lexer import Lexer
import tokens as tok
from tokens import Token


class Test(unittest.TestCase):
    def test_get_next_token_1(self):
        input = "=+(){},;"

        tokens = [
            tok.ASSING,
            tok.PLUS,
            tok.LPAREN,
            tok.RPAREN,
            tok.LSQUIRLY,
            tok.RSQUIRLY,
            tok.COMMA,
            tok.SEMICOLON,
        ]

        lex = Lexer(input)
        for token in tokens:
            self.assertEqual(lex.get_next_token().type, token)

    def test_get_next_token_2(self):
        input = """\
let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;
if (5 < 10) {
return true;
} else {
return false;
}
"""
        lex = Lexer(input)
        tokens = [
            Token(tok.LET, "let"),
            Token(tok.IDENT, "five"),
            Token(tok.ASSING, "="),
            Token(tok.INT, "5"),
            Token(tok.SEMICOLON, ";"),
            Token(tok.LET, "let"),
            Token(tok.IDENT, "ten"),
            Token(tok.ASSING, "="),
            Token(tok.INT, "10"),
            Token(tok.SEMICOLON, ";"),
            Token(tok.LET, "let"),
            Token(tok.IDENT, "add"),
            Token(tok.ASSING, "="),
            Token(tok.FUNCTION, "fn"),
            Token(tok.LPAREN, "("),
            Token(tok.IDENT, "x"),
            Token(tok.COMMA, ","),
            Token(tok.IDENT, "y"),
            Token(tok.RPAREN, ")"),
            Token(tok.LSQUIRLY, "{"),
            Token(tok.IDENT, "x"),
            Token(tok.PLUS, "+"),
            Token(tok.IDENT, "y"),
            Token(tok.SEMICOLON, ";"),
            Token(tok.RSQUIRLY, "}"),
            Token(tok.SEMICOLON, ";"),
            Token(tok.LET, "let"),
            Token(tok.IDENT, "result"),
            Token(tok.ASSING, "="),
            Token(tok.IDENT, "add"),
            Token(tok.LPAREN, "("),
            Token(tok.IDENT, "five"),
            Token(tok.COMMA, ","),
            Token(tok.IDENT, "ten"),
            Token(tok.RPAREN, ")"),
            Token(tok.SEMICOLON, ";"),
            Token(tok.BANG, "!"),
            Token(tok.MINUS, "-"),
            Token(tok.SLASH, "/"),
            Token(tok.ASTERISK, "*"),
            Token(tok.INT, "5"),
            Token(tok.SEMICOLON, ";"),
            Token(tok.INT, "5"),
            Token(tok.LESSTHAN, "<"),
            Token(tok.INT, "10"),
            Token(tok.GREATERTHAN, ">"),
            Token(tok.INT, "5"),
            Token(tok.SEMICOLON, ";"),
            Token(tok.IF, "if"),
            Token(tok.LPAREN, "("),
            Token(tok.INT, "5"),
            Token(tok.LESSTHAN, "<"),
            Token(tok.INT, "10"),
            Token(tok.RPAREN, ")"),
            Token(tok.LSQUIRLY, "{"),
            Token(tok.RETURN, "return"),
            Token(tok.TRUE, "true"),
            Token(tok.SEMICOLON, ";"),
            Token(tok.RSQUIRLY, "}"),
            Token(tok.ELSE, "else"),
            Token(tok.LSQUIRLY, "{"),
            Token(tok.RETURN, "return"),
            Token(tok.FALSE, "false"),
            Token(tok.SEMICOLON, ";"),
            Token(tok.RSQUIRLY, "}"),
            Token(tok.EOF, "EOF"),
        ]
        for token in tokens:
            self.assertEqual(lex.get_next_token(), token)
