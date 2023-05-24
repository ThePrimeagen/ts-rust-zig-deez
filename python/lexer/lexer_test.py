import unittest

from lexer import Lexer
from tokens import Token, TokenType


class Test(unittest.TestCase):
    def test_get_next_token_1(self):
        input = '=+(){},;'
        
        tokens = [
        TokenType.Equal,
        TokenType.Plus,
        TokenType.LParen,
        TokenType.RParen,
        TokenType.LSquirly,
        TokenType.RSquirly,
        TokenType.Comma,
        TokenType.Semicolon,
        ]

        lex = Lexer(input)
        for token in tokens:
            self.assertEqual(lex.get_next_token().type, token)

    def test_get_next_token_2(self):
        input ='''\
let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
'''
        lex = Lexer(input)
        tokens = [
        Token(TokenType.Let, "let"),
        Token(TokenType.Ident, "five" ),
        Token(TokenType.Equal, "="),
        Token(TokenType.Int, "5" ),
        Token(TokenType.Semicolon, ";"),
        Token(TokenType.Let, "let"),
        Token(TokenType.Ident, "ten" ),
        Token(TokenType.Equal, "="),
        Token(TokenType.Int, "10" ),
        Token(TokenType.Semicolon, ";"),
        Token(TokenType.Let, "let"),
        Token(TokenType.Ident, "add" ),
        Token(TokenType.Equal, "="),
        Token(TokenType.Function, "fn"),
        Token(TokenType.LParen, "("),
        Token(TokenType.Ident, "x" ),
        Token(TokenType.Comma, ","),
        Token(TokenType.Ident, "y" ),
        Token(TokenType.RParen, ")"),
        Token(TokenType.LSquirly, "{"),
        Token(TokenType.Ident, "x" ),
        Token(TokenType.Plus, "+"),
        Token(TokenType.Ident, "y" ),
        Token(TokenType.Semicolon, ";"),
        Token(TokenType.RSquirly, "}"),
        Token(TokenType.Semicolon, ";"),
        Token(TokenType.Let, "let"),
        Token(TokenType.Ident, "result" ),
        Token(TokenType.Equal, "="),
        Token(TokenType.Ident, "add" ),
        Token(TokenType.LParen, "("),
        Token(TokenType.Ident, "five" ),
        Token(TokenType.Comma, ","),
        Token(TokenType.Ident, "ten" ),
        Token(TokenType.RParen, ")"),
        Token(TokenType.Semicolon, ";"),
        Token(TokenType.Eof, "EOF"),
        ] 
        for token in tokens:
            self.assertEqual(lex.get_next_token(), token)