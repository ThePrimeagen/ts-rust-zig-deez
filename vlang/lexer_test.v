module main

import unittest
import lexer
import tokens

struct Test {
    unittest.TestCase
}

fn (t Test) test_get_next_token_1() {
    input := "=+(){},;"
    tokens := [
        tokens.Token{type: tokens.TokenType.Equal},
        tokens.Token{type: tokens.TokenType.Plus},
        tokens.Token{type: tokens.TokenType.LParen},
        tokens.Token{type: tokens.TokenType.RParen},
        tokens.Token{type: tokens.TokenType.LSquirly},
        tokens.Token{type: tokens.TokenType.RSquirly},
        tokens.Token{type: tokens.TokenType.Comma},
        tokens.Token{type: tokens.TokenType.Semicolon},
    ]

    lex := lexer.Lexer{input: input}
    for token in tokens {
        t.assert_eq(lex.get_next_token().type, token.type)
    }
}

fn (t Test) test_get_next_token_2() {
    input := '''
let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
'''
    lex := lexer.Lexer{input: input}
    tokens := [
        tokens.Token{type: tokens.TokenType.Let, literal: "let"},
        tokens.Token{type: tokens.TokenType.Ident, literal: "five"},
        tokens.Token{type: tokens.TokenType.Equal, literal: "="},
        tokens.Token{type: tokens.TokenType.Int, literal: "5"},
        tokens.Token{type: tokens.TokenType.Semicolon, literal: ";"},
        tokens.Token{type: tokens.TokenType.Let, literal: "let"},
        tokens.Token{type: tokens.TokenType.Ident, literal: "ten"},
        tokens.Token{type: tokens.TokenType.Equal, literal: "="},
        tokens.Token{type: tokens.TokenType.Int, literal: "10"},
        tokens.Token{type: tokens.TokenType.Semicolon, literal: ";"},
        tokens.Token{type: tokens.TokenType.Let, literal: "let"},
        tokens.Token{type: tokens.TokenType.Ident, literal: "add"},
        tokens.Token{type: tokens.TokenType.Equal, literal: "="},
        tokens.Token{type: tokens.TokenType.Function, literal: "fn"},
        tokens.Token{type: tokens.TokenType.LParen, literal: "("},
        tokens.Token{type: tokens.TokenType.Ident, literal: "x"},
        tokens.Token{type: tokens.TokenType.Comma, literal: ","},
        tokens.Token{type: tokens.TokenType.Ident, literal: "y"},
        tokens.Token{type: tokens.TokenType.RParen, literal: ")"},
        tokens.Token{type: tokens.TokenType.LSquirly, literal: "{"},
        tokens.Token{type: tokens.TokenType.Ident, literal: "x"},
        tokens.Token{type: tokens.TokenType.Plus, literal: "+"},
        tokens.Token{type: tokens.TokenType.Ident, literal: "y"},
        tokens.Token{type: tokens.TokenType.Semicolon, literal: ";"},
        tokens.Token{type: tokens.TokenType.RSquirly, literal: "}"},
        tokens.Token{type: tokens.TokenType.Semicolon, literal: ";"},
        tokens.Token{type: tokens.TokenType.Let, literal: "let"},
        tokens.Token{type: tokens.TokenType.Ident, literal: "result"},
        tokens.Token{type: tokens.TokenType.Equal, literal: "="},
        tokens.Token{type: tokens.TokenType.Ident, literal: "add"},
        tokens.Token{type: tokens.TokenType.LParen, literal: "("},
        tokens.Token{type: tokens.TokenType.Ident, literal: "five"},
        tokens.Token{type: tokens.TokenType.Comma, literal: ","},
        tokens.Token{type: tokens.TokenType.Ident, literal: "ten"},
        tokens.Token{type: tokens.TokenType.RParen, literal: ")"},
        tokens.Token{type: tokens.TokenType.Semicolon, literal: ";"},
        tokens.Token{type: tokens.TokenType.Eof, literal: "EOF"},
    ]
    for token in tokens {
        t.assert_eq(lex.get_next_token(), token)
    }
}

fn main() {
    test := Test{}
    test.test_get_next_token_1()
    test.test_get_next_token_2()
    test.TestCase.run_tests()
}
