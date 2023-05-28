import pytest

from deez_py import (
    __version__,
    Lexer,
    Token,
    TokenType
)


def test_version():
    assert __version__ == "0.1.3"


def test_get_next_token_single_character_tokens():
    input_str = "=+(){},;"
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

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token().type == token


def test_get_next_token_complex_program():
    input_str = """\
        let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);
    """
    lex = Lexer(input_str)
    tokens = [
        Token(TokenType.Let, "let"),
        Token(TokenType.Ident, "five"),
        Token(TokenType.Equal, "="),
        Token(TokenType.Int, "5"),
        Token(TokenType.Semicolon, ";"),
        Token(TokenType.Let, "let"),
        Token(TokenType.Ident, "ten"),
        Token(TokenType.Equal, "="),
        Token(TokenType.Int, "10"),
        Token(TokenType.Semicolon, ";"),
        Token(TokenType.Let, "let"),
        Token(TokenType.Ident, "add"),
        Token(TokenType.Equal, "="),
        Token(TokenType.Function, "fn"),
        Token(TokenType.LParen, "("),
        Token(TokenType.Ident, "x"),
        Token(TokenType.Comma, ","),
        Token(TokenType.Ident, "y"),
        Token(TokenType.RParen, ")"),
        Token(TokenType.LSquirly, "{"),
        Token(TokenType.Ident, "x"),
        Token(TokenType.Plus, "+"),
        Token(TokenType.Ident, "y"),
        Token(TokenType.Semicolon, ";"),
        Token(TokenType.RSquirly, "}"),
        Token(TokenType.Semicolon, ";"),
        Token(TokenType.Let, "let"),
        Token(TokenType.Ident, "result"),
        Token(TokenType.Equal, "="),
        Token(TokenType.Ident, "add"),
        Token(TokenType.LParen, "("),
        Token(TokenType.Ident, "five"),
        Token(TokenType.Comma, ","),
        Token(TokenType.Ident, "ten"),
        Token(TokenType.RParen, ")"),
        Token(TokenType.Semicolon, ";"),
        Token(TokenType.Eof, "EOF"),
    ]
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token():
    input_str = "=+(){},;"
    tokens = [
        Token(TokenType.Equal, "="),
        Token(TokenType.Plus, "+"),
        Token(TokenType.LParen, "("),
        Token(TokenType.RParen, ")"),
        Token(TokenType.LSquirly, "{"),
        Token(TokenType.RSquirly, "}"),
        Token(TokenType.Comma, ","),
        Token(TokenType.Semicolon, ";"),
        Token(TokenType.Eof, "EOF"),
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token_with_keywords():
    input_str = "let fn"
    tokens = [
        Token(TokenType.Let, "let"),
        Token(TokenType.Function, "fn"),
        Token(TokenType.Eof, "EOF"),
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token_with_illegal_characters():
    input_str = "!@#$%^&*"
    tokens = [
        Token(TokenType.Illegal, "!"),
    ] * 9

    lex = Lexer(input_str)
    for token in tokens:
        print(lex.get_next_token())
        assert lex.get_next_token() == token


def test_get_next_token_with_identifiers_and_integers():
    input_str = "x123 456"
    tokens = [
        Token(TokenType.Ident, "x"),
        Token(TokenType.Int, "123"),
        Token(TokenType.Int, "456"),
        Token(TokenType.Eof, "EOF"),
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token_with_whitespace():
    input_str = "let    x  =  5  ;"
    tokens = [
        Token(TokenType.Let, "let"),
        Token(TokenType.Ident, "x"),
        Token(TokenType.Equal, "="),
        Token(TokenType.Int, "5"),
        Token(TokenType.Semicolon, ";"),
        Token(TokenType.Eof, "EOF"),
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token_with_multiple_digits():
    input_str = "123 456 789"
    tokens = [
        Token(TokenType.Int, "123"),
        Token(TokenType.Int, "456"),
        Token(TokenType.Int, "789"),
        Token(TokenType.Eof, "EOF"),
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token_with_mixed_tokens():
    input_str = "let x = 10 + y;"
    tokens = [
        Token(TokenType.Let, "let"),
        Token(TokenType.Ident, "x"),
        Token(TokenType.Equal, "="),
        Token(TokenType.Int, "10"),
        Token(TokenType.Plus, "+"),
        Token(TokenType.Ident, "y"),
        Token(TokenType.Semicolon, ";"),
        Token(TokenType.Eof, "EOF"),
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token_with_unknown_token():
    input_str = "^"
    tokens = [
        Token(TokenType.Illegal, "^"),
    ] * 2

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token_empty_input():
    input_str = ""
    tokens = [
        Token(TokenType.Eof, "EOF"),
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token
