import pytest

from deez_py import (
    Lexer,
    Token,
    TokenType,
    __version__,
)


def test_version():
    assert __version__ == "0.1.6"


def test_get_next_token_single_character_tokens():
    input_str = "=+(){},;"
    tokens = [
        TokenType.ASSING,
        TokenType.PLUS,
        TokenType.LPAREN,
        TokenType.RPAREN,
        TokenType.LSQUIRLY,
        TokenType.RSQUIRLY,
        TokenType.COMMA,
        TokenType.SEMICOLON,
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token().type == token


def test_get_next_token_complex_program():
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

        10 == 10;
        10 != 9;
    """
    lex = Lexer(input)
    tokens = [
        Token(TokenType.LET, "let"),
        Token(TokenType.IDENT, "five"),
        Token(TokenType.ASSING, "="),
        Token(TokenType.INT, "5"),
        Token(TokenType.SEMICOLON, ";"),
        Token(TokenType.LET, "let"),
        Token(TokenType.IDENT, "ten"),
        Token(TokenType.ASSING, "="),
        Token(TokenType.INT, "10"),
        Token(TokenType.SEMICOLON, ";"),
        Token(TokenType.LET, "let"),
        Token(TokenType.IDENT, "add"),
        Token(TokenType.ASSING, "="),
        Token(TokenType.FUNCTION, "fn"),
        Token(TokenType.LPAREN, "("),
        Token(TokenType.IDENT, "x"),
        Token(TokenType.COMMA, ","),
        Token(TokenType.IDENT, "y"),
        Token(TokenType.RPAREN, ")"),
        Token(TokenType.LSQUIRLY, "{"),
        Token(TokenType.IDENT, "x"),
        Token(TokenType.PLUS, "+"),
        Token(TokenType.IDENT, "y"),
        Token(TokenType.SEMICOLON, ";"),
        Token(TokenType.RSQUIRLY, "}"),
        Token(TokenType.SEMICOLON, ";"),
        Token(TokenType.LET, "let"),
        Token(TokenType.IDENT, "result"),
        Token(TokenType.ASSING, "="),
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
        Token(TokenType.LESSTHAN, "<"),
        Token(TokenType.INT, "10"),
        Token(TokenType.GREATERTHAN, ">"),
        Token(TokenType.INT, "5"),
        Token(TokenType.SEMICOLON, ";"),
        Token(TokenType.IF, "if"),
        Token(TokenType.LPAREN, "("),
        Token(TokenType.INT, "5"),
        Token(TokenType.LESSTHAN, "<"),
        Token(TokenType.INT, "10"),
        Token(TokenType.RPAREN, ")"),
        Token(TokenType.LSQUIRLY, "{"),
        Token(TokenType.RETURN, "return"),
        Token(TokenType.TRUE, "true"),
        Token(TokenType.SEMICOLON, ";"),
        Token(TokenType.RSQUIRLY, "}"),
        Token(TokenType.ELSE, "else"),
        Token(TokenType.LSQUIRLY, "{"),
        Token(TokenType.RETURN, "return"),
        Token(TokenType.FALSE, "false"),
        Token(TokenType.SEMICOLON, ";"),
        Token(TokenType.RSQUIRLY, "}"),
        Token(TokenType.INT, "10"),
        Token(TokenType.EQUAL, "=="),
        Token(TokenType.INT, "10"),
        Token(TokenType.SEMICOLON, ";"),
        Token(TokenType.INT, "10"),
        Token(TokenType.NOTEQUAL, "!="),
        Token(TokenType.INT, "9"),
        Token(TokenType.SEMICOLON, ";"),
        Token(TokenType.EOF, "EOF"),
    ]
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token():
    input_str = "=+(){},;"
    tokens = [
        Token(TokenType.ASSING, "="),
        Token(TokenType.PLUS, "+"),
        Token(TokenType.LPAREN, "("),
        Token(TokenType.RPAREN, ")"),
        Token(TokenType.LSQUIRLY, "{"),
        Token(TokenType.RSQUIRLY, "}"),
        Token(TokenType.COMMA, ","),
        Token(TokenType.SEMICOLON, ";"),
        Token(TokenType.EOF, "EOF"),
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token_with_keywords():
    input_str = "let fn"
    tokens = [
        Token(TokenType.LET, "let"),
        Token(TokenType.FUNCTION, "fn"),
        Token(TokenType.EOF, "EOF"),
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token_with_identifiers_and_integers():
    input_str = "x123 456"
    tokens = [
        Token(TokenType.IDENT, "x"),
        Token(TokenType.INT, "123"),
        Token(TokenType.INT, "456"),
        Token(TokenType.EOF, "EOF"),
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token_with_whitespace():
    input_str = "let    x  =  5  ;"
    tokens = [
        Token(TokenType.LET, "let"),
        Token(TokenType.IDENT, "x"),
        Token(TokenType.ASSING, "="),
        Token(TokenType.INT, "5"),
        Token(TokenType.SEMICOLON, ";"),
        Token(TokenType.EOF, "EOF"),
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token_with_multiple_digits():
    input_str = "123 456 789"
    tokens = [
        Token(TokenType.INT, "123"),
        Token(TokenType.INT, "456"),
        Token(TokenType.INT, "789"),
        Token(TokenType.EOF, "EOF"),
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token_with_mixed_tokens():
    input_str = "let x = 10 + y;"
    tokens = [
        Token(TokenType.LET, "let"),
        Token(TokenType.IDENT, "x"),
        Token(TokenType.ASSING, "="),
        Token(TokenType.INT, "10"),
        Token(TokenType.PLUS, "+"),
        Token(TokenType.IDENT, "y"),
        Token(TokenType.SEMICOLON, ";"),
        Token(TokenType.EOF, "EOF"),
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token_with_unknown_token():
    input_str = "^"
    tokens = [
        Token(TokenType.ILLEGAL, "ILLEGAL"),
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token


def test_get_next_token_empty_input():
    input_str = ""
    tokens = [
        Token(TokenType.EOF, "EOF"),
    ]

    lex = Lexer(input_str)
    for token in tokens:
        assert lex.get_next_token() == token
