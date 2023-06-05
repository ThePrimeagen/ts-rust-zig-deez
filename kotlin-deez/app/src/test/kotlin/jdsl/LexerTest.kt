package jdsl

import jdsl.TokenType.*
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class LexerTest {

    @Test
    fun testDeez() {
        val input = "{}(),;+="
        val lexer = Lexer(input)
        val tokens = lexer.readTokens()

        val expectedTokens = listOf(
            Token(LSQUIRLY, "{"),
            Token(RSQUIRLY, "}"),
            Token(LPAREN, "("),
            Token(RPAREN, ")"),
            Token(COMMA, ","),
            Token(SEMICOLON, ";"),
            Token(PLUS, "+"),
            Token(ASSIGN, "="),
            Token(EOF, ""),
        )

        for (i in tokens.indices) {
            val token = tokens[i]
            val expected = expectedTokens[i]
            assertEquals(expected, token)
        }
    }

    @Test
    fun testJDSL() {
        val input = """
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
        """.trimIndent()

        val lexer = Lexer(input)
        val tokens = lexer.readTokens()

        val expectedTokens = listOf(
            Token(LET, "let"),
            Token(IDENTIFIER, "five"),
            Token(ASSIGN, "="),
            Token(NUMBER, "5"),
            Token(SEMICOLON, ";"),
            Token(LET, "let"),
            Token(IDENTIFIER, "ten"),
            Token(ASSIGN, "="),
            Token(NUMBER, "10"),
            Token(SEMICOLON, ";"),
            Token(LET, "let"),
            Token(IDENTIFIER, "add"),
            Token(ASSIGN, "="),
            Token(FUNCTION, "fn"),
            Token(LPAREN, "("),
            Token(IDENTIFIER, "x"),
            Token(COMMA, ","),
            Token(IDENTIFIER, "y"),
            Token(RPAREN, ")"),
            Token(LSQUIRLY, "{"),
            Token(IDENTIFIER, "x"),
            Token(PLUS, "+"),
            Token(IDENTIFIER, "y"),
            Token(SEMICOLON, ";"),
            Token(RSQUIRLY, "}"),
            Token(SEMICOLON, ";"),
            Token(LET, "let"),
            Token(IDENTIFIER, "result"),
            Token(ASSIGN, "="),
            Token(IDENTIFIER, "add"),
            Token(LPAREN, "("),
            Token(IDENTIFIER, "five"),
            Token(COMMA, ","),
            Token(IDENTIFIER, "ten"),
            Token(RPAREN, ")"),
            Token(SEMICOLON, ";"),
            Token(BANG, "!"),
            Token(MINUS, "-"),
            Token(SLASH, "/"),
            Token(STAR, "*"),
            Token(NUMBER, "5"),
            Token(SEMICOLON, ";"),
            Token(NUMBER, "5"),
            Token(LESS, "<"),
            Token(NUMBER, "10"),
            Token(GREATER, ">"),
            Token(NUMBER, "5"),
            Token(SEMICOLON, ";"),
            Token(IF, "if"),
            Token(LPAREN, "("),
            Token(NUMBER, "5"),
            Token(LESS, "<"),
            Token(NUMBER, "10"),
            Token(RPAREN, ")"),
            Token(LSQUIRLY, "{"),
            Token(RETURN, "return"),
            Token(TRUE, "true"),
            Token(SEMICOLON, ";"),
            Token(RSQUIRLY, "}"),
            Token(ELSE, "else"),
            Token(LSQUIRLY, "{"),
            Token(RETURN, "return"),
            Token(FALSE, "false"),
            Token(SEMICOLON, ";"),
            Token(RSQUIRLY, "}"),
            Token(NUMBER, "10"),
            Token(EQ, "=="),
            Token(NUMBER, "10"),
            Token(SEMICOLON, ";"),
            Token(NUMBER, "10"),
            Token(NOT_EQ, "!="),
            Token(NUMBER, "9"),
            Token(SEMICOLON, ";"),
            Token(EOF, ""),
        )

        for (i in tokens.indices) {
            val token = tokens[i]
            val expected = expectedTokens[i]
            assertEquals(expected, token)
        }
    }
}