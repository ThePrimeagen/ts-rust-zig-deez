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
            Token(LSQUIRLY),
            Token(RSQUIRLY),
            Token(LPAREN),
            Token(RPAREN),
            Token(COMMA),
            Token(SEMICOLON),
            Token(PLUS),
            Token(EQUAL),
            Token(EOF, null),
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
        """.trimIndent()

        val lexer = Lexer(input)
        val tokens = lexer.readTokens()

        val expectedTokens = listOf(
            Token(LET, "let"),
            Token(IDENTIFIER, "five"),
            Token(EQUAL),
            Token(NUMBER, "5"),
            Token(SEMICOLON),
            Token(LET, "let"),
            Token(IDENTIFIER, "ten"),
            Token(EQUAL),
            Token(NUMBER, "10"),
            Token(SEMICOLON),
            Token(LET, "let"),
            Token(IDENTIFIER, "add"),
            Token(EQUAL),
            Token(FUNCTION, "func"),
            Token(LPAREN),
            Token(IDENTIFIER, "x"),
            Token(COMMA),
            Token(IDENTIFIER, "y"),
            Token(RPAREN),
            Token(LSQUIRLY),
            Token(IDENTIFIER, "x"),
            Token(PLUS),
            Token(IDENTIFIER, "y"),
            Token(SEMICOLON),
            Token(RSQUIRLY),
            Token(SEMICOLON),
            Token(LET, "let"),
            Token(IDENTIFIER, "result"),
            Token(EQUAL),
            Token(IDENTIFIER, "add"),
            Token(LPAREN),
            Token(IDENTIFIER, "five"),
            Token(COMMA),
            Token(IDENTIFIER, "ten"),
            Token(RPAREN),
            Token(SEMICOLON),
            Token(EOF )
        )

        for (i in tokens.indices) {
            val token = tokens[i]
            val expected = expectedTokens[i]
            assertEquals(expected, token)
        }
    }
}