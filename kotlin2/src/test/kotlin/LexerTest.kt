package dev.hermannm.monkeylang

import kotlin.test.Test
import kotlin.test.assertEquals

class LexerTest {
    @Test
    fun `Test single-character symbols`() {
        val input = "=+(){},;"
        val lexer = Lexer(input)

        val tokens = listOf(
            Token.Equals,
            Token.Plus,
            Token.LeftParen,
            Token.RightParen,
            Token.LeftBrace,
            Token.RightBrace,
            Token.Comma,
            Token.Semicolon
        )

        for (token in tokens) {
            val nextToken = lexer.nextToken()
            assertEquals(token, nextToken)
        }
    }

    @Test
    fun `Test sample with identifiers`() {
        val input = """
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
        """.trimIndent()

        val lexer = Lexer(input)

        val tokens = listOf(
            Token.Let,
            Token.Identifier("five"),
            Token.Equals,
            Token.Integer("5"),
            Token.Semicolon,
            Token.Let,
            Token.Identifier("ten"),
            Token.Equals,
            Token.Integer("10"),
            Token.Semicolon,
            Token.Let,
            Token.Identifier("add"),
            Token.Equals,
            Token.Function,
            Token.LeftParen,
            Token.Identifier("x"),
            Token.Comma,
            Token.Identifier("y"),
            Token.RightParen,
            Token.LeftBrace,
            Token.Identifier("x"),
            Token.Plus,
            Token.Identifier("y"),
            Token.Semicolon,
            Token.RightBrace,
            Token.Semicolon,
            Token.Let,
            Token.Identifier("result"),
            Token.Equals,
            Token.Identifier("add"),
            Token.LeftParen,
            Token.Identifier("five"),
            Token.Comma,
            Token.Identifier("ten"),
            Token.RightParen,
            Token.Semicolon,
            Token.EndOfFile
        )

        for (token in tokens) {
            val nextToken = lexer.nextToken()
            assertEquals(token, nextToken)
        }
    }
}
