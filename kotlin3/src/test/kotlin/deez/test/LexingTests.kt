package deez.test

import deez.Token
import deez.TokenType
import deez.lexer
import kotlin.test.Test

class LexingTests {
    @Test
    fun testLexing() {
        val input = """
            let five = 5;
            let ten = 10;
            
            let add = fn(x, y) {
                x + y;
            };
            
            let result = add(five, ten);
        """.trimIndent()

        val expectedTokens = listOf(
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENTIFIER, "five"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.INTEGER, "5"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENTIFIER, "ten"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.INTEGER, "10"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENTIFIER, "add"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.FUNCTION, "fn"),
            Token(TokenType.LEFT_PAREN, "("),
            Token(TokenType.IDENTIFIER, "x"),
            Token(TokenType.COMMA, ","),
            Token(TokenType.IDENTIFIER, "y"),
            Token(TokenType.RIGHT_PAREN, ")"),
            Token(TokenType.LEFT_BRACE, "{"),
            Token(TokenType.IDENTIFIER, "x"),
            Token(TokenType.PLUS, "+"),
            Token(TokenType.IDENTIFIER, "y"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.RIGHT_BRACE, "}"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.LET, "let"),
            Token(TokenType.IDENTIFIER, "result"),
            Token(TokenType.ASSIGN, "="),
            Token(TokenType.IDENTIFIER, "add"),
            Token(TokenType.LEFT_PAREN, "("),
            Token(TokenType.IDENTIFIER, "five"),
            Token(TokenType.COMMA, ","),
            Token(TokenType.IDENTIFIER, "ten"),
            Token(TokenType.RIGHT_PAREN, ")"),
            Token(TokenType.SEMICOLON, ";"),
            Token(TokenType.EOF, ""),
        )

        val actualTokens = lexer(input).toList()
        assert(actualTokens == expectedTokens) { "Tokens do not match" }
    }
}