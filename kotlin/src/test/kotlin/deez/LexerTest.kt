package deez

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class LexerTest {

    @Test
    fun testSpecialCharacters() {
        val input = "{}(),;+="
        val lexer = Lexer(input)

        val expectedTokens = listOf(
            Lexer.Token(Lexer.Token.Type.LEFT_BRACE),
            Lexer.Token(Lexer.Token.Type.RIGHT_BRACE),
            Lexer.Token(Lexer.Token.Type.LEFT_PARENTHESES),
            Lexer.Token(Lexer.Token.Type.RIGHT_PARENTHESES),
            Lexer.Token(Lexer.Token.Type.COMMA),
            Lexer.Token(Lexer.Token.Type.SEMICOLON),
            Lexer.Token(Lexer.Token.Type.PLUS),
            Lexer.Token(Lexer.Token.Type.EQUAL),
            Lexer.Token(Lexer.Token.Type.EOF)
        )

        expectedTokens.forEach {
            assertEquals(it, lexer.nextToken())
        }
    }

    @Test
    fun testMultilineInput() {
        val input = """
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
        """.trimIndent()

        val lexer = Lexer(input)

        val expectedTokens = listOf(
            Lexer.Token(Lexer.Token.Type.LET),
            Lexer.Token(Lexer.Token.Type.IDENTIFIER, "five"),
            Lexer.Token(Lexer.Token.Type.EQUAL),
            Lexer.Token(Lexer.Token.Type.NUMBER, "5"),
            Lexer.Token(Lexer.Token.Type.SEMICOLON),
            Lexer.Token(Lexer.Token.Type.LET),
            Lexer.Token(Lexer.Token.Type.IDENTIFIER, "ten"),
            Lexer.Token(Lexer.Token.Type.EQUAL),
            Lexer.Token(Lexer.Token.Type.NUMBER, "10"),
            Lexer.Token(Lexer.Token.Type.SEMICOLON),
            Lexer.Token(Lexer.Token.Type.LET),
            Lexer.Token(Lexer.Token.Type.IDENTIFIER, "add"),
            Lexer.Token(Lexer.Token.Type.EQUAL),
            Lexer.Token(Lexer.Token.Type.FUNCTION),
            Lexer.Token(Lexer.Token.Type.LEFT_PARENTHESES),
            Lexer.Token(Lexer.Token.Type.IDENTIFIER, "x"),
            Lexer.Token(Lexer.Token.Type.COMMA),
            Lexer.Token(Lexer.Token.Type.IDENTIFIER, "y"),
            Lexer.Token(Lexer.Token.Type.RIGHT_PARENTHESES),
            Lexer.Token(Lexer.Token.Type.LEFT_BRACE),
            Lexer.Token(Lexer.Token.Type.IDENTIFIER, "x"),
            Lexer.Token(Lexer.Token.Type.PLUS),
            Lexer.Token(Lexer.Token.Type.IDENTIFIER, "y"),
            Lexer.Token(Lexer.Token.Type.SEMICOLON),
            Lexer.Token(Lexer.Token.Type.RIGHT_BRACE),
            Lexer.Token(Lexer.Token.Type.SEMICOLON),
            Lexer.Token(Lexer.Token.Type.LET),
            Lexer.Token(Lexer.Token.Type.IDENTIFIER, "result"),
            Lexer.Token(Lexer.Token.Type.EQUAL),
            Lexer.Token(Lexer.Token.Type.IDENTIFIER, "add"),
            Lexer.Token(Lexer.Token.Type.LEFT_PARENTHESES),
            Lexer.Token(Lexer.Token.Type.IDENTIFIER, "five"),
            Lexer.Token(Lexer.Token.Type.COMMA),
            Lexer.Token(Lexer.Token.Type.IDENTIFIER, "ten"),
            Lexer.Token(Lexer.Token.Type.RIGHT_PARENTHESES),
            Lexer.Token(Lexer.Token.Type.SEMICOLON),
            Lexer.Token(Lexer.Token.Type.EOF)
        )

        for (expectedToken in expectedTokens) {
            val token = lexer.nextToken()
            assertEquals(expectedToken, token)
        }
    }
}