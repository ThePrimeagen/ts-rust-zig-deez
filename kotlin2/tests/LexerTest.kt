package dev.hermannm.monkeylang

import kotlin.test.Test
import kotlin.test.assertEquals

class LexerTest {
    @Test
    fun `Test basic single-character symbols`() = testExpectedTokensInInput(
        input = "=+(){},;",

        expectedTokens = listOf(
            Token.Assign,
            Token.Plus,
            Token.LeftParen,
            Token.RightParen,
            Token.LeftBrace,
            Token.RightBrace,
            Token.Comma,
            Token.Semicolon,
            Token.EndOfFile,
        ),
    )

    @Test
    fun `Test sample with identifiers`() = testExpectedTokensInInput(
        input = """
            let five = 5;
            let ten = 10;
            
            let add = fn(x, y) {
              x + y;
            };
            
            let result = add(five, ten);
        """.trimIndent(),

        expectedTokens = listOf(
            Token.Let,
            Token.Identifier("five"),
            Token.Assign,
            Token.Integer(5),
            Token.Semicolon,
            Token.Let,
            Token.Identifier("ten"),
            Token.Assign,
            Token.Integer(10),
            Token.Semicolon,
            Token.Let,
            Token.Identifier("add"),
            Token.Assign,
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
            Token.Assign,
            Token.Identifier("add"),
            Token.LeftParen,
            Token.Identifier("five"),
            Token.Comma,
            Token.Identifier("ten"),
            Token.RightParen,
            Token.Semicolon,
            Token.EndOfFile,
        ),
    )

    @Test
    fun `Test extended symbols`() = testExpectedTokensInInput(
        input = """
            !-/*5;
            5 < 10 > 5;
            10 == 10;
            10 != 9;
        """.trimIndent(),

        expectedTokens = listOf(
            Token.Bang,
            Token.Minus,
            Token.Slash,
            Token.Asterisk,
            Token.Integer(5),
            Token.Semicolon,
            Token.Integer(5),
            Token.LessThan,
            Token.Integer(10),
            Token.GreaterThan,
            Token.Integer(5),
            Token.Semicolon,
            Token.Integer(10),
            Token.Equals,
            Token.Integer(10),
            Token.Semicolon,
            Token.Integer(10),
            Token.NotEquals,
            Token.Integer(9),
            Token.Semicolon,
            Token.EndOfFile,
        ),
    )

    @Test
    fun `Test if statement`() = testExpectedTokensInInput(
        input = """
            if (5 < 10) {
                return true;
            } else {
                return false;
            }
        """.trimIndent(),

        expectedTokens = listOf(
            Token.If,
            Token.LeftParen,
            Token.Integer(5),
            Token.LessThan,
            Token.Integer(10),
            Token.RightParen,
            Token.LeftBrace,
            Token.Return,
            Token.True,
            Token.Semicolon,
            Token.RightBrace,
            Token.Else,
            Token.LeftBrace,
            Token.Return,
            Token.False,
            Token.Semicolon,
            Token.RightBrace,
            Token.EndOfFile,
        ),
    )

    @Test
    fun `Test strings`() = testExpectedTokensInInput(
        input = """
            "foobar"
            "foo bar"
        """.trimIndent(),

        expectedTokens = listOf(
            Token.StringLiteral("foobar"),
            Token.StringLiteral("foo bar"),
            Token.EndOfFile,
        ),
    )

    @Test
    fun `Test arrays`() = testExpectedTokensInInput(
        input = "[1, 2];",

        expectedTokens = listOf(
            Token.LeftBracket,
            Token.Integer(1),
            Token.Comma,
            Token.Integer(2),
            Token.RightBracket,
            Token.Semicolon,
            Token.EndOfFile,
        ),
    )

    @Test
    fun `Test maps`() = testExpectedTokensInInput(
        input = """
            {"foo": "bar"}
        """.trimIndent(),

        expectedTokens = listOf(
            Token.LeftBrace,
            Token.StringLiteral("foo"),
            Token.Colon,
            Token.StringLiteral("bar"),
            Token.RightBrace,
            Token.EndOfFile,
        ),
    )

    private fun testExpectedTokensInInput(input: String, expectedTokens: List<Token>) {
        val lexer = Lexer(input)

        for (token in expectedTokens) {
            val nextToken = lexer.nextToken()
            assertEquals(token, nextToken)
        }
    }
}
