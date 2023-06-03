package dev.hermannm.monkeylang

import kotlin.test.Test
import kotlin.test.assertEquals

class LexerTest {
    @Test
    fun testBasicSingleCharacterSymbols() = testExpectedTokensInInput(
        input = "=+(){},;",

        expectedTokens = listOf(
            Token.Assign,
            Token.Plus,
            Token.LeftParen,
            Token.RightParen,
            Token.LeftSquirly,
            Token.RightSquirly,
            Token.Comma,
            Token.Semicolon,
            Token.EndOfFile,
        ),
    )

    @Test
    fun testSampleWithIdentifiers() = testExpectedTokensInInput(
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
            Token.LeftSquirly,
            Token.Identifier("x"),
            Token.Plus,
            Token.Identifier("y"),
            Token.Semicolon,
            Token.RightSquirly,
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
    fun testExtendedSymbols() = testExpectedTokensInInput(
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
    fun testIfStatement() = testExpectedTokensInInput(
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
            Token.LeftSquirly,
            Token.Return,
            Token.True,
            Token.Semicolon,
            Token.RightSquirly,
            Token.Else,
            Token.LeftSquirly,
            Token.Return,
            Token.False,
            Token.Semicolon,
            Token.RightSquirly,
            Token.EndOfFile,
        ),
    )

    @Test
    fun testStrings() = testExpectedTokensInInput(
        input = """
            "foobar"
            "foo bar"
            "foo bar baz
        """.trimIndent(),

        expectedTokens = listOf(
            Token.StringLiteral("foobar"),
            Token.StringLiteral("foo bar"),
            Token.Illegal("\"foo bar baz"),
            Token.EndOfFile,
        ),
    )

    @Test
    fun testArrays() = testExpectedTokensInInput(
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
    fun testMaps() = testExpectedTokensInInput(
        input = """
            {"foo": "bar"}
        """.trimIndent(),

        expectedTokens = listOf(
            Token.LeftSquirly,
            Token.StringLiteral("foo"),
            Token.Colon,
            Token.StringLiteral("bar"),
            Token.RightSquirly,
            Token.EndOfFile,
        ),
    )

    private fun testExpectedTokensInInput(input: String, expectedTokens: List<Token>) {
        val lexer = Lexer(input)

        for (token in expectedTokens) {
            val nextToken = lexer.next()
            assertEquals(token, nextToken)
        }
    }
}
