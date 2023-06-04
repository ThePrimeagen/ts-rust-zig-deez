package dev.hermannm.monkeylang

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.fail

class LexerTest {
    @Test
    fun testBasicSingleCharacterSymbols() = testExpectedTokensInInput(
        input = "=+(){},;",

        expectedTokens = listOf(
            Token(TokenType.Assign, "="),
            Token(TokenType.Plus, "+"),
            Token(TokenType.LeftParen, "("),
            Token(TokenType.RightParen, ")"),
            Token(TokenType.LeftSquirly, "{"),
            Token(TokenType.RightSquirly, "}"),
            Token(TokenType.Comma, ","),
            Token(TokenType.Semicolon, ";"),
            Token(TokenType.EndOfFile, ""),
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
            Token(TokenType.Let, "let"),
            Token(TokenType.Identifier, "five"),
            Token(TokenType.Assign, "="),
            Token(TokenType.Integer, "5"),
            Token(TokenType.Semicolon, ";"),
            Token(TokenType.Let, "let"),
            Token(TokenType.Identifier, "ten"),
            Token(TokenType.Assign, "="),
            Token(TokenType.Integer, "10"),
            Token(TokenType.Semicolon, ";"),
            Token(TokenType.Let, "let"),
            Token(TokenType.Identifier, "add"),
            Token(TokenType.Assign, "="),
            Token(TokenType.Function, "fn"),
            Token(TokenType.LeftParen, "("),
            Token(TokenType.Identifier, "x"),
            Token(TokenType.Comma, ","),
            Token(TokenType.Identifier, "y"),
            Token(TokenType.RightParen, ")"),
            Token(TokenType.LeftSquirly, "{"),
            Token(TokenType.Identifier, "x"),
            Token(TokenType.Plus, "+"),
            Token(TokenType.Identifier, "y"),
            Token(TokenType.Semicolon, ";"),
            Token(TokenType.RightSquirly, "}"),
            Token(TokenType.Semicolon, ";"),
            Token(TokenType.Let, "let"),
            Token(TokenType.Identifier, "result"),
            Token(TokenType.Assign, "="),
            Token(TokenType.Identifier, "add"),
            Token(TokenType.LeftParen, "("),
            Token(TokenType.Identifier, "five"),
            Token(TokenType.Comma, ","),
            Token(TokenType.Identifier, "ten"),
            Token(TokenType.RightParen, ")"),
            Token(TokenType.Semicolon, ";"),
            Token(TokenType.EndOfFile, ""),
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
            Token(TokenType.Bang, "!"),
            Token(TokenType.Minus, "-"),
            Token(TokenType.Slash, "/"),
            Token(TokenType.Asterisk, "*"),
            Token(TokenType.Integer, "5"),
            Token(TokenType.Semicolon, ";"),
            Token(TokenType.Integer, "5"),
            Token(TokenType.LessThan, "<"),
            Token(TokenType.Integer, "10"),
            Token(TokenType.GreaterThan, ">"),
            Token(TokenType.Integer, "5"),
            Token(TokenType.Semicolon, ";"),
            Token(TokenType.Integer, "10"),
            Token(TokenType.Equals, "=="),
            Token(TokenType.Integer, "10"),
            Token(TokenType.Semicolon, ";"),
            Token(TokenType.Integer, "10"),
            Token(TokenType.NotEquals, "!="),
            Token(TokenType.Integer, "9"),
            Token(TokenType.Semicolon, ";"),
            Token(TokenType.EndOfFile, ""),
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
            Token(TokenType.If, "if"),
            Token(TokenType.LeftParen, "("),
            Token(TokenType.Integer, "5"),
            Token(TokenType.LessThan, "<"),
            Token(TokenType.Integer, "10"),
            Token(TokenType.RightParen, ")"),
            Token(TokenType.LeftSquirly, "{"),
            Token(TokenType.Return, "return"),
            Token(TokenType.True, "true"),
            Token(TokenType.Semicolon, ";"),
            Token(TokenType.RightSquirly, "}"),
            Token(TokenType.Else, "else"),
            Token(TokenType.LeftSquirly, "{"),
            Token(TokenType.Return, "return"),
            Token(TokenType.False, "false"),
            Token(TokenType.Semicolon, ";"),
            Token(TokenType.RightSquirly, "}"),
            Token(TokenType.EndOfFile, ""),
        ),
    )

    @Test
    fun testStrings() = testExpectedTokensInInput(
        input = """
            "foobar"
            "foo bar"
            "foo bar baz"
        """.trimIndent(),

        expectedTokens = listOf(
            Token(TokenType.StringLiteral, "\"foobar\""),
            Token(TokenType.StringLiteral, "\"foo bar\""),
            Token(TokenType.StringLiteral, "\"foo bar baz\""),
            Token(TokenType.EndOfFile, ""),
        ),
    )

    @Test
    fun testArrays() = testExpectedTokensInInput(
        input = "[1, 2];",

        expectedTokens = listOf(
            Token(TokenType.LeftBracket, "["),
            Token(TokenType.Integer, "1"),
            Token(TokenType.Comma, ","),
            Token(TokenType.Integer, "2"),
            Token(TokenType.RightBracket, "]"),
            Token(TokenType.Semicolon, ";"),
            Token(TokenType.EndOfFile, ""),
        ),
    )

    @Test
    fun testMaps() = testExpectedTokensInInput(
        input = """
            {"foo": "bar"}
        """.trimIndent(),

        expectedTokens = listOf(
            Token(TokenType.LeftSquirly, "{"),
            Token(TokenType.StringLiteral, "\"foo\""),
            Token(TokenType.Colon, ":"),
            Token(TokenType.StringLiteral, "\"bar\""),
            Token(TokenType.RightSquirly, "}"),
            Token(TokenType.EndOfFile, ""),
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
