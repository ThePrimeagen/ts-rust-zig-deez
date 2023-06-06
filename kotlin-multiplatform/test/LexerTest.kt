package monkeylang

import kotlin.test.Test
import kotlin.test.assertEquals

class LexerTest {
    @Test
    fun testBasicSingleCharacterSymbols() = testExpectedTokensInInput(
        input = "=+(){},;",

        expectedTokens = listOf(
            TestToken(TokenType.Assign, "="),
            TestToken(TokenType.Plus, "+"),
            TestToken(TokenType.LeftParen, "("),
            TestToken(TokenType.RightParen, ")"),
            TestToken(TokenType.LeftSquirly, "{"),
            TestToken(TokenType.RightSquirly, "}"),
            TestToken(TokenType.Comma, ","),
            TestToken(TokenType.Semicolon, ";"),
            TestToken(TokenType.EndOfFile, ""),
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
            TestToken(TokenType.Let, "let"),
            TestToken(TokenType.Identifier, "five"),
            TestToken(TokenType.Assign, "="),
            TestToken(TokenType.Integer, "5"),
            TestToken(TokenType.Semicolon, ";"),
            TestToken(TokenType.Let, "let"),
            TestToken(TokenType.Identifier, "ten"),
            TestToken(TokenType.Assign, "="),
            TestToken(TokenType.Integer, "10"),
            TestToken(TokenType.Semicolon, ";"),
            TestToken(TokenType.Let, "let"),
            TestToken(TokenType.Identifier, "add"),
            TestToken(TokenType.Assign, "="),
            TestToken(TokenType.Function, "fn"),
            TestToken(TokenType.LeftParen, "("),
            TestToken(TokenType.Identifier, "x"),
            TestToken(TokenType.Comma, ","),
            TestToken(TokenType.Identifier, "y"),
            TestToken(TokenType.RightParen, ")"),
            TestToken(TokenType.LeftSquirly, "{"),
            TestToken(TokenType.Identifier, "x"),
            TestToken(TokenType.Plus, "+"),
            TestToken(TokenType.Identifier, "y"),
            TestToken(TokenType.Semicolon, ";"),
            TestToken(TokenType.RightSquirly, "}"),
            TestToken(TokenType.Semicolon, ";"),
            TestToken(TokenType.Let, "let"),
            TestToken(TokenType.Identifier, "result"),
            TestToken(TokenType.Assign, "="),
            TestToken(TokenType.Identifier, "add"),
            TestToken(TokenType.LeftParen, "("),
            TestToken(TokenType.Identifier, "five"),
            TestToken(TokenType.Comma, ","),
            TestToken(TokenType.Identifier, "ten"),
            TestToken(TokenType.RightParen, ")"),
            TestToken(TokenType.Semicolon, ";"),
            TestToken(TokenType.EndOfFile, ""),
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
            TestToken(TokenType.Bang, "!"),
            TestToken(TokenType.Minus, "-"),
            TestToken(TokenType.Slash, "/"),
            TestToken(TokenType.Asterisk, "*"),
            TestToken(TokenType.Integer, "5"),
            TestToken(TokenType.Semicolon, ";"),
            TestToken(TokenType.Integer, "5"),
            TestToken(TokenType.LessThan, "<"),
            TestToken(TokenType.Integer, "10"),
            TestToken(TokenType.GreaterThan, ">"),
            TestToken(TokenType.Integer, "5"),
            TestToken(TokenType.Semicolon, ";"),
            TestToken(TokenType.Integer, "10"),
            TestToken(TokenType.Equals, "=="),
            TestToken(TokenType.Integer, "10"),
            TestToken(TokenType.Semicolon, ";"),
            TestToken(TokenType.Integer, "10"),
            TestToken(TokenType.NotEquals, "!="),
            TestToken(TokenType.Integer, "9"),
            TestToken(TokenType.Semicolon, ";"),
            TestToken(TokenType.EndOfFile, ""),
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
            TestToken(TokenType.If, "if"),
            TestToken(TokenType.LeftParen, "("),
            TestToken(TokenType.Integer, "5"),
            TestToken(TokenType.LessThan, "<"),
            TestToken(TokenType.Integer, "10"),
            TestToken(TokenType.RightParen, ")"),
            TestToken(TokenType.LeftSquirly, "{"),
            TestToken(TokenType.Return, "return"),
            TestToken(TokenType.True, "true"),
            TestToken(TokenType.Semicolon, ";"),
            TestToken(TokenType.RightSquirly, "}"),
            TestToken(TokenType.Else, "else"),
            TestToken(TokenType.LeftSquirly, "{"),
            TestToken(TokenType.Return, "return"),
            TestToken(TokenType.False, "false"),
            TestToken(TokenType.Semicolon, ";"),
            TestToken(TokenType.RightSquirly, "}"),
            TestToken(TokenType.EndOfFile, ""),
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
            TestToken(TokenType.StringLiteral, "\"foobar\""),
            TestToken(TokenType.StringLiteral, "\"foo bar\""),
            TestToken(TokenType.StringLiteral, "\"foo bar baz\""),
            TestToken(TokenType.EndOfFile, ""),
        ),
    )

    @Test
    fun testArrays() = testExpectedTokensInInput(
        input = "[1, 2];",

        expectedTokens = listOf(
            TestToken(TokenType.LeftBracket, "["),
            TestToken(TokenType.Integer, "1"),
            TestToken(TokenType.Comma, ","),
            TestToken(TokenType.Integer, "2"),
            TestToken(TokenType.RightBracket, "]"),
            TestToken(TokenType.Semicolon, ";"),
            TestToken(TokenType.EndOfFile, ""),
        ),
    )

    @Test
    fun testMaps() = testExpectedTokensInInput(
        input = """
            {"foo": "bar"}
        """.trimIndent(),

        expectedTokens = listOf(
            TestToken(TokenType.LeftSquirly, "{"),
            TestToken(TokenType.StringLiteral, "\"foo\""),
            TestToken(TokenType.Colon, ":"),
            TestToken(TokenType.StringLiteral, "\"bar\""),
            TestToken(TokenType.RightSquirly, "}"),
            TestToken(TokenType.EndOfFile, ""),
        ),
    )

    @Test
    fun testPositions() {
        val input = """
            let a = "foobar"
            let b = "foo
            bar"
            a == b
        """.trimIndent()

        val expectedTokens = listOf(
            Token(TokenType.Let, "let", DocumentRange.fromPositions(1, 0, 1, 3)),
            Token(TokenType.Identifier, "a", DocumentRange.fromPositions(1, 4, 1, 5)),
            Token(TokenType.Assign, "=", DocumentRange.fromPositions(1, 6, 1, 7)),
            Token(TokenType.StringLiteral, "\"foobar\"", DocumentRange.fromPositions(1, 8, 1, 16)),
            Token(TokenType.Let, "let", DocumentRange.fromPositions(2, 0, 2, 3)),
            Token(TokenType.Identifier, "b", DocumentRange.fromPositions(2, 4, 2, 5)),
            Token(TokenType.Assign, "=", DocumentRange.fromPositions(2, 6, 2, 7)),
            Token(TokenType.StringLiteral, "\"foo\nbar\"", DocumentRange.fromPositions(2, 8, 3, 4)),
            Token(TokenType.Identifier, "a", DocumentRange.fromPositions(4, 0, 4, 1)),
            Token(TokenType.Equals, "==", DocumentRange.fromPositions(4, 2, 4, 4)),
            Token(TokenType.Identifier, "b", DocumentRange.fromPositions(4, 5, 4, 6)),
            Token(TokenType.EndOfFile, "", DocumentRange.fromPositions(4, 6, 4, 6)),
        )

        val actualTokens = Lexer(input)
        for (token in expectedTokens) {
            val nextToken = actualTokens.next()
            assertEquals(token.type, nextToken.type)
            assertEquals(token.literal, nextToken.literal)
            assertEquals(token.range, nextToken.range)
        }
    }

    private fun testExpectedTokensInInput(input: String, expectedTokens: List<TestToken>) {
        val lexer = Lexer(input)
        for (token in expectedTokens) {
            val nextToken = lexer.next()
            assertEquals(token.type, nextToken.type)
            assertEquals(token.literal, nextToken.literal)
        }
    }
}
