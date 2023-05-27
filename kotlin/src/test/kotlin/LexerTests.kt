import org.junit.jupiter.api.Test
import TokenType.*
import kotlin.test.assertEquals

internal class LexerTests {
    @Test
    fun test1() {
        val input = """"foo"=+(){},;"""
        val expected = listOf(
            Token(STRING, "foo"),
            Token(EQUAL, "="),
            Token(PLUS, "+"),
            Token(LPAREN, "("),
            Token(RPAREN, ")"),
            Token(LSQUIRLY, "{"),
            Token(RSQUIRLY, "}"),
            Token(COMMA, ","),
            Token(SEMI, ";"),
            Token(EOF)
        )

        val tokens = Lexer.parse(input).toList()
        assertEquals(expected, tokens)
    }

    @Test
    fun test2() {
        val input = """
                                let five = 5;
                                let ten = 10;
                                let add = fn(x, y) {
                                    x + y;
                                };
                                let result = add(five, ten);
                                """
        val expected = listOf(
            Token(LET, "let"),
            Token(IDENT, "five"),
            Token(EQUAL, "="),
            Token(INT, "5"),
            Token(SEMI, ";"),
            Token(LET, "let"),
            Token(IDENT, "ten"),
            Token(EQUAL, "="),
            Token(INT, "10"),
            Token(SEMI, ";"),
            Token(LET, "let"),
            Token(IDENT, "add"),
            Token(EQUAL, "="),
            Token(FUNC, "fn"),
            Token(LPAREN, "("),
            Token(IDENT, "x"),
            Token(COMMA, ","),
            Token(IDENT, "y"),
            Token(RPAREN, ")"),
            Token(LSQUIRLY, "{"),
            Token(IDENT, "x"),
            Token(PLUS, "+"),
            Token(IDENT, "y"),
            Token(SEMI, ";"),
            Token(RSQUIRLY, "}"),
            Token(SEMI, ";"),
            Token(LET, "let"),
            Token(IDENT, "result"),
            Token(EQUAL, "="),
            Token(IDENT, "add"),
            Token(LPAREN, "("),
            Token(IDENT, "five"),
            Token(COMMA, ","),
            Token(IDENT, "ten"),
            Token(RPAREN, ")"),
            Token(SEMI, ";"),
            Token(EOF)
        )

        val tokens = Lexer.parse(input).toList()
        assertEquals(expected, tokens)
    }
}