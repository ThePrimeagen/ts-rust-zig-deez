import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class LexerTest {

    @Test
    void singleCharacterTokenTests() {
        String input = "=+(){},;";
        Lexer l = new Lexer(input);
        TokenType[] expected = {
                TokenType.ASSIGN,
                TokenType.PLUS,
                TokenType.LPAREN,
                TokenType.RPAREN,
                TokenType.LSQIRLY,
                TokenType.RSQIRLY,
                TokenType.COMMA,
                TokenType.SEMI,
                TokenType.EOF,
        };

        Assertions.assertDoesNotThrow(() -> {
            for (TokenType t : expected) {
                TokenType g = l.nextToken().type();
                if (!t.equals(g)) {
                    throw new AssertionError(String.format("Wanted %s, got %s\n", t.name(), g.name()));
                }
            }
        });
    }

    @Test
    void multiCharacterTokenTests() {
        String input = """
                let five = 5;
                let ten = 10;
                let add = fn(x, y) {
                     x + y;
                };
                let result = add(five, ten);

                !-/*5;
                5 < 10 > 5;""";
        Lexer l = new Lexer(input);
        Token[] expected = {
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENT, "five"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.INT, "5"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENT, "ten"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.INT, "10"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENT, "add"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.FUNC, "fn"),
                new Token(TokenType.LPAREN, "("),
                new Token(TokenType.IDENT, "x"),
                new Token(TokenType.COMMA, ","),
                new Token(TokenType.IDENT, "y"),
                new Token(TokenType.RPAREN, ")"),
                new Token(TokenType.LSQIRLY, "{"),
                new Token(TokenType.IDENT, "x"),
                new Token(TokenType.PLUS, "+"),
                new Token(TokenType.IDENT, "y"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.RSQIRLY, "}"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENT, "result"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.IDENT, "add"),
                new Token(TokenType.LPAREN, "("),
                new Token(TokenType.IDENT, "five"),
                new Token(TokenType.COMMA, ","),
                new Token(TokenType.IDENT, "ten"),
                new Token(TokenType.RPAREN, ")"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.BANG, "!"),
                new Token(TokenType.MINUS, "-"),
                new Token(TokenType.SLASH, "/"),
                new Token(TokenType.ASTERISK, "*"),
                new Token(TokenType.INT, "5"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.INT, "5"),
                new Token(TokenType.LT, "<"),
                new Token(TokenType.INT, "10"),
                new Token(TokenType.GT, ">"),
                new Token(TokenType.INT, "5"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.EOF, "eof"),
        };
        Assertions.assertDoesNotThrow(() -> {
            for (Token t : expected) {
                Token g = l.nextToken();
                if (!t.equals(g)) {
                    throw new AssertionError(String.format("Wanted %s, got %s\n", t, g));
                }
            }
        });
    }

    @Test
    void testUnderscoreInIdentifier() {
        String input = """
                let two_plus_two = 2 + 2;
                let _result = two_plus_two;""";
        Lexer l = new Lexer(input);
        Token[] expected = {
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENT, "two_plus_two"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.INT, "2"),
                new Token(TokenType.PLUS, "+"),
                new Token(TokenType.INT, "2"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENT, "_result"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.IDENT, "two_plus_two"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.EOF, "eof"),
        };

        for (Token t : expected) {
            Token g = l.nextToken();
            Assertions.assertEquals(t, g, "Wanted %s, got %s".formatted(t, g));
        }
    }

    @Test
    void testNewOperationsAndKeywords() {
        String input = """
                if (5 < 10) {
                    return true;
                } else {
                    return false;
                }
                10 == 10; 10 != 9;""";

        Lexer l = new Lexer(input);

        Token[] expected = {
                TokenType.IF.token(),
                TokenType.LPAREN.token(),
                TokenType.INT.createToken("5"),
                TokenType.LT.token(),
                TokenType.INT.createToken("10"),
                TokenType.RPAREN.token(),
                TokenType.LSQIRLY.token(),
                TokenType.RETURN.token(),
                TokenType.TRUE.token(),
                TokenType.SEMI.token(),
                TokenType.RSQIRLY.token(),
                TokenType.ELSE.token(),
                TokenType.LSQIRLY.token(),
                TokenType.RETURN.token(),
                TokenType.FALSE.token(),
                TokenType.SEMI.token(),
                TokenType.RSQIRLY.token(),
                TokenType.INT.createToken("10"),
                TokenType.EQUAL.token(),
                TokenType.INT.createToken("10"),
                TokenType.SEMI.token(),
                TokenType.INT.createToken("10"),
                TokenType.NOT_EQUAL.token(),
                TokenType.INT.createToken("9"),
                TokenType.SEMI.token(),
                TokenType.EOF.token(),
        };

        for (Token t : expected) {
            Token g = l.nextToken();
            Assertions.assertEquals(t, g, "Wanted %s, got %s".formatted(t, g));
        }
    }
}
