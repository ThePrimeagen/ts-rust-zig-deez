import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import root.LocalizedToken;
import root.Token;
import root.TokenType;
import root.lexer.Lexer;

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
                new Token(TokenType.IDENTIFIER, "five"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.INT, "5"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENTIFIER, "ten"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.INT, "10"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENTIFIER, "add"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.FUNC, "fn"),
                new Token(TokenType.LPAREN, "("),
                new Token(TokenType.IDENTIFIER, "x"),
                new Token(TokenType.COMMA, ","),
                new Token(TokenType.IDENTIFIER, "y"),
                new Token(TokenType.RPAREN, ")"),
                new Token(TokenType.LSQIRLY, "{"),
                new Token(TokenType.IDENTIFIER, "x"),
                new Token(TokenType.PLUS, "+"),
                new Token(TokenType.IDENTIFIER, "y"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.RSQIRLY, "}"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENTIFIER, "result"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.IDENTIFIER, "add"),
                new Token(TokenType.LPAREN, "("),
                new Token(TokenType.IDENTIFIER, "five"),
                new Token(TokenType.COMMA, ","),
                new Token(TokenType.IDENTIFIER, "ten"),
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
                new Token(TokenType.IDENTIFIER, "two_plus_two"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.INT, "2"),
                new Token(TokenType.PLUS, "+"),
                new Token(TokenType.INT, "2"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENTIFIER, "_result"),
                new Token(TokenType.ASSIGN, "="),
                new Token(TokenType.IDENTIFIER, "two_plus_two"),
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

    @Test
    void testLocalization() {
        String input = """
                let a = 10;
                let b = 5;
                
                fn add(a,b) {
                    return a + b
                }
                
                let foo = add(a, b)
                """;

        LocalizedToken[] expected = {
                TokenType.LET.token().localize(0, 0, "let a = 10;"),
                TokenType.IDENTIFIER.createToken("a").localize(0, 4, "let a = 10;"),
                TokenType.ASSIGN.token().localize(0, 6, "let a = 10;"),
                TokenType.INT.createToken("10").localize(0, 8, "let a = 10;"),
                TokenType.SEMI.token().localize(0, 10, "let a = 10;"),

                TokenType.LET.token().localize(1, 0, "let b = 5;"),
                TokenType.IDENTIFIER.createToken("b").localize(1, 4, "let b = 5;"),
                TokenType.ASSIGN.token().localize(1, 6, "let b = 5;"),
                TokenType.INT.createToken("5").localize(1, 8, "let b = 5;"),
                TokenType.SEMI.token().localize(1, 9, "let b = 5;"),

                TokenType.FUNC.token().localize(3, 0, "fn add(a,b) {"),
                TokenType.IDENTIFIER.createToken("add").localize(3, 3, "fn add(a,b) {"),
                TokenType.LPAREN.token().localize(3, 6, "fn add(a,b) {"),
                TokenType.IDENTIFIER.createToken("a").localize(3, 7, "fn add(a,b) {"),
                TokenType.COMMA.token().localize(3, 8, "fn add(a,b) {"),
                TokenType.IDENTIFIER.createToken("b").localize(3, 9, "fn add(a,b) {"),
                TokenType.RPAREN.token().localize(3, 10, "fn add(a,b) {"),
                TokenType.LSQIRLY.token().localize(3, 12, "fn add(a,b) {"),

                TokenType.RETURN.token().localize(4, 4, "    return a + b"),
                TokenType.IDENTIFIER.createToken("a").localize(4, 11, "    return a + b"),
                TokenType.PLUS.token().localize(4, 13, "    return a + b"),
                TokenType.IDENTIFIER.createToken("b").localize(4, 15, "    return a + b"),

                TokenType.RSQIRLY.token().localize(5, 0, "}"),

                TokenType.LET.token().localize(7, 0, "let foo = add(a, b)"),
                TokenType.IDENTIFIER.createToken("foo").localize(7, 4, "let foo = add(a, b)"),
                TokenType.ASSIGN.token().localize(7, 8, "let foo = add(a, b)"),
                TokenType.IDENTIFIER.createToken("add").localize(7, 10, "let foo = add(a, b)"),
                TokenType.LPAREN.token().localize(7, 13, "let foo = add(a, b)"),
                TokenType.IDENTIFIER.createToken("a").localize(7, 14, "let foo = add(a, b)"),
                TokenType.COMMA.token().localize(7, 15, "let foo = add(a, b)"),
                TokenType.IDENTIFIER.createToken("b").localize(7, 17, "let foo = add(a, b)"),
                TokenType.RPAREN.token().localize(7, 18, "let foo = add(a, b)"),
        };

        Lexer l = new Lexer(input);

        for (LocalizedToken t : expected) {
            LocalizedToken g = l.nextLocalized();
            Assertions.assertEquals(t, g, "Wanted %s, got %s".formatted(t, g));
        }
    }
}
