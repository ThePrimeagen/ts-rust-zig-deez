package startup.the.parsing;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import startup.the.token.Token;
import startup.the.token.TokenType;

public class LexerTest {

    @Test
    void singleCharacterTokenTests() {
        String input = "=+(){},;";
        Lexer l = new Lexer(input);
        TokenType[] expected = {
                TokenType.EQUAL,
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
                let result = add(five, ten);""";
        Lexer l = new Lexer(input);
        Token[] expected = {
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENT, "five"),
                new Token(TokenType.EQUAL, "="),
                new Token(TokenType.INT, "5"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENT, "ten"),
                new Token(TokenType.EQUAL, "="),
                new Token(TokenType.INT, "10"),
                new Token(TokenType.SEMI, ";"),
                new Token(TokenType.LET, "let"),
                new Token(TokenType.IDENT, "add"),
                new Token(TokenType.EQUAL, "="),
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
                new Token(TokenType.EQUAL, "="),
                new Token(TokenType.IDENT, "add"),
                new Token(TokenType.LPAREN, "("),
                new Token(TokenType.IDENT, "five"),
                new Token(TokenType.COMMA, ","),
                new Token(TokenType.IDENT, "ten"),
                new Token(TokenType.RPAREN, ")"),
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
}
