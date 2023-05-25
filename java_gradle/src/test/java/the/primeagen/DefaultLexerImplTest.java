package the.primeagen;

import org.junit.jupiter.api.Test;
import the.primeagen.lexer.DefaultLexerImpl;
import the.primeagen.lexer.ILexer;
import the.primeagen.lexer.Token;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class DefaultLexerImplTest {

    @Test
    public void testSpecialCharacters() {
        final String input = "=+(){},;";
        final ILexer lexer = new DefaultLexerImpl(input);

        final Token[] expectedTokens = {
                Token.of(Token.Type.EQUAL),
                Token.of(Token.Type.PLUS),
                Token.of(Token.Type.LEFT_PARENTHESES),
                Token.of(Token.Type.RIGHT_PARENTHESES),
                Token.of(Token.Type.LEFT_BRACE),
                Token.of(Token.Type.RIGHT_BRACE),
                Token.of(Token.Type.COMMA),
                Token.of(Token.Type.SEMICOLON),
                Token.of(Token.Type.EOF)
        };

        assertTokens(lexer, expectedTokens);
    }

    @Test
    public void testString() {
        final String input = """
                let five = 5;
                let ten = 10;
                let add = fn(x, y) {
                  x + y;
                };
                               
                let result = add(five, ten);
                """;

        final ILexer lexer = new DefaultLexerImpl(input);

        final Token[] expectedTokens = {
                Token.of(Token.Type.LET),
                Token.of(Token.Type.IDENTIFIER, "five"),
                Token.of(Token.Type.EQUAL),
                Token.of(Token.Type.INT, "5"),
                Token.of(Token.Type.SEMICOLON),
                Token.of(Token.Type.LET),
                Token.of(Token.Type.IDENTIFIER, "ten"),
                Token.of(Token.Type.EQUAL),
                Token.of(Token.Type.INT, "10"),
                Token.of(Token.Type.SEMICOLON),
                Token.of(Token.Type.LET),
                Token.of(Token.Type.IDENTIFIER, "add"),
                Token.of(Token.Type.EQUAL),
                Token.of(Token.Type.FUNCTION),
                Token.of(Token.Type.LEFT_PARENTHESES),
                Token.of(Token.Type.IDENTIFIER, "x"),
                Token.of(Token.Type.COMMA),
                Token.of(Token.Type.IDENTIFIER, "y"),
                Token.of(Token.Type.RIGHT_PARENTHESES),
                Token.of(Token.Type.LEFT_BRACE),
                Token.of(Token.Type.IDENTIFIER, "x"),
                Token.of(Token.Type.PLUS),
                Token.of(Token.Type.IDENTIFIER, "y"),
                Token.of(Token.Type.SEMICOLON),
                Token.of(Token.Type.RIGHT_BRACE),
                Token.of(Token.Type.SEMICOLON),
                Token.of(Token.Type.LET),
                Token.of(Token.Type.IDENTIFIER, "result"),
                Token.of(Token.Type.EQUAL),
                Token.of(Token.Type.IDENTIFIER, "add"),
                Token.of(Token.Type.LEFT_PARENTHESES),
                Token.of(Token.Type.IDENTIFIER, "five"),
                Token.of(Token.Type.COMMA),
                Token.of(Token.Type.IDENTIFIER, "ten"),
                Token.of(Token.Type.RIGHT_PARENTHESES),
                Token.of(Token.Type.SEMICOLON),
                Token.of(Token.Type.EOF)
        };

        assertTokens(lexer, expectedTokens);
    }


    private void assertTokens(ILexer lexer, Token[] expectedTokens) {
        for (int i = 0; i < expectedTokens.length; i++) {
            final Token expectedToken = expectedTokens[i];
            final Token token = lexer.getNextToken();
            assertEquals(expectedToken, token, "Token at position " + i + " (" + expectedToken + ") differs from actual token " + token);
        }
    }
}
