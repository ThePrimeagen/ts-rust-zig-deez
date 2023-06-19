import lexer.Lexer
import lexer.TokenType
import lexer.Token

/**
 * tests the lexer
 */
class LexerTest {

    static void main(String[] args) {
        Test.run 'nextToken simple', {
            String input = '=+(){},;'

            List<TokenType> tokens = [
                TokenType.ASSIGN,
                TokenType.PLUS,
                TokenType.LPAREN,
                TokenType.RPAREN,
                TokenType.LBRACE,
                TokenType.RBRACE,
                TokenType.COMMA,
                TokenType.SEMICOLON
            ]

            Lexer lexer = new Lexer(input)

            tokens.each { expected ->
                TokenType actual = lexer.nextToken().type
                assert expected == actual
            }
        }

        Test.run 'nextToken complete', {
            String input = '''
                let five = 5;
                let ten = 10;
                let add = fn(x, y) {
                    x + y;
                };
                let result = add(five, ten);
                !-/*5;
                5 < 10 > 5;
                if (5 < 10) {
                    return true;
                } else {
                    return false;
                }

                10 == 10;
                10 != 9;
            '''

            Lexer lexer = new Lexer(input)

            List<Token> tokens = [
                new Token(TokenType.LET),
                new Token(TokenType.IDENT, 'five'),
                new Token(TokenType.ASSIGN),
                new Token(TokenType.INT, '5'),
                new Token(TokenType.SEMICOLON),
                new Token(TokenType.LET),
                new Token(TokenType.IDENT, 'ten'),
                new Token(TokenType.ASSIGN),
                new Token(TokenType.INT, '10'),
                new Token(TokenType.SEMICOLON),
                new Token(TokenType.LET),
                new Token(TokenType.IDENT, 'add'),
                new Token(TokenType.ASSIGN),
                new Token(TokenType.FUNCTION),
                new Token(TokenType.LPAREN),
                new Token(TokenType.IDENT, 'x'),
                new Token(TokenType.COMMA),
                new Token(TokenType.IDENT, 'y'),
                new Token(TokenType.RPAREN),
                new Token(TokenType.LBRACE),
                new Token(TokenType.IDENT, 'x'),
                new Token(TokenType.PLUS),
                new Token(TokenType.IDENT, 'y'),
                new Token(TokenType.SEMICOLON),
                new Token(TokenType.RBRACE),
                new Token(TokenType.SEMICOLON),
                new Token(TokenType.LET),
                new Token(TokenType.IDENT, 'result'),
                new Token(TokenType.ASSIGN),
                new Token(TokenType.IDENT, 'add'),
                new Token(TokenType.LPAREN),
                new Token(TokenType.IDENT, 'five'),
                new Token(TokenType.COMMA),
                new Token(TokenType.IDENT, 'ten'),
                new Token(TokenType.RPAREN),
                new Token(TokenType.SEMICOLON),
                new Token(TokenType.BANG),
                new Token(TokenType.DASH),
                new Token(TokenType.FORWARD_SLASH),
                new Token(TokenType.ASTERISK),
                new Token(TokenType.INT, '5'),
                new Token(TokenType.SEMICOLON),
                new Token(TokenType.INT, '5'),
                new Token(TokenType.LESS_THAN),
                new Token(TokenType.INT, '10'),
                new Token(TokenType.GREATER_THAN),
                new Token(TokenType.INT, '5'),
                new Token(TokenType.SEMICOLON),
                new Token(TokenType.IF),
                new Token(TokenType.LPAREN),
                new Token(TokenType.INT, '5'),
                new Token(TokenType.LESS_THAN),
                new Token(TokenType.INT, '10'),
                new Token(TokenType.RPAREN),
                new Token(TokenType.LBRACE),
                new Token(TokenType.RETURN),
                new Token(TokenType.TRUE),
                new Token(TokenType.SEMICOLON),
                new Token(TokenType.RBRACE),
                new Token(TokenType.ELSE),
                new Token(TokenType.LBRACE),
                new Token(TokenType.RETURN),
                new Token(TokenType.FALSE),
                new Token(TokenType.SEMICOLON),
                new Token(TokenType.RBRACE),
                new Token(TokenType.INT, '10'),
                new Token(TokenType.EQUAL),
                new Token(TokenType.INT, '10'),
                new Token(TokenType.SEMICOLON),
                new Token(TokenType.INT, '10'),
                new Token(TokenType.NOT_EQUAL),
                new Token(TokenType.INT, '9'),
                new Token(TokenType.SEMICOLON),
                new Token(TokenType.EOF)
            ]

            tokens.each { Token expected ->
                Token actual = lexer.nextToken()
                assert expected.type == actual.type
                assert expected.literal == actual.literal
            }
        }
    }

}
