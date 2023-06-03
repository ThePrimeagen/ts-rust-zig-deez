import java.util.HashMap;
import java.util.Optional;

public class Lexer {

    private final String input;
    private int pos = 0;

    final HashMap<String, Token> keywordMap = new HashMap<>() {
        {
            put("fn", TokenType.FUNC.createToken("fn"));
            put("let", TokenType.LET.createToken("let"));
        }
    };

    public Lexer(String input) {
        this.input = input;
    }

    public Token nextToken() {
        this.skipWhitespace();

        var tokenType = switch (this.getCc()) {
            case '{' -> TokenType.LSQIRLY;
            case '}' -> TokenType.RSQIRLY;
            case '(' -> TokenType.LPAREN;
            case ')' -> TokenType.RPAREN;
            case ',' -> TokenType.COMMA;
            case ';' -> TokenType.SEMI;
            case '+' -> TokenType.PLUS;
            case '=' -> TokenType.EQUAL;
            case '\0' -> TokenType.EOF;
            default -> TokenType.ILLEGAL;
        };

        var token = switch (tokenType) {
            case EOF -> tokenType.createToken("eof");
            default -> tokenType.createToken(String.valueOf(this.getCc()));
        };

        if (token.type() != TokenType.ILLEGAL) {
            this.advance();
            return token;
        }

        if (Character.isLetter(this.getCc())) {
            var ident = this.indent();

            return Optional
                    .ofNullable(this.keywordMap.get(ident))
                    .orElseGet(() -> TokenType.IDENT.createToken(ident));
        } else if (Character.isDigit(this.getCc())) {
            return TokenType.INT.createToken(this.number());
        }

        this.advance();
        return token;
    }

    private void advance() {
        if (this.pos >= this.input.length()) {
            return;
        }
        this.pos++;
    }

    private char getCc() {
        if (this.pos >= this.input.length()) {
            return '\0';
        }
        return this.input.charAt(this.pos);
    }

    private String number() {
        int pos = this.pos;
        while (Character.isDigit(this.getCc())) {
            this.advance();
        }
        return this.input.substring(pos, this.pos);
    }

    private String indent() {
        int pos = this.pos;
        while (Character.isLetter(this.getCc())) {
            this.advance();
        }
        return this.input.substring(pos, this.pos);
    }

    private void skipWhitespace() {
        while (Character.isWhitespace(this.getCc())) {
            this.advance();
        }
    }
}