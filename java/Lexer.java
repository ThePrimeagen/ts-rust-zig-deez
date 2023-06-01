import java.util.HashMap;
import java.util.Optional;

class Lexer {

    private final String input;
    private int pos = 0;

    final HashMap<String, Token> keywordMap = new HashMap<>() {
        {
            put("fn", createToken(TokenType.FUNC, "fn"));
            put("let", createToken(TokenType.LET, "let"));
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
            case EOF -> this.createToken(tokenType, "eof");
            default -> this.createToken(tokenType, String.valueOf(this.getCc()));
        };

        if (Character.isLetter(this.getCc())) {
            var ident = this.indent();

            return Optional
                    .ofNullable(this.keywordMap.get(ident))
                    .orElseGet(() -> createToken(TokenType.IDENT, ident));
        } else if (Character.isDigit(this.getCc())) {
            return this.createToken(TokenType.INT, this.number());
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

    private Token createToken(TokenType type, String literal) {
        return new Token(type, literal);
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
