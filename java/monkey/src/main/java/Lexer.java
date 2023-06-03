public class Lexer {

    private final String input;
    private int pos = 0;

    public Lexer(String input) {
        this.input = input;
    }

    public Token nextToken() {
        this.skipWhitespace();

        var token = switch (this.getCc()) {
            case '{' -> TokenType.LSQIRLY.token();
            case '}' -> TokenType.RSQIRLY.token();
            case '(' -> TokenType.LPAREN.token();
            case ')' -> TokenType.RPAREN.token();
            case ',' -> TokenType.COMMA.token();
            case ';' -> TokenType.SEMI.token();
            case '+' -> TokenType.PLUS.token();
            case '=' -> {
                if (this.peek(this.pos + 1) == '=') {
                    this.advance();
                    yield TokenType.EQUAL.token();
                }

                yield TokenType.ASSIGN.token();
            }
            case '-' -> TokenType.MINUS.token();
            case '!' -> {
                if (this.peek(this.pos + 1) == '=') {
                    this.advance();
                    yield TokenType.NOT_EQUAL.token();
                }

                yield TokenType.BANG.token();
            }
            case '*' -> TokenType.ASTERISK.token();
            case '/' -> TokenType.SLASH.token();
            case '>' -> TokenType.GT.token();
            case '<' -> TokenType.LT.token();
            case '\0' -> TokenType.EOF.token();
            default -> TokenType.ILLEGAL.createToken(String.valueOf(this.getCc()));
        };

        if (token.type() != TokenType.ILLEGAL) {
            this.advance();
            return token;
        }

        switch ((Character) this.getCc()) {
            case Character c when isLetter(c) -> {
                var ident = this.indent();
                return switch (ident) {
                    case "fn" -> TokenType.FUNC.token();
                    case "let" -> TokenType.LET.token();
                    case "true" -> TokenType.TRUE.token();
                    case "false" -> TokenType.FALSE.token();
                    case "if" -> TokenType.IF.token();
                    case "else" -> TokenType.ELSE.token();
                    case "return" -> TokenType.RETURN.token();
                    default -> TokenType.IDENT.createToken(ident);
                };
            }
            case Character c when Character.isDigit(c) -> {
                return TokenType.INT.createToken(this.number());
            }
            default -> {
                this.advance();
                return token;
            }
        }

    }

    private void advance() {
        if (this.pos >= this.input.length()) {
            this.pos = -1;
            return;
        }
        this.pos++;
    }

    private char getCc() {
        if (this.pos >= this.input.length() || this.pos < 0) {
            return '\0';
        }
        return this.input.charAt(this.pos);
    }

    private char peek(int pos) {
        if (pos >= this.input.length() || pos < 0) {
            return '\0';
        }
        return this.input.charAt(pos);
    }

    private boolean isLetter(char c) {
        return Character.isLetter(c) || c == '_';
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
        while (this.isLetter(this.getCc())) {
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