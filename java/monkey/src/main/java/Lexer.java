public class Lexer {

    private final String input;
    private int pos = 0;

    public Lexer(String input) {
        this.input = input;
    }

    public Token nextToken() {
        this.skipWhitespace();

        var currentChar = this.readCc();

        return switch ((Character) currentChar) {
            case '{' -> TokenType.LSQIRLY.token();
            case '}' -> TokenType.RSQIRLY.token();
            case '(' -> TokenType.LPAREN.token();
            case ')' -> TokenType.RPAREN.token();
            case ',' -> TokenType.COMMA.token();
            case ';' -> TokenType.SEMI.token();
            case '+' -> TokenType.PLUS.token();
            case '=' -> {
                if (this.getCc() == '=') {
                    this.advance();
                    yield TokenType.EQUAL.token();
                }

                yield TokenType.ASSIGN.token();
            }
            case '-' -> TokenType.MINUS.token();
            case '!' -> {
                if (this.getCc() == '=') {
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

            case Character c when isLetter(c) -> {
                var ident = this.indent(currentChar);
                yield switch (ident) {
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
            case Character c when Character.isDigit(c) -> TokenType.INT.createToken(this.number(currentChar));
            default -> TokenType.ILLEGAL.createToken(String.valueOf(currentChar));
        };
    }

    private void advance() {
        if (this.pos >= this.input.length()) {
            this.pos = -1;
            return;
        }
        this.pos++;
    }

    private char getCc() {
        return this.peek(this.pos);
    }

    private char readCc() {
        var currentChar = this.getCc();
        this.advance();
        return currentChar;
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

    private String number(char firstChar) {
        int pos = this.pos;
        while (Character.isDigit(this.getCc())) {
            this.advance();
        }
        return firstChar + this.input.substring(pos, this.pos);
    }

    private String indent(char firstChar) {
        int pos = this.pos;
        while (this.isLetter(this.getCc())) {
            this.advance();
        }
        return firstChar + this.input.substring(pos, this.pos);
    }

    private void skipWhitespace() {
        while (Character.isWhitespace(this.getCc())) {
            this.advance();
        }
    }
}
