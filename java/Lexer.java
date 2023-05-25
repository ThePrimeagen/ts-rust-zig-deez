import java.util.HashMap;

class Lexer {
    String input;
    int pos;
    HashMap<String, Token> keywordMap = new HashMap<String, Token>() {
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

        Token t = null;
        TokenType tt = TokenType.ILLEGAL;
        switch (this.getCc()) {
            case '{' -> tt = TokenType.LSQIRLY;
            case '}' -> tt = TokenType.RSQIRLY;
            case '(' -> tt = TokenType.LPAREN;
            case ')' -> tt = TokenType.RPAREN;
            case ',' -> tt = TokenType.COMMA;
            case ';' -> tt = TokenType.SEMI;
            case '+' -> tt = TokenType.PLUS;
            case '=' -> tt = TokenType.EQUAL;
            case '\0' -> tt = TokenType.EOF;
        }

        if (tt.equals(TokenType.EOF)) {
            t = this.createToken(tt, "eof");
        } else {
            t = this.createToken(tt, String.valueOf(this.getCc()));
        }

        if (this.isLetter(this.getCc())) {
            String ident = this.indent();
            Token keyword = this.keywordMap.getOrDefault(ident, null);
            if (keyword != null) {
                return keyword;
            } else {
                return createToken(TokenType.IDENT, ident);
            }
        } else if (this.isNumber(this.getCc())) {
            return this.createToken(TokenType.INT, this.number());
        } else if (t == null) {
            return this.createToken(TokenType.ILLEGAL, String.valueOf(this.getCc()));
        }

        this.advance();
        return t;
    }

    private void advance() {
        if (this.pos + 1 >= this.input.length()) {
            this.pos = -1;
            return;
        }
        this.pos++;
    }

    private char getCc() {
        if (this.pos > this.input.length() || this.pos < 0) {
            return 0;
        }
        return this.input.charAt(this.pos);
    }

    private Token createToken(TokenType type, String literal) {
        return new Token(type, literal);
    }

    private boolean isLetter(char c) {
        return 'a' <= c && 'z' >= c || 'A' <= c && 'Z' >= c || c == '_';
    }

    private boolean isNumber(char c) {
        return '0' <= c && '9' >= c;
    }

    private String number() {
        int pos = this.pos;
        while (this.isNumber(this.getCc())) {
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
        char cc = this.getCc();
        while (cc == ' ' || cc == '\t' || cc == '\n' || cc == '\r') {
            this.advance();
            cc = this.getCc();
        }
    }
}
