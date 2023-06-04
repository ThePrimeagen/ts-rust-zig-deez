
class Lexer {
    def keywords = [
        fn: TokenType.FUNCTION, 
        let: TokenType.LET,
        true: TokenType.TRUE,
        false: TokenType.FALSE,
        if: TokenType.IF,
        else: TokenType.ELSE,
        return: TokenType.RETURN
    ];
    String input;
    int position;
    int readPosition; // used for peeking
    byte ch; // only support ASCII

    Lexer(String input) {
        this.input = input;
        this.readChar();
    }
    
    Token NextToken() {
        Token tok;

        this.skipWhitespace();
        switch (this.ch) {
            case (byte)'=':
                if (this.peekChar() == (byte)'=') { 
                    byte ch = this.ch;
                    this.readChar();
                    String literal = new String(ch) + new String(this.ch); // I hate this
                    tok = new Token(TokenType.EQ, literal); // Can we just pass '==' to the TokenType constructor literally rather than creating 2 new garbage strings
                } else {
                    tok = new Token(TokenType.ASSIGN, this.ch); 
                }
                break;
            case (byte)'+':
                tok = new Token(TokenType.PLUS, this.ch);
                break;
            case (byte)'-':
                tok = new Token(TokenType.MINUS, this.ch);
                break;
            case (byte)'!':
                if (this.peekChar() == (byte)'=') {
                    byte ch = this.ch;
                    this.readChar();
                    String literal = new String(ch) + new String(this.ch); // I hate this
                    tok = new Token(TokenType.NOT_EQ, literal);// Can we just pass '!=' to the TokenType constructor literally rather than creating 2 new garbage strings
                } else {
                    tok = new Token(TokenType.BANG, this.ch);
                }
                break;
            case (byte)'/':
                tok = new Token(TokenType.SLASH, this.ch);
                break;
            case (byte)'*':
                tok = new Token(TokenType.ASTERISK, this.ch);
                break;
            case (byte)'<':
                tok = new Token(TokenType.LT, this.ch);
                break;
            case (byte)'>':
                tok = new Token(TokenType.GT, this.ch);
                break;
            case (byte)';':
                tok = new Token(TokenType.SEMICOLON, this.ch);
                break;
            case (byte)'(':
                tok = new Token(TokenType.LPAREN, this.ch);
                break;
            case (byte)')':
                tok = new Token(TokenType.RPAREN, this.ch);
                break;
            case (byte)',':
                tok = new Token(TokenType.COMMA, this.ch);
                break;
            case (byte)'{':
                tok = new Token(TokenType.LBRACE, this.ch);
                break;
            case (byte)'}':
                tok = new Token(TokenType.RBRACE, this.ch);
                break;
            case (byte)0:
                tok = new Token(TokenType.EOF, this.ch);
                break;
            default:
                if (isLetter(this.ch)) {
                    String tmp = new String(this.readIdentifier());
                    tok = new Token(LookupIdent(tmp), tmp);
                    return tok;
                } else if (isDigit(this.ch)) {
                    tok = new Token(TokenType.INT, this.readNumber());
                    return tok;
                } else {
                    tok = new Token(TokenType.ILLEGAL, this.ch);;
                }
        }

        this.readChar();
        return tok;
    }

    byte peekChar() {
        if (this.readPosition >= this.input.length()) {
            return 0;
        } else {
            return (byte)this.input[this.readPosition];
        }
    }

    String readNumber() {
        int position = this.position;
        while (isDigit(this.ch)) {
            this.readChar();
        }
        return this.input[position..<this.position];
    }

    boolean isDigit(byte ch) {
        return ch >= '0' && ch <= '9';
    }

    void skipWhitespace() {
        while (this.ch == (byte)' ' || this.ch == (byte)'\t' || this.ch == (byte)'\n' || this.ch == (byte)'\r') {
            this.readChar();
        }
    }

    TokenType LookupIdent(String ident) {
        return this.keywords[ident] ?: TokenType.IDENT; // returns IDENT if key is invalid
    }

    void readChar() {
        if (this.readPosition >= this.input.length()) {
            this.ch = 0;
        } else {
            this.ch = this.input.getBytes()[this.readPosition]; // does this work?
            // this.ch = (byte) this.input.getAt(this.readPosition); // does this actually take longer than just leaving it as a char?
        }
        this.position = this.readPosition;
        ++this.readPosition;
    }

    String readIdentifier() {
        int position = this.position;
        while (isLetter(this.ch)) {
            this.readChar();
        }
        return this.input[position..<this.position];
    }

    def isLetter(byte ch) { // Possible speed bump: Eliminate argument, it's always this.ch
        return (ch >= (byte)'a' && ch <= (byte)'z') || (ch >= (byte)'A' && ch <= (byte)'Z') || ch == (byte)'_';
    }

}
