enum TokenType {

    ILLEGAL,
    EOF,
    IDENT,
    IF,
    RETURN,
    TRUE,
    FALSE,
    ELSE,
    INT,
    ASSIGN,
    NOT_EQUAL,
    EQUAL,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
    BANG,
    DASH,
    FORWARD_SLASH,
    ASTERISK,
    LESS_THAN,
    GREATER_THAN

}


class Token {

    TokenType type
    String literal

    Token(TokenType type, String literal) {
        this.type = type
        this.literal = literal
    }

    Token(TokenType type) {
        this.type = type
        this.literal = null
    }

}


class Lexer {

    private int position = 0
    private int readPosition = 0
    private String ch
    private final String input

    Lexer(String input) {
        this.input = input
        readChar()
    }

    Token getNextToken() {
        skipWhitespace()

        Token token
        switch (ch) {
            case '{':
                token = new Token(TokenType.LBRACE)
                break
            case '}':
                token = new Token(TokenType.RBRACE)
                break
            case '(':
                token = new Token(TokenType.LPAREN)
                break
            case ')':
                token = new Token(TokenType.RPAREN)
                break
            case ',':
                token = new Token(TokenType.COMMA)
                break
            case '!':
                if (peek() == '=') {
                    readChar()
                    token = new Token(TokenType.NOT_EQUAL)
                } else {
                    token = new Token(TokenType.BANG)
                }
                break
            case '>':
                token = new Token(TokenType.GREATER_THAN)
                break
            case '<':
                token = new Token(TokenType.LESS_THAN)
                break
            case '*':
                token = new Token(TokenType.ASTERISK)
                break
            case '/':
                token = new Token(TokenType.FORWARD_SLASH)
                break
            case '-':
                token = new Token(TokenType.DASH)
                break
            case ';':
                token = new Token(TokenType.SEMICOLON)
                break
            case '+':
                token = new Token(TokenType.PLUS)
                break
            case '=':
                if (peek() == '=') {
                    readChar()
                    token = new Token(TokenType.EQUAL)
                } else {
                    token = new Token(TokenType.ASSIGN)
                }
                break
            case '\0':
                token = new Token(TokenType.EOF)
                break
        }

        if (isLetter(ch)) {
            String ident = readIdent()
            Token keyword = getKeywords()[ident]
            if (keyword) {
                return keyword
            } else {
                return new Token(TokenType.IDENT, ident)
            }
        } else if (isNumber(ch)) {
            return new Token(TokenType.INT, readInt())
        } else if (!token) {
            return new Token(TokenType.ILLEGAL, ch)
        }

        readChar()
        return token
    }

    private String peek() {
        return readPosition >= input.length() ? '\0' : input[readPosition]
    }

    private void skipWhitespace() {
        while (ch in [' ', '\t', '\n', '\r']) {
            readChar()
        }
    }

    private void readChar() {
        ch = readPosition >= input.length() ? '\0' : input[readPosition]
        position = readPosition
        readPosition++
    }

    private String readIdent() {
        int position = this.position

        while (isLetter(ch)) {
            readChar()
        }

        return input[position..this.position - 1]
    }

    private String readInt() {
        int position = this.position

        while (isNumber(ch)) {
            readChar()
        }

        return input[position..this.position - 1]
    }

    private boolean isLetter(String character) {
        return character[0] in ('a'..'z') + ('A'..'Z') + '_'
    }

    private boolean isNumber(String character) {
        return character[0] in ('0'..'9')
    }

    private Map<String, Token> getKeywords() {
        return [
            'fn': new Token(TokenType.FUNCTION),
            'let': new Token(TokenType.LET),
            'return': new Token(TokenType.RETURN),
            'true': new Token(TokenType.TRUE),
            'false': new Token(TokenType.FALSE),
            'if': new Token(TokenType.IF),
            'else': new Token(TokenType.ELSE)
        ]
    }

}