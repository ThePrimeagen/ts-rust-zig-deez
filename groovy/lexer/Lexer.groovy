package lexer

/**
 * a token type
 */
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

/**
 * represents a token in the lexer.
 */
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

/**
 * a lexer that tokenizes a string input
 */
class Lexer {

    private static final int ZERO = 0
    private static final String NULL_CHAR = '\0'
    private static final String EQUAL_SIGN = '='

    private int position = ZERO
    private int readPosition = ZERO
    private String ch
    private final String input

    Lexer(String input) {
        this.input = input
        readChar()
    }

    Token nextToken() {
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
                if (peek() == EQUAL_SIGN) {
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
            case EQUAL_SIGN:
                if (peek() == EQUAL_SIGN) {
                    readChar()
                    token = new Token(TokenType.EQUAL)
                } else {
                    token = new Token(TokenType.ASSIGN)
                }
                break
            case NULL_CHAR:
                token = new Token(TokenType.EOF)
                break
        }

        if (isLetter(ch)) {
            String ident = readIdent()
            Token keyword = keywords[ident]
            return keyword ?: new Token(TokenType.IDENT, ident)
        } else if (isNumber(ch)) {
            return new Token(TokenType.INT, readInt())
        } else if (!token) {
            return new Token(TokenType.ILLEGAL, ch)
        }

        readChar()
        return token
    }

    private String peek() {
        return readPosition >= input.length() ? NULL_CHAR : input[readPosition]
    }

    private void skipWhitespace() {
        while (ch in [' ', '\t', '\n', '\r']) {
            readChar()
        }
    }

    private void readChar() {
        ch = readPosition >= input.length() ? NULL_CHAR : input[readPosition]
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
        return character[ZERO] in ('a'..'z') + ('A'..'Z') + '_'
    }

    private boolean isNumber(String character) {
        return character[ZERO] in ('0'..'9')
    }

    private final Map<String, Token> keywords = [
        'fn': new Token(TokenType.FUNCTION),
        'let': new Token(TokenType.LET),
        'return': new Token(TokenType.RETURN),
        'true': new Token(TokenType.TRUE),
        'false': new Token(TokenType.FALSE),
        'if': new Token(TokenType.IF),
        'else': new Token(TokenType.ELSE)
    ]

}
