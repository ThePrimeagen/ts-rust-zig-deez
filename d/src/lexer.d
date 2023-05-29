module lexer;

import std.format;
import std.stdio : writeln;
import token;

struct Lexer
{
    string input;
    int position;
    int readPosition;
    byte ch;

    this(string input) {
        this.input = input;
        readChar;
    }

    void readChar()
    {
        if (readPosition >= input.length)
        {
            ch = 0;
        }
        else
        {
            ch = input[readPosition];
        }
        position = readPosition;
        readPosition += 1;
    }

    Token NextToken() {
        Token tok;
        switch(ch) {
            case '=':
                tok = Token(ASSIGN, ch);
                break;
            case ';':
                tok = Token(SEMICOLON, ch);
                break;
            case '(':
                tok = Token(LPAREN, ch);
                break;
            case ')':
                tok = Token(RPAREN, ch);
                break;
            case ',':
                tok = Token(COMMA, ch);
                break;
            case '+':
                tok = Token(PLUS, ch);
                break;
            case '{':
                tok = Token(LBRACE, ch);
                break;
            case '}':
                tok = Token(RBRACE, ch);
                break;
            case 0:
                tok = Token(EOF, ' ');
                break;
            default:
                tok = Token(ILLEGAL, ' ');
        }

        readChar;
        return tok;
    }
}

/// Test Symbols tokens
unittest
{
    const input = `=+(){},;`;

    Token[] tests = [
        Token(ASSIGN, '='),
        Token(PLUS, '+'),
        Token(LPAREN, '('),
        Token(RPAREN, ')'),
        Token(LBRACE, '{'),
        Token(RBRACE, '}'),
        Token(COMMA, ','),
        Token(SEMICOLON, ';'),
        Token(EOF, ' '),
    ];

    Lexer l = Lexer(input);

    foreach (t; tests)
    {
        const tok = l.NextToken();

        assert(tok.type == t.type,
            format("Wrong token type %s expected %s", tok.type, t.type)
        );
        assert(tok.literal == t.literal,
            format("Wrong literal %s expected %s", tok.literal, t.literal)
        );
    }
}

/// Monkey code
unittest {
    const input = `let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);`;

    Token[] tests = [
		Token(LET, "let"),
		Token(IDENT, "five"),
		Token(ASSIGN, "="),
		Token(INT, "5"),
		Token(SEMICOLON, ";"),
		Token(LET, "let"),
		Token(IDENT, "ten"),
		Token(ASSIGN, "="),
		Token(INT, "10"),
		Token(SEMICOLON, ";"),
		Token(LET, "let"),
		Token(IDENT, "add"),
		Token(ASSIGN, "="),
		Token(FUNCTION, "fn"),
		Token(LPAREN, "("),
		Token(IDENT, "x"),
		Token(COMMA, ","),
		Token(IDENT, "y"),
		Token(RPAREN, ")"),
		Token(LBRACE, "{"),
		Token(IDENT, "x"),
		Token(PLUS, "+"),
		Token(IDENT, "y"),
		Token(SEMICOLON, ";"),
		Token(RBRACE, ")"),
		Token(SEMICOLON, ";"),
		Token(LET, "let"),
		Token(IDENT, "result"),
		Token(ASSIGN, "="),
		Token(IDENT, "add"),
		Token(LPAREN, "("),
		Token(IDENT, "five"),
		Token(COMMA, ","),
		Token(IDENT, "ten"),
		Token(RPAREN, ")"),
		Token(SEMICOLON, ";"),
		Token(EOF, ' '),
    ];

    Lexer l = Lexer(input);

    foreach (t; tests)
    {
        const tok = l.NextToken();

        assert(tok.type == t.type,
            format("Wrong token type %s expected %s", tok.type, t.type)
        );
        assert(tok.literal == t.literal,
            format("Wrong literal %s expected %s", tok.literal, t.literal)
        );
    }

}
