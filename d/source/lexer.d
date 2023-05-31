/**
 * Lexer and token structures based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import std.ascii;
import std.format;
import std.range : enumerate;
import std.string : assumeUTF, representation;

/// Token type tags
enum TokenTag : ubyte
{
    Illegal,
    Eof,
    Ident,
    Int,
    Equal,
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LSquirly,
    RSquirly,
    Function,
    Let,
}

/// Token SoA tagged with starting position and type
struct TokenList
{
    ulong[] start; /// Starting position of tokens
    TokenTag[] tag; /// Type tag for tokens
}

private enum TokenTag[string] reservedKeywords = [
    "fn": TokenTag.Function, "let": TokenTag.Let
];

private TokenTag tagForIdent(string identifier)
{
    if (identifier in reservedKeywords)
    {
        return reservedKeywords[identifier];
    }

    return TokenTag.Ident;
}

/// Encapsulates file tokenization
struct Lexer
{
private:
    ulong position = 0; /// Current character cursor
    ulong readPosition = 1; /// Read cursor (after current char)

public:
    immutable(ubyte)[] input; /// Input string
    TokenList tokens; /// Tokens in input

    /**
     * Constructs the lexer.
     * Params: text = the input string
     */
    this(string text)
    {
        this.input = representation(text);
    }

    /**
     * Advances in a character from the input.
     */
    void readChar()
    {
        this.position = this.readPosition;
        this.readPosition++;
    }

    /**
     * Scan the next token at the current position in the input.
     */
    void nextToken()
    {
        const auto c = this.input[this.position];
        switch (c) with (TokenTag)
        {
        case '=':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= Assign;
            this.readChar();
            break;
        case '+':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= Plus;
            this.readChar();
            break;
        case '-':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= Minus;
            this.readChar();
            break;
        case '!':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= Bang;
            this.readChar();
            break;
        case '/':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= Slash;
            this.readChar();
            break;
        case '*':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= Asterisk;
            this.readChar();
            break;
        case '<':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= Lt;
            this.readChar();
            break;
        case '>':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= Gt;
            this.readChar();
            break;
        case ';':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= Semicolon;
            this.readChar();
            break;
        case ',':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= Comma;
            this.readChar();
            break;
        case '(':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= LParen;
            this.readChar();
            break;
        case ')':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= RParen;
            this.readChar();
            break;
        case '{':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= LSquirly;
            this.readChar();
            break;
        case '}':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= RSquirly;
            this.readChar();
            break;
        case '0': .. case '9':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= Int;

            // Assume that we only have integers for now
            this.readNumber();
            break;
        case '_':
            goto case; // Explicit fallthrough to identifier scan
        case 'A': .. case 'Z':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= Ident;

            // No reserved keywords that start with uppercase letters or '_'
            this.readIdentifier();
            break;
        case 'a': .. case 'z':
            const auto start = this.position;

            // Scan for reserved keywords or identifier
            this.readIdentifier();
            const auto identSlice = this.input[start .. this.position];

            this.tokens.start ~= start;
            this.tokens.tag ~= tagForIdent(identSlice.assumeUTF);
            break;
        case '\0':
            this.tokens.start ~= this.position;
            this.tokens.tag ~= Eof;
            this.readChar();
            break;
        default:
            if (isWhite(c))
            {
                skipWhitespace();
            }
            else
            {
                this.tokens.start ~= this.position;
                this.tokens.tag ~= Illegal;
                this.readChar();
            }
        }
    }

    /**
     * Skip until there is no more whitespace.
     */
    void skipWhitespace()
    {
        do
        {
            this.readChar();
        }
        while (this.position < this.input.length && isWhite(this.input[this.position]));
    }

    /**
     * Scan number when tokenizer detects 0-9.
     */
    void readNumber()
    {
        do
        {
            this.readChar();
        }
        while (isDigit(this.input[this.position]));
    }

    /**
     * Scan identifier when tokenizer detects A-Z, a-z, or '_'.
     */
    void readIdentifier()
    {
        char ch;
        do
        {
            this.readChar();
            ch = this.input[this.position];
        }
        while (isAlpha(ch) || ch == '_');
    }

    /**
     * Scan entire input for tokens.
     */
    void tokenize()
    {
        while (this.position < this.input.length)
        {
            this.nextToken();
        }
    }
}

/** Lexer tests */

/// Minimal lexer test
unittest
{
    const auto input = "";

    auto lexer = Lexer(input);
    lexer.tokenize();

    assert(lexer.tokens.start.length == 0 && lexer.tokens.tag.length == 0,
            "Token list must be empty for empty string");
}

/// Empty input lexer test
unittest
{
    const auto input = "  ";

    auto lexer = Lexer(input);
    lexer.tokenize();

    assert(lexer.tokens.start.length == 0 && lexer.tokens.tag.length == 0,
            "Token list must be empty for empty string");
}

/// Basic lexer test
unittest
{
    const auto input = "=+(){},;";

    with (TokenTag)
    {
        const auto expectedStart = [0, 1, 2, 3, 4, 5, 6, 7];

        const auto expectedTag = [
            Assign, Plus, LParen, RParen, LSquirly, RSquirly, Comma, Semicolon
        ];

        auto lexer = Lexer(input);
        lexer.tokenize();

        assert(lexer.tokens.start.length == expectedStart.length,
                format("Token position list was %d elements; expected to be %d elements long",
                    lexer.tokens.start.length, expectedStart.length));

        assert(lexer.tokens.tag.length == expectedTag.length,
                format("Token tag list was %d elements; expected to be %d elements long",
                    lexer.tokens.tag.length, expectedTag.length));

        foreach (i, start; lexer.tokens.start.enumerate(0))
        {
            assert(start == expectedStart[i],
                    format("Wrong token position %d for tag[%d]; expected %d",
                        start, i, expectedStart[i]));
        }

        foreach (i, tag; lexer.tokens.tag.enumerate(0))
        {
            assert(tag == expectedTag[i],
                    format("Wrong token type '%s' for tag[%d]; expected '%s'",
                        tag, i, expectedTag[i]));
        }
    }
}

/// Complete lexer test
unittest
{
    const auto input = "let five = 5;
let ten = 10;
let add = fn(x, y) {
  x + y;
};
let result = add(five, ten);
";

    with (TokenTag)
    {
        const auto expectedStart = [
            0, 4, 9, 11, 12, 14, 18, 22, 24, 26, 28, 32, 36, 38, 40, 41, 42,
            44, 45, 47, 51, 53, 55, 56, 58, 59, 61, 65, 72, 74, 77, 78, 82, 84, 87,
            88
        ];

        const auto expectedTag = [
            Let, Ident, Assign, Int, Semicolon, Let, Ident, Assign, Int,
            Semicolon, Let, Ident, Assign, Function, LParen, Ident, Comma, Ident,
            RParen, LSquirly, Ident, Plus, Ident, Semicolon, RSquirly,
            Semicolon, Let, Ident, Assign, Ident, LParen, Ident, Comma, Ident,
            RParen, Semicolon
        ];

        auto lexer = Lexer(input);
        lexer.tokenize();

        assert(lexer.tokens.start.length == expectedStart.length,
                format("Token position list was %d elements; expected to be %d elements long",
                    lexer.tokens.start.length, expectedStart.length));

        assert(lexer.tokens.tag.length == expectedTag.length,
                format("Token tag list was %d elements; expected to be %d elements long",
                    lexer.tokens.tag.length, expectedTag.length));

        foreach (i, start; lexer.tokens.start.enumerate(0))
        {
            assert(start == expectedStart[i],
                    format("Wrong token position %d for tag[%d]; expected %d",
                        start, i, expectedStart[i]));
        }

        foreach (i, tag; lexer.tokens.tag.enumerate(0))
        {
            assert(tag == expectedTag[i],
                    format("Wrong token type '%s' for tag[%d]; expected '%s'",
                        tag, i, expectedTag[i]));
        }
    }
}

/// More complete lexer test
unittest
{
    const auto input = "let five = 5;
let ten = 10;
let add = fn(x, y) {
  x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;
";

    with (TokenTag)
    {
        const auto expectedStart = [
            0, 4, 9, 11, 12, 14, 18, 22, 24, 26, 28, 32, 36, 38, 40, 41, 42,
            44, 45, 47, 51, 53, 55, 56, 58, 59, 61, 65, 72, 74, 77, 78, 82,
            84, 87, 88, 90, 91, 92, 93, 94, 95, 97, 99, 101, 104, 106, 107
        ];

        const auto expectedTag = [
            Let, Ident, Assign, Int, Semicolon, Let, Ident, Assign, Int,
            Semicolon, Let, Ident, Assign, Function, LParen, Ident, Comma, Ident,
            RParen, LSquirly, Ident, Plus, Ident, Semicolon, RSquirly,
            Semicolon, Let, Ident, Assign, Ident, LParen, Ident, Comma, Ident,
            RParen, Semicolon, Bang, Minus, Slash, Asterisk, Int, Semicolon,
            Int, Lt, Int, Gt, Int, Semicolon
        ];

        auto lexer = Lexer(input);
        lexer.tokenize();

        assert(lexer.tokens.start.length == expectedStart.length,
                format("Token position list was %d elements; expected to be %d elements long",
                    lexer.tokens.start.length, expectedStart.length));

        assert(lexer.tokens.tag.length == expectedTag.length,
                format("Token tag list was %d elements; expected to be %d elements long",
                    lexer.tokens.tag.length, expectedTag.length));

        foreach (i, start; lexer.tokens.start.enumerate(0))
        {
            assert(start == expectedStart[i],
                    format("Wrong token position %d for tag[%d]; expected %d",
                        start, i, expectedStart[i]));
        }

        foreach (i, tag; lexer.tokens.tag.enumerate(0))
        {
            assert(tag == expectedTag[i],
                    format("Wrong token type '%s' for tag[%d]; expected '%s'",
                        tag, i, expectedTag[i]));
        }
    }
}
