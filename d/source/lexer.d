/**
 * Lexer and token structures based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import std.ascii;
import std.range : enumerate;
import std.string : assumeUTF, representation;

/// Token type tags
enum Token : ubyte
{
    Illegal,
    Eof,
    Ident,
    Int,
    Equal,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LSquirly,
    RSquirly,
    Function,
    Let,
}

/// Encapsulates file tokenization
struct Lexer
{
private:
    ulong position = 0; /// Current character cursor
    ulong readPosition = 1; /// Read cursor (after current char)

public:
    immutable(ubyte)[] input; /// Input string
    ulong[] tokenStart; /// Starting position of tokens
    Token[] tokenTag; /// Type tag for tokens

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
        ubyte c = this.input[this.position];
        switch (c)
        {
        case '=':
            this.tokenTag ~= Token.Equal;
            this.tokenStart ~= this.position;
            this.readChar();
            break;
        case '+':
            this.tokenTag ~= Token.Plus;
            this.tokenStart ~= this.position;
            this.readChar();
            break;
        case ',':
            this.tokenTag ~= Token.Comma;
            this.tokenStart ~= this.position;
            this.readChar();
            break;
        case ';':
            this.tokenTag ~= Token.Semicolon;
            this.tokenStart ~= this.position;
            this.readChar();
            break;
        case '(':
            this.tokenTag ~= Token.LParen;
            this.tokenStart ~= this.position;
            this.readChar();
            break;
        case ')':
            this.tokenTag ~= Token.RParen;
            this.tokenStart ~= this.position;
            this.readChar();
            break;
        case '{':
            this.tokenTag ~= Token.LSquirly;
            this.tokenStart ~= this.position;
            this.readChar();
            break;
        case '}':
            this.tokenTag ~= Token.RSquirly;
            this.tokenStart ~= this.position;
            this.readChar();
            break;
        case '0': .. case '9':
            this.tokenTag ~= Token.Int;
            this.tokenStart ~= this.position;

            // Assume that we only have integers for now
            this.readNumber();
            break;
        case '_':
            goto case; // Explicit fallthrough to identifier scan
        case 'A': .. case 'Z':
            this.tokenTag ~= Token.Ident;
            this.tokenStart ~= this.position;

            // No reserved keywords that start with uppercase letters or '_'
            this.readIdentifier();
            break;
        case 'a': .. case 'z':
            auto tag = Token.Ident;
            const auto start = this.position;
            this.tokenStart ~= start;

            // Scan for reserved keywords or identifier
            this.readIdentifier();
            const auto identSlice = this.input[start .. this.position];

            switch (identSlice.assumeUTF)
            {
            case "fn":
                tag = Token.Function;
                break;
            case "let":
                tag = Token.Let;
                break;
            default:
                break;
            }

            this.tokenTag ~= tag;
            break;
        case '\0':
            this.tokenTag ~= Token.Eof;
            this.tokenStart ~= this.position;
            this.readChar();
            break;
        default:
            if (!isWhite(c))
            {
                this.tokenTag ~= Token.Illegal;
                this.tokenStart ~= this.position;
            }
            this.readChar();
        }
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
        ubyte ch;
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

/// Basic lexer test
unittest
{
    const auto input = "=+(){},;";
    with (Token)
    {
        const auto expected = [
            Equal, Plus, LParen, RParen, LSquirly, RSquirly, Comma, Semicolon, Eof
        ];

        auto lexer = Lexer(input);
        lexer.tokenize();

        foreach (i, tag; lexer.tokenTag.enumerate(0))
        {
            assert(tag == expected[i]);
        }
    }
}

/// Complete lexer test
unittest
{
    const auto input = "
  let five = 5;
  let ten = 10;
  let add = fn(x, y) {
    x + y;
  };
  let result = add(five, ten);
  ";

    with (Token)
    {
        const auto expected = [
            Let, Ident, Equal, Int, Semicolon, Let, Ident, Equal, Int,
            Semicolon, Let, Ident, Equal, Function, LParen, Ident, Comma, Ident,
            RParen, LSquirly, Ident, Plus, Ident, Semicolon, RSquirly,
            Semicolon, Let, Ident, Equal, Ident, LParen, Ident, Comma, Ident,
            RParen, Semicolon, Eof
        ];

        auto lexer = Lexer(input);
        lexer.tokenize();

        foreach (i, tag; lexer.tokenTag.enumerate(0))
        {
            assert(tag == expected[i]);
        }
    }
}
