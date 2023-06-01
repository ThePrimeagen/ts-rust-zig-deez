/**
 * Lexer and token structures based on the awesome book
 * "Writing an Interpreter in Go" by Thorsten Ball.
 *
 * Authors: ToraKuma42
 * License: MIT
 * Version: 0.0.1
 */

import std.array : appender, Appender;
import std.ascii : isAlpha, isDigit, isWhite;
import std.conv : to;
import std.format : format;
import std.range : enumerate;
import std.string : assumeUTF, representation;

/// Token type tags
enum TokenTag : ubyte
{
    Illegal,
    Eof,
    Ident,
    Int,
    Eq,
    NotEq,
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
    True,
    False,
    If,
    Else,
    Return
}

/// Token SoA tagged with starting position and type
struct TokenList
{
    Appender!(ulong[]) start; /// Starting position of tokens
    Appender!(TokenTag[]) tag; /// Type tag for tokens

    /**
     * Constructs the token multilist.
     * Params: len = The initial length of the token multilist
     */
    this(ulong len)
    {
        this.start = appender!(ulong[])(); /// Starting position of tokens
        this.tag = appender!(TokenTag[])(); /// Type tag for tokens

        const auto initialSize = (len >> 1) + (len >> 2);

        this.start.reserve(initialSize);
        this.tag.reserve(initialSize);
    }
}

/// Map reserved keywords to token types
immutable TokenTag[string] reservedKeywords;

/// Map token types to string representations
immutable string[TokenTag] tagReprs;

shared static this()
{
    import std.exception : assumeUnique;

    TokenTag[string] tempKeywords = [
        "fn": TokenTag.Function, "let": TokenTag.Let, "true": TokenTag.True,
        "false": TokenTag.False, "if": TokenTag.If, "else": TokenTag.Else,
        "return": TokenTag.Return
    ];

    string[TokenTag] tempRepr = [
        TokenTag.Eof: "\\0", TokenTag.Eq: "==", TokenTag.NotEq: "!=",
        TokenTag.Assign: "=", TokenTag.Plus: "+", TokenTag.Minus: "-",
        TokenTag.Bang: "!", TokenTag.Asterisk: "*", TokenTag.Slash: "/",
        TokenTag.Lt: "<", TokenTag.Gt: ">", TokenTag.Comma: ":",
        TokenTag.Semicolon: ";", TokenTag.LParen: "(", TokenTag.RParen: ")",
        TokenTag.LSquirly: "{", TokenTag.RSquirly: "}"
    ];

    foreach (e; tempKeywords.byKeyValue)
    {
        tempRepr[e.value] = e.key;
    }
    tempRepr.rehash;

    reservedKeywords = assumeUnique(tempKeywords);
    tagReprs = assumeUnique(tempRepr);
}

/// Encapsulates file tokenization
struct Lexer
{
private:
    ulong position = 0; /// Current character cursor
    ulong readPosition = 1; /// Read cursor (after current char)
    ulong[ulong] endPosition; /// Cache for end position of identity and number tokens

    /**
     * Tags an identifier for the new token and caches the end of identifiers.
     * Params: identifier = the identifier to map to a tag
     * Returns: The token tag
     */
    TokenTag tagForIdent(ulong start, string identifier)
    {
        if (identifier in reservedKeywords)
        {
            return reservedKeywords[identifier];
        }

        this.endPosition[start] = this.position;
        return TokenTag.Ident;
    }

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
        this.tokens = TokenList(this.input.length);
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
     * Seeks a character ahead of the current position.
     * Returns: the character ahead of the current position.
     */
    char peek()
    {
        return this.readPosition < this.input.length ? this.input[this.readPosition] : '\0';
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
            this.tokens.start.put(this.position);

            const auto nextChar = this.peek();
            if (nextChar == '=')
            {
                this.tokens.tag.put(Eq);
                this.readChar();
            }
            else
            {
                this.tokens.tag.put(Assign);
            }

            this.readChar();
            break;
        case '+':
            this.tokens.start.put(this.position);
            this.tokens.tag.put(Plus);
            this.readChar();
            break;
        case '-':
            this.tokens.start.put(this.position);
            this.tokens.tag.put(Minus);
            this.readChar();
            break;
        case '!':
            this.tokens.start.put(this.position);

            const auto nextChar = this.peek();
            if (nextChar == '=')
            {
                this.tokens.tag.put(NotEq);
                this.readChar();
            }
            else
            {
                this.tokens.tag.put(Bang);
            }

            this.readChar();
            break;
        case '/':
            this.tokens.start.put(this.position);
            this.tokens.tag.put(Slash);
            this.readChar();
            break;
        case '*':
            this.tokens.start.put(this.position);
            this.tokens.tag.put(Asterisk);
            this.readChar();
            break;
        case '<':
            this.tokens.start.put(this.position);
            this.tokens.tag.put(Lt);
            this.readChar();
            break;
        case '>':
            this.tokens.start.put(this.position);
            this.tokens.tag.put(Gt);
            this.readChar();
            break;
        case ';':
            this.tokens.start.put(this.position);
            this.tokens.tag.put(Semicolon);
            this.readChar();
            break;
        case ',':
            this.tokens.start.put(this.position);
            this.tokens.tag.put(Comma);
            this.readChar();
            break;
        case '(':
            this.tokens.start.put(this.position);
            this.tokens.tag.put(LParen);
            this.readChar();
            break;
        case ')':
            this.tokens.start.put(this.position);
            this.tokens.tag.put(RParen);
            this.readChar();
            break;
        case '{':
            this.tokens.start.put(this.position);
            this.tokens.tag.put(LSquirly);
            this.readChar();
            break;
        case '}':
            this.tokens.start.put(this.position);
            this.tokens.tag.put(RSquirly);
            this.readChar();
            break;
        case '0': .. case '9':
            const auto start = this.position;

            this.tokens.start.put(start);
            this.tokens.tag.put(Int);

            // Assume that we only have integers for now
            this.readNumber();
            this.endPosition[start] = this.position;
            break;
        case '_':
            goto case; // Explicit fallthrough to identifier scan
        case 'A': .. case 'Z':
            const auto start = this.position;

            this.tokens.start.put(start);
            this.tokens.tag.put(Ident);

            // No reserved keywords that start with uppercase letters or '_'
            this.readIdentifier();
            this.endPosition[start] = this.position;
            break;
        case 'a': .. case 'z':
            const auto start = this.position;

            // Scan for reserved keywords or identifier
            this.readIdentifier();
            const auto identSlice = this.input[start .. this.position];

            this.tokens.start.put(start);
            this.tokens.tag.put(this.tagForIdent(start, identSlice.assumeUTF));
            break;
        case '\0':
            this.tokens.start.put(this.position);
            this.tokens.tag.put(Eof);
            this.readChar();
            break;
        default:
            if (isWhite(c))
            {
                skipWhitespace();
            }
            else
            {
                this.tokens.start.put(this.position);
                this.tokens.tag.put(Illegal);
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
        while (this.position < this.input.length && isDigit(this.input[this.position]));
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
            if (this.position < this.input.length)
            {
                ch = this.input[this.position];
            }
            else
            {
                break;
            }
        }
        while (isAlpha(ch) || ch == '_');
    }

    /**
     * Shows a string representation of the token at the given index.
     * Params: index = the index of the representing token.
     * Returns: String representation of the token tag.
     */
    string tagRepr(ulong index)
    {
        const auto tag = this.tokens.tag[][index];
        if (tag in tagReprs)
        {
            return tagReprs[tag];
        }
        else if (tag == TokenTag.Ident || tag == TokenTag.Int)
        {
            const auto start = this.tokens.start[][index];
            const auto identSlice = this.input[start .. this.endPosition[start]];
            return identSlice.assumeUTF;
        }
        else if (tag == TokenTag.Illegal)
        {
            const auto start = this.tokens.start[][index];
            return "%s".format(to!char(this.input[start]));
        }

        return "";
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
        endPosition.rehash;
    }
}

/** Lexer tests */

/// Helper function to validate the lexer's ability to tokenize input strings.
private void validateTokenize(Lexer lexer, const(ulong[]) expectedStart,
        const(TokenTag[]) expectedTag)
{
    auto startPos = lexer.tokens.start[];
    auto tokenTag = lexer.tokens.tag[];

    assert(startPos.length == expectedStart.length,
            format("Token position list was %d elements; expected to be %d elements long",
                startPos.length, expectedStart.length));

    assert(tokenTag.length == expectedTag.length,
            format("Token tag list was %d elements; expected to be %d elements long",
                tokenTag.length, expectedTag.length));

    foreach (i, start; startPos.enumerate(0))
    {
        assert(start == expectedStart[i],
                format("Wrong token position %d for tag[%d]; expected %d",
                    start, i, expectedStart[i]));
    }

    foreach (i, tag; tokenTag.enumerate(0))
    {
        assert(tag == expectedTag[i],
                format("Wrong token type '%s' for tag[%d]; expected '%s'", tag, i, expectedTag[i]));
    }

}

/// Minimal lexer test
unittest
{
    const auto input = "";

    auto lexer = Lexer(input);
    lexer.tokenize();

    assert(lexer.tokens.start[].length == 0 && lexer.tokens.tag[].length == 0,
            "Token list must be empty for empty string");
}

/// Empty input lexer test
unittest
{
    const auto input = "  ";

    auto lexer = Lexer(input);
    lexer.tokenize();

    assert(lexer.tokens.start[].length == 0 && lexer.tokens.tag[].length == 0,
            "Token list must be empty for empty string");
}

/// Basic lexer test
unittest
{
    const auto input = "=+(){},;";

    with (TokenTag)
    {
        const ulong[] expectedStart = [0, 1, 2, 3, 4, 5, 6, 7];

        const auto expectedTag = [
            Assign, Plus, LParen, RParen, LSquirly, RSquirly, Comma, Semicolon
        ];

        auto lexer = Lexer(input);
        lexer.tokenize();

        validateTokenize(lexer, expectedStart, expectedTag);
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
        const ulong[] expectedStart = [
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

        validateTokenize(lexer, expectedStart, expectedTag);
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
        const ulong[] expectedStart = [
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

        validateTokenize(lexer, expectedStart, expectedTag);
    }
}

/// Lexer test with even more keywords
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

if (5 < 10) {
  return true;
} else {
  return false;
}
";

    with (TokenTag)
    {
        const ulong[] expectedStart = [
            0, 4, 9, 11, 12, 14, 18, 22, 24, 26, 28, 32, 36, 38, 40, 41, 42,
            44, 45, 47, 51, 53, 55, 56, 58, 59, 61, 65, 72, 74, 77, 78, 82,
            84, 87, 88, 90, 91, 92, 93, 94, 95, 97, 99, 101, 104, 106, 107,
            110, 113, 114, 116, 118, 120, 122, 126, 133, 137, 139, 141, 146, 150,
            157, 162, 164
        ];

        const auto expectedTag = [
            Let, Ident, Assign, Int, Semicolon, Let, Ident, Assign, Int,
            Semicolon, Let, Ident, Assign, Function, LParen, Ident, Comma, Ident,
            RParen, LSquirly, Ident, Plus, Ident, Semicolon, RSquirly,
            Semicolon, Let, Ident, Assign, Ident, LParen, Ident, Comma, Ident,
            RParen, Semicolon, Bang, Minus, Slash, Asterisk, Int, Semicolon,
            Int, Lt, Int, Gt, Int, Semicolon, If, LParen, Int, Lt, Int, RParen,
            LSquirly, Return, True, Semicolon, RSquirly, Else, LSquirly,
            Return, False, Semicolon, RSquirly
        ];

        auto lexer = Lexer(input);
        lexer.tokenize();

        validateTokenize(lexer, expectedStart, expectedTag);
    }
}

/// Ultimate lexer test with digram symbols
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

if (5 < 10) {
  return true;
} else {
  return false;
}

10 == 10;
10 != 9;
";

    with (TokenTag)
    {
        const ulong[] expectedStart = [
            0, 4, 9, 11, 12, 14, 18, 22, 24, 26, 28, 32, 36, 38, 40, 41, 42,
            44, 45, 47, 51, 53, 55, 56, 58, 59, 61, 65, 72, 74, 77, 78, 82,
            84, 87, 88, 90, 91, 92, 93, 94, 95, 97, 99, 101, 104, 106, 107,
            110, 113, 114, 116, 118, 120, 122, 126, 133, 137, 139, 141, 146,
            150, 157, 162, 164, 167, 170, 173, 175, 177, 180, 183, 184
        ];

        const auto expectedTag = [
            Let, Ident, Assign, Int, Semicolon, Let, Ident, Assign, Int,
            Semicolon, Let, Ident, Assign, Function, LParen, Ident, Comma, Ident,
            RParen, LSquirly, Ident, Plus, Ident, Semicolon, RSquirly,
            Semicolon, Let, Ident, Assign, Ident, LParen, Ident, Comma, Ident,
            RParen, Semicolon, Bang, Minus, Slash, Asterisk, Int, Semicolon,
            Int, Lt, Int, Gt, Int, Semicolon, If, LParen, Int, Lt, Int, RParen,
            LSquirly, Return, True, Semicolon, RSquirly, Else, LSquirly,
            Return, False, Semicolon, RSquirly, Int, Eq, Int, Semicolon, Int,
            NotEq, Int, Semicolon,
        ];

        auto lexer = Lexer(input);
        lexer.tokenize();

        validateTokenize(lexer, expectedStart, expectedTag);
    }
}
