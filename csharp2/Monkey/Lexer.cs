namespace Monkey;

enum TokenType
{
    Ident,
    Int,

    Illegal,
    Eof,
    Assign,

    // Separators
    Bang,
    Dash,
    ForwardSlash,
    Asterisk,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,

    // Operators
    Plus,
    Comma,
    Semicolon,
    Lparen,
    Rparen,
    LSquirly,
    RSquirly,

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
    True,
    False,
}

readonly record struct Token(TokenType Type, string? Literal = default);

class Lexer
{
    readonly string _input;
    int _position;
    int _readPosition;
    char _ch;

    public Lexer(string input)
    {
        _input = input;
        ReadChar();
    }

    public Token NextToken()
    {
        SkipWhiteSpace();

        if (char.IsAsciiDigit(_ch))
            return new(TokenType.Int, ReadInt().ToString());

        if (char.IsAsciiLetter(_ch) || _ch == '_')
        {
            return ReadIdent() switch
            {
                "fn" => new(TokenType.Function),
                "let" => new(TokenType.Let),
                "if" => new(TokenType.If),
                "false" => new(TokenType.False),
                "true" => new(TokenType.True),
                "return" => new(TokenType.Return),
                "else" => new(TokenType.Else),
                var ident => new(TokenType.Ident, ident.ToString()),
            };
        }

        Token token = _ch switch
        {
            '{' => new(TokenType.LSquirly),
            '}' => new(TokenType.RSquirly),
            '(' => new(TokenType.Lparen),
            ')' => new(TokenType.Rparen),
            ',' => new(TokenType.Comma),
            ';' => new(TokenType.Semicolon),
            '+' => new(TokenType.Plus),
            '-' => new(TokenType.Dash),
            '!' => Peek() switch
            {
                '=' => new(TokenType.NotEqual),
                _ => new(TokenType.Bang),
            },
            '>' => new(TokenType.GreaterThan),
            '<' => new(TokenType.LessThan),
            '*' => new(TokenType.Asterisk),
            '/' => new(TokenType.ForwardSlash),
            '=' => Peek() switch
            {
                '=' => new(TokenType.Equal),
                _ => new(TokenType.Assign),
            },
            '\0' => new(TokenType.Eof),
            _ => new(TokenType.Illegal)
        };

        if (token.Type is TokenType.Equal or TokenType.NotEqual)
            ReadChar();

        ReadChar();
        return token;
    }

    char Peek()
    {
        if (_readPosition >= _input.Length)
            return '\0';

        return _input[_readPosition];
    }

    void ReadChar()
    {
        _ch = _readPosition >= _input.Length ? '\0' : _input[_readPosition];
        _position = _readPosition;
        _readPosition += 1;
    }

    void SkipWhiteSpace()
    {
        while (char.IsWhiteSpace(_ch))
            ReadChar();
    }

    ReadOnlySpan<char> ReadIdent()
    {
        var pos = _position;
        while (char.IsAsciiLetter(_ch) || _ch == '_')
            ReadChar();

        return _input.AsSpan()[pos.._position];
    }

    ReadOnlySpan<char> ReadInt()
    {
        var pos = _position;
        while (char.IsAsciiDigit(_ch))
            ReadChar();

        return _input.AsSpan()[pos.._position];
    }
}
