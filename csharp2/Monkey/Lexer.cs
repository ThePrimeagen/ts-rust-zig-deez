namespace Monkey;

class Lexer {
    readonly string _input;
    int _position;
    int _readPosition;
    char _ch;

    public Lexer(string input) {
        _input = input;
        ReadChar();
    }

    public Token NextToken() {
        SkipWhiteSpace();

        if (char.IsAsciiDigit(_ch))
            return new(TokenType.Int, ReadInt().ToString());

        if (char.IsAsciiLetter(_ch) || _ch == '_') {
            return ReadIdent() switch {
                "fn" => Token.Function,
                "let" => Token.Let,
                "if" => Token.If,
                "false" => Token.False,
                "true" => Token.True,
                "return" => Token.Return,
                "else" => Token.Else,
                var ident => new(TokenType.Ident, ident.ToString()),
            };
        }

        Token token = _ch switch {
            '{' => Token.LSquirly,
            '}' => Token.RSquirly,
            '(' => Token.LParen,
            ')' => Token.RParen,
            ',' => Token.Comma,
            ';' => Token.Semicolon,
            '+' => Token.Plus,
            '-' => Token.Minus,
            '!' => Peek() switch {
                '=' => Token.NotEqual,
                _ => Token.Bang,
            },
            '>' => Token.GreaterThan,
            '<' => Token.LessThan,
            '*' => Token.Asterisk,
            '/' => Token.Slash,
            '=' => Peek() switch {
                '=' => Token.Equal,
                _ => Token.Assign,
            },
            '\0' => Token.Eof,
            _ => Token.Illegal,
        };

        if (token.Type is TokenType.Equal or TokenType.NotEqual)
            ReadChar();

        ReadChar();
        return token;
    }

    char Peek() {
        if (_readPosition >= _input.Length)
            return '\0';

        return _input[_readPosition];
    }

    void ReadChar() {
        _ch = _readPosition >= _input.Length ? '\0' : _input[_readPosition];
        _position = _readPosition;
        _readPosition += 1;
    }

    void SkipWhiteSpace() {
        while (char.IsWhiteSpace(_ch))
            ReadChar();
    }

    ReadOnlySpan<char> ReadIdent() {
        var pos = _position;
        while (char.IsAsciiLetter(_ch) || _ch == '_')
            ReadChar();

        return _input.AsSpan()[pos.._position];
    }

    ReadOnlySpan<char> ReadInt() {
        var pos = _position;
        while (char.IsAsciiDigit(_ch))
            ReadChar();

        return _input.AsSpan()[pos.._position];
    }
}
