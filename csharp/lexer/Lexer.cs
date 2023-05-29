namespace lexer;

public struct Lexer
{
    private int _position;
    private int _read_position;
    private char _current_char;
    private readonly ReadOnlyMemory<char> _input;

    private ReadOnlySpan<char> _inputSpan { get { return _input.Span; } }

    public Lexer(string input)
    {
        _position = 0;
        _read_position = 0;
        _current_char = '\0';
        _input = input.AsMemory();

        ReadCharacter();
    }

    private void ReadCharacter()
    {
        if(_read_position >= _input.Length)
        {
            _current_char = '\0';
        }
        else
        {
            _current_char = _inputSpan[_read_position];
        }

        _position = _read_position;
        _read_position++;
    }

    private void SkipWhitespace()
    {
        while(char.IsWhiteSpace(_current_char))
        {
            ReadCharacter();
        }
    }

    private ReadOnlySpan<char> ReadIdent()
    {
        var start = _position;

        while(char.IsLetter(_current_char) || _inputSpan[_read_position] == '_')
        {
            ReadCharacter();
        }

        return _inputSpan[start.._position];
    }

    private ReadOnlySpan<char> ReadString()
    {
        if(_current_char == '\"')
        {
            ReadCharacter();
        }

        var start = _position;

        while(_current_char != '\0' && _current_char != '\"')
        {
            ReadCharacter();
        }

        if(_current_char == '\0')
        {
            throw new Exception("Unterminated string literal.");
        }

        var stringValue = _inputSpan[start.._position];

        ReadCharacter();

        return stringValue;
    }

    private int ReadInt()
    {
        var start = _position;

        while(char.IsDigit(_current_char))
        {
            ReadCharacter();
        }

        return int.Parse(_inputSpan[start.._position]);
    }

    public TokenInfo NextToken()
    {
        SkipWhitespace();

        if(_current_char == '\"')
        {
            return new TokenInfo(Token.String, ReadString().ToString());
        }

        if (char.IsLetter(_current_char) || _current_char == '_')
        {
            var ident = ReadIdent();
            return ident switch
            {
                "fn" => new TokenInfo(Token.Function),
                "let" => new TokenInfo(Token.Let),
                _ => new TokenInfo(Token.Ident, ident.ToString())
            };
        }

        if(char.IsDigit(_current_char))
        {
            return new TokenInfo(Token.Integer, ReadInt());
        }

        var token = _current_char switch
        {
            '=' => new TokenInfo(Token.Equal),
            '+' => new TokenInfo(Token.Plus),
            ',' => new TokenInfo(Token.Comma),
            ';' => new TokenInfo(Token.Semicolon),
            '(' => new TokenInfo(Token.LParen),
            ')' => new TokenInfo(Token.RParen),
            '{' => new TokenInfo(Token.LSquirly),
            '}' => new TokenInfo(Token.RSquirly),
            '\0' => new TokenInfo(Token.Eof),
            _ => new TokenInfo(Token.Illegal, _current_char)
        };

        ReadCharacter();

        return token;
    }

    public IEnumerable<TokenInfo> ParseTokens()
    {
        TokenInfo tok;
        while((tok = NextToken()).Type != Token.Eof)
        {
            yield return tok;
        }

        yield return tok;
    }
}