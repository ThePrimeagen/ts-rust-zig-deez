namespace MonkeyLang;

public struct Lexer
{
    private int position;
    private int readPosition;
    private char currentChar;
    private readonly string input;

    public Lexer(string input)
    {
        this.input = input;

        ReadChar();
    }

    public IEnumerable<Token> ParseTokens()
    {
        Token token;
        do
        {
            yield return token = NextToken();

        } while (token.Type is not TokenType.Eof);
    }

    public Token NextToken()
    {
        SkipWhitespace();

        if (currentChar is '\"')
            return new Token(TokenType.String, ReadString().ToString());

        if (char.IsLetter(currentChar) || currentChar is '_')
        {
            var identifier = ReadIdentifier();
            return identifier switch
            {
                "fn" => new Token(TokenType.Function),
                "let" => new Token(TokenType.Let),
                "true" => new Token(TokenType.True),
                "false" => new Token(TokenType.False),
                "if" => new Token(TokenType.If),
                "else" => new Token(TokenType.Else),
                "return" => new Token(TokenType.Return),
                _ => new Token(TokenType.Identifier, identifier.ToString())
            };
        }

        if (char.IsDigit(currentChar))
            return new Token(TokenType.Integer, ReadInt());

        var token = currentChar switch
        {
            '=' => PeekChar() switch
            {
                '=' => SkipAndReturn(new Token(TokenType.EQ)),
                _ => new Token(TokenType.Assign),
            }
            ,
            '!' => PeekChar() switch
            {
                '=' => SkipAndReturn(new Token(TokenType.NOT_EQ)),
                _ => new Token(TokenType.Bang),
            },
            '-' => new Token(TokenType.Minus),
            '/' => new Token(TokenType.Slash),
            '*' => new Token(TokenType.Asterisk),
            '<' => new Token(TokenType.LT),
            '>' => new Token(TokenType.GT),
            '+' => new Token(TokenType.Plus),
            ',' => new Token(TokenType.Comma),
            ';' => new Token(TokenType.Semicolon),
            '(' => new Token(TokenType.LParen),
            ')' => new Token(TokenType.RParen),
            '{' => new Token(TokenType.LSquirly),
            '}' => new Token(TokenType.RSquirly),
            '\0' => new Token(TokenType.Eof),
            _ => new Token(TokenType.Illegal, input[position..readPosition]),
        };

        ReadChar();

        return token;
    }

    private void ReadChar()
    {
        currentChar = PeekChar();
        position = readPosition;
        readPosition++;
    }

    private readonly char PeekChar()
    {
        if (readPosition >= input.Length)
            return '\0';

        return input[readPosition];
    }

    private void SkipWhitespace()
    {
        while (char.IsWhiteSpace(currentChar))
        {
            ReadChar();
        }
    }

    private ReadOnlySpan<char> ReadIdentifier()
    {
        int start = position;

        while (char.IsLetter(currentChar) || currentChar is '_')
        {
            ReadChar();
        }

        return input.AsSpan(start, position - start);
    }

    private ReadOnlySpan<char> ReadString()
    {
        if (currentChar is '\"')
            ReadChar();

        int start = position;

        while (currentChar is not '\0' and not '\"')
        {
            ReadChar();
        }

        if (currentChar is '\0')
            throw new Exception("Unterminated string literal.");

        ReadChar();

        return input.AsSpan(start, position - start - 1);
    }

    private string ReadInt()
    {
        int start = position;

        while (char.IsDigit(currentChar))
        {
            ReadChar();
        }

        return input[start..position].ToString();
    }

    private Token SkipAndReturn(Token tokenInfo)
    {
        ReadChar();
        return tokenInfo;
    }
}