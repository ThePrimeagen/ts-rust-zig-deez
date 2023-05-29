namespace lexer;

public enum Token
{
    Ident,
    Integer,
    Let,
    Illegal,
    Eof,
    Equal,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LSquirly,
    RSquirly,
    Function,
    String
}

public readonly struct TokenInfo
{
    public Token Type { get; }
    public object? Value { get; }

    public TokenInfo(Token type, object? value)
    {
        Type = type;
        Value = value;
    }

    public TokenInfo(Token type)
    {
        Type = type;
        Value = null;
    }

    public override string ToString()
    {
        if (Value is null)
        {
            return Type.ToString();
        }

        return $"{Type}, {Value}";
    }
}