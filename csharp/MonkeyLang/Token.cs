namespace MonkeyLang;

public enum Token
{
    Assign,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Bang,
    LT,
    GT,
    EQ,
    NOT_EQ,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LSquirly,
    RSquirly,
    Ident,
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
    Integer,
    String,
    Illegal,
    Eof
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