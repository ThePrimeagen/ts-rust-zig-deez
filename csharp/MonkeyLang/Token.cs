namespace MonkeyLang;

public enum TokenType
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
    Identifier,
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

public readonly record struct Token(TokenType Type, string? Literal = default)
{
    public override string ToString()
    {
        if (Literal is null)
            return Type.ToString();

        return $"{Type}, {Literal}";
    }
}