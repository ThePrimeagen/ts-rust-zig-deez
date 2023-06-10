namespace Monkey;

enum TokenType
{
    Illegal,
    Eof,

    // Identifiers
    Ident,
    Int,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LessThan,
    GreaterThan,
    Equal,
    NotEqual,

    // Delimiters
    Comma,
    Semicolon,
    LParen,
    RParen,
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

readonly record struct Token(TokenType Type, string? Literal = null)
{
    internal static Token Illegal = new(TokenType.Illegal, "ILLEGAL");
    internal static Token Eof = new(TokenType.Eof, "EOF");

    internal static Token Assign = new(TokenType.Assign, "=");
    internal static Token Plus = new(TokenType.Plus, "+");
    internal static Token Minus = new(TokenType.Minus, "-");
    internal static Token Bang = new(TokenType.Bang, "!");
    internal static Token Asterisk = new(TokenType.Asterisk, "*");
    internal static Token Slash = new(TokenType.Slash, "/");
    internal static Token LessThan = new(TokenType.LessThan, "<");
    internal static Token GreaterThan = new(TokenType.GreaterThan, ">");
    internal static Token Equal = new(TokenType.Equal, "==");
    internal static Token NotEqual = new(TokenType.NotEqual, "!=");

    internal static Token Comma = new(TokenType.Comma, ",");
    internal static Token Semicolon = new(TokenType.Semicolon, ";");
    internal static Token LParen = new(TokenType.LParen, "(");
    internal static Token RParen = new(TokenType.RParen, ")");
    internal static Token LSquirly = new(TokenType.LSquirly, "{");
    internal static Token RSquirly = new(TokenType.RSquirly, "}");

    internal static Token Function = new(TokenType.Function, "fn");
    internal static Token Let = new(TokenType.Let, "let");
    internal static Token If = new(TokenType.If, "if");
    internal static Token Else = new(TokenType.Else, "else");
    internal static Token Return = new(TokenType.Return, "return");
    internal static Token True = new(TokenType.True, "true");
    internal static Token False = new(TokenType.False, "false");
}
