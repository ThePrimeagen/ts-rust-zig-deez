namespace Monkey.Lexing;

public static class Tokens
{
    public static IToken Illegal(char value) => new IllegalToken(value);
    public static readonly IToken EoF = new EoFToken();

    public static IToken Identifier(string value) => new IdentifierToken(value);
    public static IToken Integer(int value) => new IntegerToken(value);

    public static readonly IToken Assign = new AssignToken();
    public static readonly IToken Plus = new PlusToken();
    public static readonly IToken Dash = new DashToken();
    public static readonly IToken Bang = new BangToken();
    public static readonly IToken Asterisk = new AsteriskToken();
    public static readonly IToken ForwardSlash = new ForwardSlashToken();
    public static readonly IToken LessThan = new LessThanToken();
    public static readonly IToken GreaterThan = new GreaterThanToken();
    public static readonly IToken NotEqual = new NotEqualToken();
    public static readonly IToken Equal = new EqualToken();

    public static readonly IToken Comma = new CommaToken();
    public static readonly IToken Semicolon = new SemicolonToken();

    public static readonly IToken LParen = new LParenToken();
    public static readonly IToken RParen = new RParenToken();
    public static readonly IToken LSquirly = new LSquirlyToken();
    public static readonly IToken RSquirly = new RSquirlyToken();

    public static readonly IToken Function = new FunctionToken();
    public static readonly IToken Let = new LetToken();
    public static readonly IToken True = new TrueToken();
    public static readonly IToken False = new FalseToken();
    public static readonly IToken If = new IfToken();
    public static readonly IToken Else = new ElseToken();
    public static readonly IToken Return = new ReturnToken();

    public static T OutOfRange<T>(string parameter) => throw new ArgumentOutOfRangeException(parameter);
}

public interface IToken {}
public readonly record struct IllegalToken(char Value): IToken;
public readonly record struct EoFToken: IToken;
public readonly record struct IdentifierToken(string Identifier): IToken;
public readonly record struct IntegerToken(int Integer): IToken;
public readonly record struct AssignToken: IToken;
public readonly record struct PlusToken: IToken;

public readonly record struct DashToken: IToken;
public readonly record struct BangToken: IToken;
public readonly record struct AsteriskToken: IToken;
public readonly record struct ForwardSlashToken: IToken;
public readonly record struct LessThanToken: IToken;
public readonly record struct GreaterThanToken: IToken;
public readonly record struct NotEqualToken: IToken;
public readonly record struct EqualToken: IToken;
public readonly record struct CommaToken: IToken;
public readonly record struct SemicolonToken: IToken;
public readonly record struct LParenToken: IToken;
public readonly record struct RParenToken: IToken;
public readonly record struct LSquirlyToken: IToken;
public readonly record struct RSquirlyToken: IToken;
public readonly record struct FunctionToken: IToken;
public readonly record struct LetToken: IToken;
public readonly record struct TrueToken: IToken;
public readonly record struct FalseToken: IToken;
public readonly record struct IfToken: IToken;
public readonly record struct ElseToken: IToken;
public readonly record struct ReturnToken: IToken;
