namespace Monkey;

public abstract record Token
{
    public static Token Illegal(char value) => new Illegal(value);
    public static readonly Token EoF = new EoF();
    
    public static Token Identifier(string value) => new Identifier(value);
    public static Token Integer(int value) => new Integer(value);

    public static readonly Token Assign = new Assign();
    public static readonly Token Plus = new Plus();
    public static readonly Token Dash = new Dash();
    public static readonly Token Bang = new Bang();
    public static readonly Token Asterisk = new Asterisk();
    public static readonly Token ForwardSlash = new ForwardSlash();
    public static readonly Token LessThan = new LessThan();
    public static readonly Token GreaterThan = new GreaterThan();
    public static readonly Token NotEqual = new NotEqual();
    public static readonly Token Equal = new Equal();
    
    public static readonly Token Comma = new Comma();
    public static readonly Token Semicolon = new Semicolon();
    
    public static readonly Token LParen = new LParen();
    public static readonly Token RParen = new RParen();
    public static readonly Token LSquirly = new LSquirly();
    public static readonly Token RSquirly = new RSquirly();
    
    public static readonly Token Function = new Function();   
    public static readonly Token Let = new Let();
    public static readonly Token True = new True();
    public static readonly Token False = new False();
    public static readonly Token If = new If();
    public static readonly Token Else = new Else();
    public static readonly Token Return = new Return();
};

public record Illegal(char Value) : Token;
public record EoF : Token;
public record Identifier(string Value): Token;
public record Integer(int Value) : Token;
public record Assign: Token;
public record Plus: Token;

public record Dash : Token;
public record Bang: Token;
public record Asterisk: Token;
public record ForwardSlash: Token;
public record LessThan: Token;
public record GreaterThan: Token;
public record NotEqual: Token;
public record Equal: Token;
public record Comma: Token;
public record Semicolon: Token;
public record LParen: Token;
public record RParen: Token;
public record LSquirly: Token;
public record RSquirly: Token;
public record Function: Token;
public record Let : Token;
public record True: Token;
public record False: Token;
public record If: Token;
public record Else: Token;
public record Return: Token;
