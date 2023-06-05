using Monkey.Lexing;

namespace Monkey.Parsing;

public static class Expressions
{
    public static IExpression Error(string message, IToken token) => new ErrorExpression(message, token);
    public static IExpression Identifier(string identifier) => new IdentifierExpression(identifier);
    public static IExpression Integer(int value) => new IntegerExpression(value);
    public static IExpression Prefix(IToken prefix, IExpression right) => new PrefixExpression(prefix, right);
    public static IExpression Infix(IExpression left, IToken infix, IExpression right) => new InfixExpression(left, infix, right);
    public static IExpression Boolean(bool value) => new BooleanExpression(value);
    public static IExpression If(IExpression condition, IStatement consequence, IStatement alternative)
        => new IfExpression(condition, consequence, alternative);
    public static IExpression Function(string[] parameters, IStatement body) => new FunctionExpression(parameters, body);
    public static IExpression Call(IExpression function, IExpression[] arguments) => new CallExpression(function, arguments);

    public static T OutOfRange<T>(string parameter) => throw new ArgumentOutOfRangeException(parameter);
}

public interface IExpression {}
public readonly record struct ErrorExpression(string Message, IToken Token) : IExpression;
public readonly record struct IdentifierExpression(string Identifier): IExpression;
public readonly record struct IntegerExpression(int Integer): IExpression;
public readonly record struct PrefixExpression(IToken Operation, IExpression Right) : IExpression;
public readonly record struct InfixExpression(IExpression Left, IToken Operation, IExpression Right) : IExpression;
public readonly record struct BooleanExpression(bool Boolean) : IExpression;
public readonly record struct IfExpression(IExpression Condition, IStatement Consequence, IStatement Alternative) : IExpression;
public readonly record struct FunctionExpression(string[] Parameters, IStatement Body) : IExpression;
public readonly record struct CallExpression(IExpression Expression, IExpression[] Arguments) : IExpression;