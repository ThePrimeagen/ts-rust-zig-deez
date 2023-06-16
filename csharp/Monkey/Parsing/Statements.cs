namespace Monkey.Parsing;

public static class Statements
{
    public static IStatement Let(string identifier, IExpression expression) => new LetStatement(identifier, expression);
    public static IStatement Return(IExpression expression) => new ReturnStatement(expression);
    public static IStatement Expression(IExpression expression) => new ExpressionStatement(expression);
    public static IStatement Block(IStatement[] statements) => new BlockStatement(statements);

    public static T OutOfRange<T>(string parameter) => throw new ArgumentOutOfRangeException(parameter);
}

public interface IStatement {}
public readonly record struct LetStatement(string Identifier, IExpression Value) : IStatement;
public readonly record struct ReturnStatement(IExpression Value) : IStatement;
public readonly record struct ExpressionStatement(IExpression Value) : IStatement;
public readonly record struct BlockStatement(IStatement[] Statements) : IStatement;
