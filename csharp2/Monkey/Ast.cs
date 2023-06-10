using System.Text;

namespace Monkey;

interface INode
{
    Token Token { get; }
}

interface IStatement : INode { }

interface IExpression : INode { }

class LetStatement : IStatement
{
    public Token Token { get; } = Token.Let;
    public Identifier Identifier { get; }
    public IExpression? Value { get; set; }

    public LetStatement(string identifierValue) => Identifier = new(identifierValue);

    public override string ToString()
    {
        var builder = new StringBuilder($"{Token.Literal} {Identifier.Token.Literal} = ");
        if (Value is not null)
            builder.Append(Value.ToString());

        builder.Append(';');
        return builder.ToString();
    }
}

class ReturnStatement : IStatement
{
    public Token Token { get; } = Token.Return;
    public IExpression? Value { get; set; }

    public override string ToString()
    {
        var builder = new StringBuilder($"{Token.Literal} ");
        if (Value is not null)
            builder.Append(Value.ToString());

        builder.Append(';');
        return builder.ToString();
    }
}

class Identifier : IExpression
{
    public Token Token { get; }

    public Identifier(string literal)
        => Token = new Token(TokenType.Ident, literal);

    public override string ToString() => Token.Literal;
}

class ExpressionStatement : IStatement
{
    public Token Token { get; set; }
    public IExpression? Value { get; set; }

    public override string ToString() => Value?.ToString() ?? "";
}

class Ast
{
    public List<IStatement> Statements { get; private set; } = new();

    public Ast(IEnumerable<IStatement>? statements = null)
    {
        if (statements is not null)
            Statements.AddRange(statements);
    }

    public override string ToString()
    {
        var builder = new StringBuilder();
        foreach (var statement in Statements)
            builder.Append(statement.ToString());

        return builder.ToString();
    }
}
