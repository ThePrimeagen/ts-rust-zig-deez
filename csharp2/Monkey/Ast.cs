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
    public required IExpression Value { get; set; }

    public LetStatement(string identifierValue) => Identifier = new(identifierValue);

    public override string ToString()
    {
        var builder = new StringBuilder($"{Token.Literal} {Identifier.Token.Literal} = ");
        builder.Append(Value.ToString());
        builder.Append(';');
        return builder.ToString();
    }
}

class ReturnStatement : IStatement
{
    public Token Token { get; } = Token.Return;
    public required IExpression Value { get; set; }

    public override string ToString()
    {
        var builder = new StringBuilder($"{Token.Literal} ");
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

class ExpressionStatement : IExpression
{
    public Token Token { get; set; }
    public required IExpression Value { get; set; }

    public override string ToString() => Value.ToString();
}

class Ast
{
    readonly List<IStatement> _statements = new();

    public Ast(IEnumerable<IStatement>? statements = null)
    {
        if (statements is not null)
            _statements.AddRange(statements);
    }

    public override string ToString()
    {
        var builder = new StringBuilder();
        foreach (var statement in _statements)
            builder.Append(statement.ToString());

        return builder.ToString();
    }
}
