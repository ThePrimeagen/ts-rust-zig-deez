using System.Text;

namespace Monkey;

interface INode {
    Token Token { get; set; }
}

interface IStatement : INode { }

interface IExpression : INode { }

class LetStatement : IStatement {
    public required Token Token { get; set; }
    public required Identifier Name { get; set; }
    public IExpression? Value { get; set; }

    public override string ToString() {
        var builder = new StringBuilder($"{Token.Literal} {Name.Token.Literal} = ");
        if (Value is not null)
            builder.Append(Value.ToString());

        builder.Append(';');
        return builder.ToString();
    }
}

class ReturnStatement : IStatement {
    public required Token Token { get; set; }
    public IExpression? Expression { get; set; }

    public override string ToString() {
        var builder = new StringBuilder($"{Token.Literal} ");
        if (Expression is not null)
            builder.Append(Expression.ToString());

        builder.Append(';');
        return builder.ToString();
    }
}

class ExpressionStatement : IStatement {
    public required Token Token { get; set; }
    public IExpression? Expression { get; set; }

    public override string ToString() => Expression?.ToString() ?? "";
}

class Identifier : IExpression {
    public required Token Token { get; set; }
    public required string Value { get; set; }

    public override string ToString() => Value;
}

class IntegerLiteral : IExpression {
    public required Token Token { get; set; }
    public required long Value { get; set; }

    public override string ToString() => Token.Literal;
}

class PrefixExpression : IExpression {
    public required Token Token { get; set; }
    public required string Operator { get; set; }
    public IExpression? Right { get; set; }

    public override string ToString() {
        var builder = new StringBuilder($"({Operator}");
        if (Right is not null)
            builder.Append(Right.ToString());

        builder.Append(')');
        return builder.ToString();
    }
}

class InfixExpression : IExpression {
    public required Token Token { get; set; }
    public required string Operator { get; set; }
    public IExpression? Left { get; set; }
    public IExpression? Right { get; set; }

    public override string ToString() {
        var builder = new StringBuilder("(");
        if (Left is not null)
            builder.Append(Left.ToString());

        builder.Append($" {Operator} ");
        if (Right is not null)
            builder.Append(Right.ToString());

        builder.Append(')');
        return builder.ToString();
    }
}

class BooleanExpression : IExpression {
    public required Token Token { get; set; }
    public required bool Value { get; set; }

    public override string ToString() => Token.Literal;
}

class Ast {
    public List<IStatement> Statements { get; private set; } = new();

    public Ast(IEnumerable<IStatement>? statements = null) {
        if (statements is not null)
            Statements.AddRange(statements);
    }

    public override string ToString() {
        var builder = new StringBuilder();

        foreach (var statement in Statements)
            builder.Append(statement.ToString());

        return builder.ToString();
    }
}
