using System.Diagnostics;
using System.Runtime.InteropServices;
using Monkey.Lexing;

namespace Monkey.Parsing;

public static class Parser
{
    private enum Precedence: byte
    {
        Lowest, Equals, Comparison, Sum, Product, Prefix, Call
    }

    public static IStatement[] Parse(IEnumerable<IToken> input)
        => ParseStatements(CollectionsMarshal.AsSpan(input.ToList())).Statements;

    private static (IStatement Statement, int Length) ParseStatement(ReadOnlySpan<IToken> input)
    {
        return input switch
        {
            [LetToken, IdentifierToken(var name), AssignToken, .. var expressionTokens]
                when ParseExpression(expressionTokens) is var (expression, length)
                && expressionTokens[length] is SemicolonToken
                => (Statements.Let(name, expression), length + 4),

            [ReturnToken, .. var expressionTokens]
                when ParseExpression(expressionTokens) is var (expression, length)
                && expressionTokens[length] is SemicolonToken
                => (Statements.Return(expression), length + 2),

            _ when ParseExpression(input) is var (expression, length)
                => (Statements.Expression(expression), length + (input[length] is SemicolonToken ? 1 : 0)),

            _ => throw new UnreachableException()
        };
    }

    private static (IExpression Expression, int Length) ParseExpression(ReadOnlySpan<IToken> input, Precedence precedence = Precedence.Lowest)
    {
        var (left, current) = ParsePrefixExpression(input);

        while (
            input[current..] is [var infix, ..]
            && GetPrecedence(infix) > precedence
            && ParseInfixExpression(input[current..], left) is var (right, length)
        )
        {
            current += length;
            left = right;
        }

        return (left, current);
    }

    private static (IExpression Expression, int Length) ParsePrefixExpression(ReadOnlySpan<IToken> input)
    {
        return input switch
        {
            [IdentifierToken(var identifier), ..]
                => (Expressions.Identifier(identifier), 1),

            [IntegerToken(var integer), ..]
                => (Expressions.Integer(integer), 1),

            [(BangToken or DashToken) and var token, .. var expressionTokens]
                when ParseExpression(expressionTokens, Precedence.Prefix) is var (expression, length)
                => (Expressions.Prefix(token, expression), length + 1),

            [TrueToken, ..]
                => (Expressions.Boolean(true), 1),

            [FalseToken, ..]
                => (Expressions.Boolean(false), 1),

            [LParenToken, .. var expressionTokens]
                when ParseExpression(expressionTokens) is var (expression, length)
                && expressionTokens[length] is RParenToken
                => (expression, length + 2),

            [IfToken, LParenToken, .. var condition]
                when ParseExpression(condition) is var (conditionExpression, conditionLength)
                && condition[conditionLength..] is [RParenToken, LSquirlyToken, .. var consequence]
                && ParseStatements(consequence) is var (consequenceStatements, consequenceLength)
                && consequence[consequenceLength..] is [RSquirlyToken, .. var optionalElse]
                && conditionLength + consequenceLength + 5 is var length
                => optionalElse switch
                {
                    [ElseToken, LSquirlyToken, .. var alternative]
                        when ParseStatements(alternative) is var (alternativeStatements, alternativeLength)
                        && alternative[alternativeLength] is RSquirlyToken
                        => (Expressions.If(conditionExpression, Statements.Block(consequenceStatements), Statements.Block(alternativeStatements)), length + alternativeLength + 3),

                    _ => (Expressions.If(conditionExpression, Statements.Block(consequenceStatements), Statements.Block(Array.Empty<IStatement>())), length),
                },

            [FunctionToken, LParenToken, .. var parameterTokens]
                when ParseFunctionParameters(parameterTokens) is var (parameters, parametersLength)
                && parameterTokens[parametersLength..] is [LSquirlyToken, .. var bodyTokens]
                && ParseStatements(bodyTokens) is var (bodyStatements, bodyLength)
                && bodyTokens[bodyLength] is RSquirlyToken
                => (Expressions.Function(parameters, Statements.Block(bodyStatements)), parametersLength + bodyLength + 4),

            [var invalid, ..]
                => (Expressions.Error("Unexpected prefix token", invalid), 1),

            [] => (Expressions.Error("Unexpected end of input", Tokens.EoF), 0)
        };
    }

    private static (IExpression Expression, int Length) ParseInfixExpression(ReadOnlySpan<IToken> input, IExpression left)
    {
        return input switch
        {
            [LParenToken, .. var arguments]
                when ParseCallArguments(arguments) is var (expressions, length)
                => (Expressions.Call(left, expressions), length + 1),

            [var token, .. var rest]
                when IsBinaryOperationToken(token)
                && ParseExpression(rest, GetPrecedence(token)) is var (expression, length)
                => (Expressions.Infix(left, token, expression), length + 1),

            [var invalid, ..]
                => (Expressions.Error("Unexpected operator found", invalid), 1),

            [] => (Expressions.Error("Unexpected end of input", Tokens.EoF), 0)
        };
    }

    private static (IStatement[] Statements, int Length) ParseStatements(ReadOnlySpan<IToken> input)
    {
        var result = new List<IStatement>();
        var current = 0;

        while (
            input[current..] is [not EoFToken and not RSquirlyToken, ..] span
            && ParseStatement(span) is var (statement, length)
        )
        {
            current += length;
            result.Add(statement);
        }

        return (result.ToArray(), current);
    }

    private static (string[] Identifiers, int Length) ParseFunctionParameters(ReadOnlySpan<IToken> input)
    {
        var current = 0;
        var result = new List<string>();

        while (input[current..] is [IdentifierToken(var identifier), (CommaToken or RParenToken) and var separator, ..])
        {
            result.Add(identifier);
            current += 2;

            if (separator is RParenToken)
            {
                break;
            }
        }

        return (result.ToArray(), current);
    }

    private static (IExpression[] Arguments, int Length) ParseCallArguments(ReadOnlySpan<IToken> input)
    {
        var current = 0;
        var result = new List<IExpression>();

        while (
            ParseExpression(input[current..]) is var (expression, length)
            && input[current + length] is (CommaToken or RParenToken) and var separator
        )
        {
            result.Add(expression);
            current += length + 1;

            if (separator is RParenToken)
            {
                break;
            }
        }

        return (result.ToArray(), current);
    }

    private static bool IsBinaryOperationToken(IToken token)
    {
        return token is
            EqualToken
            or NotEqualToken
            or GreaterThanToken
            or LessThanToken
            or PlusToken
            or DashToken
            or ForwardSlashToken
            or AsteriskToken;
    }

    private static Precedence GetPrecedence(IToken token)
    {
        return token switch
        {
            EqualToken or NotEqualToken
                => Precedence.Equals,

            GreaterThanToken or LessThanToken
                => Precedence.Comparison,

            PlusToken or DashToken
                => Precedence.Sum,

            ForwardSlashToken or AsteriskToken
                => Precedence.Product,

            LParenToken
                => Precedence.Call,

            _ => Precedence.Lowest
        };
    }
}