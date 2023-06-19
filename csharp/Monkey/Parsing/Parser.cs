using System.Diagnostics;
using Monkey.Lexing;

namespace Monkey.Parsing;

// This parser has almost the same implementation as in the books, but instead of using Maps to find Prefix and Infix Expressions, we are using pattern matchin
public static class Parser
{
    // By default all enums are ints underneath, starting with 0 and increasing in value. We can change the type, but it still needs to remain a whole number
    private enum Precedence: byte
    {
        Lowest, Equals, Comparison, Sum, Product, Prefix, Call
    }

    public static IStatement[] Parse(IToken[] input)
        => ParseStatements(new TokenReader(input)).Statements;

    private static (IStatement[] Statements, TokenReader Remaining) ParseStatements(TokenReader input)
    {
        var result = new List<IStatement>();

        while (
            input is [not EoFToken and not RSquirlyToken, ..]
            && ParseStatement(input) is var (statement, remaining)
        )
        {
            result.Add(statement);
            input = remaining;
        }

        return (result.ToArray(), input);
    }

    private static (IStatement Statement, TokenReader Remaining) ParseStatement(TokenReader input)
    {
        return input switch
        {
            // Thanks to returning the remaining tokens, we can easily chain complex pattern matching together
            [LetToken, IdentifierToken(var name), AssignToken, .. var expressionTokens]
                when ParseExpression(expressionTokens) is (var expression, [SemicolonToken, .. var remaining])
                => (Statements.Let(name, expression), remaining),

            [ReturnToken, .. var expressionTokens]
                when ParseExpression(expressionTokens) is (var expression, [SemicolonToken, .. var remaining])
                => (Statements.Return(expression), remaining),

            _ when ParseExpression(input) is var (expression, remaining)
                => (Statements.Expression(expression), remaining.SkipWhile(token => token is SemicolonToken)),

            _ => throw new UnreachableException()
        };
    }

    private static (IExpression Expression, TokenReader Remaining) ParseExpression(TokenReader input, Precedence precedence = Precedence.Lowest)
    {
        var (left, remaining) = ParsePrefixExpression(input);

        while (
            remaining is [var infixOperator, ..]
            && GetPrecedence(infixOperator) > precedence
            && ParseInfixExpression(remaining, left) is var (right, next)
        )
        {
            remaining = next;
            left = right;
        }

        return (left, remaining);
    }

    private static (IExpression Expression, TokenReader Remaining) ParsePrefixExpression(TokenReader input)
    {
        return input switch
        {
            [IdentifierToken(var identifier), .. var remaining]
                => (Expressions.Identifier(identifier), remaining),

            [IntegerToken(var integer), .. var remaining]
                => (Expressions.Integer(integer), remaining),

            [(BangToken or DashToken) and var token, .. var expressionTokens]
                when ParseExpression(expressionTokens, Precedence.Prefix) is var (expression, remaining)
                => (Expressions.Prefix(token, expression), remaining),

            [TrueToken, .. var remaining]
                => (Expressions.Boolean(true), remaining),

            [FalseToken, .. var remaining]
                => (Expressions.Boolean(false), remaining),

            [LParenToken, .. var expressionTokens]
                when ParseExpression(expressionTokens) is (var expression, [RParenToken, .. var remaining])
                => (expression, remaining),

            // in case of optional patterns, we can either provide all the combinations needed, or nest the patterns to save performance
            [IfToken, LParenToken, .. var condition]
                when ParseExpression(condition) is (var conditionExpression, [RParenToken, LSquirlyToken, .. var consequence])
                && ParseStatements(consequence) is (var consequenceStatements, [RSquirlyToken, .. var optionalElse])
                => optionalElse switch
                {
                    [ElseToken, LSquirlyToken, .. var alternative]
                        when ParseStatements(alternative) is (var alternativeStatements, [RSquirlyToken, .. var remaining])
                        => (Expressions.If(conditionExpression, Statements.Block(consequenceStatements), Statements.Block(alternativeStatements)), remaining),

                    var remaining
                        => (Expressions.If(conditionExpression, Statements.Block(consequenceStatements), Statements.Block(Array.Empty<IStatement>())), remaining)
                },

            [FunctionToken, LParenToken, .. var parameterTokens]
                when ParseFunctionParameters(parameterTokens) is (var parameters, [LSquirlyToken, .. var bodyTokens])
                && ParseStatements(bodyTokens) is (var bodyStatements, [RSquirlyToken, .. var remaining])
                => (Expressions.Function(parameters, Statements.Block(bodyStatements)), remaining),

            [var invalid, .. var remaining]
                => (Expressions.Error("Unexpected prefix token", invalid), remaining),

            [] => (Expressions.Error("Unexpected end of input", Tokens.EoF), input)
        };
    }

    private static (IExpression Expression, TokenReader Remaining) ParseInfixExpression(TokenReader input, IExpression left)
    {
        return input switch
        {
            [LParenToken, .. var arguments]
                when ParseCallArguments(arguments) is var (expressions, remaining)
                => (Expressions.Call(left, expressions), remaining),

            [var token, .. var right]
                when IsBinaryOperationToken(token)
                && ParseExpression(right, GetPrecedence(token)) is var (expression, remaining)
                => (Expressions.Infix(left, token, expression), remaining),

            [var invalid, .. var remaining]
                => (Expressions.Error("Unexpected operator found", invalid), remaining),

            [] => (Expressions.Error("Unexpected end of input", Tokens.EoF), input)
        };
    }

    private static (string[] Identifiers, TokenReader Remaining) ParseFunctionParameters(TokenReader input)
    {
        var result = new List<string>();

        while (input is [IdentifierToken(var identifier), (CommaToken or RParenToken) and var separator, .. var remaining])
        {
            input = remaining;
            result.Add(identifier);

            if (separator is RParenToken)
            {
                break;
            }
        }

        return (result.ToArray(), input);
    }

    private static (IExpression[] Arguments, TokenReader Remaining) ParseCallArguments(TokenReader input)
    {
        var result = new List<IExpression>();

        while (ParseExpression(input) is (var expression, [(CommaToken or RParenToken) and var separator, .. var remaining]))
        {
            input = remaining;
            result.Add(expression);

            if (separator is RParenToken)
            {
                break;
            }
        }

        return (result.ToArray(), input);
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