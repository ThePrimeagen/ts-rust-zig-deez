using Monkey.Lexing;

namespace Monkey.Parsing;

public static class FormatterExtensions
{
    public static string Format(this IEnumerable<IStatement> statements)
        => string.Join("\n", statements.Select(statement => statement.Format()));

    public static string Format(this IEnumerable<IToken> tokens)
        => string.Join("\n", tokens.Select(token => token.ToString()));

    public static string Format(this IStatement statement)
    {
        return statement switch
        {
            LetStatement(var identifier, var expression)
                => $"let {identifier} = {expression.Format()};",
            ReturnStatement(var expression)
                => $"return {expression.Format()};",
            ExpressionStatement(var expression)
                => $"{expression.Format()};",
            BlockStatement(var statements)
                => $"{{\n{statements.Format()}\n}}",

            _ => Statements.OutOfRange<string>(nameof(statement))
        };
    }

    public static string Format(this IExpression expression)
    {
        return expression switch
        {
            ErrorExpression(var message, var token)
                => $"Oops! There was an error during parsing:\n  > Error: {message}\n  > Token: `{token.Format()}`",
            IdentifierExpression(var name)
                => name,
            IntegerExpression(var value)
                => $"{value}",
            PrefixExpression(var operation, var right)
                => $"({operation.Format()}{right.Format()})",
            InfixExpression(var left, var operation, var right)
                => $"({left.Format()} {operation.Format()} {right.Format()})",
            BooleanExpression(var value)
                => $"{value}",
            IfExpression(var condition, var consequence, var alternative)
                => $"if ({condition.Format()}) {consequence.Format()} else {alternative.Format()}",
            FunctionExpression(var parameters, var body)
                => $"fn({string.Join(", ", parameters)}) {{\n{body.Format()}}}",
            CallExpression(var name, var arguments)
                => $"{name.Format()}({string.Join(", ", arguments.Select(argument => argument.Format()))})",

            _ => Expressions.OutOfRange<string>(nameof(expression))
        };
    }

    public static string Format(this IToken token)
    {
        return token switch
        {
            AssignToken => "=",
            AsteriskToken => "*",
            BangToken => "!",
            CommaToken => ",",
            DashToken => "-",
            ElseToken => "else",
            EoFToken => "",
            EqualToken => "==",
            FalseToken => "false",
            ForwardSlashToken => "/",
            FunctionToken => "fn",
            GreaterThanToken => ">",
            IdentifierToken(var value) => value,
            IfToken => "if",
            IllegalToken => "ILLEGAL",
            IntegerToken(var value) => $"{value}",
            LessThanToken => "<",
            LetToken => "let",
            LParenToken => "(",
            LSquirlyToken => "{",
            NotEqualToken => "!=",
            PlusToken => "+",
            ReturnToken => "return",
            RParenToken => ")",
            RSquirlyToken => "}",
            SemicolonToken => ";",
            TrueToken => "true",

            _ => Tokens.OutOfRange<string>(nameof(token))
        };
    }
}