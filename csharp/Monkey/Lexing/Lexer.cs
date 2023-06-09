using System.Diagnostics.Contracts;

namespace Monkey.Lexing;

// We are trying for a "stateless" approach, where we don't store the position in the instance after every iteration.
// We don't need to have an instance, so this class can be made static
public static class Lexer
{
    // By annotating the method with `[Pure]`, we are telling the compiler that this method has no side effects.
    [Pure]
    public static IToken[] ParseTokens(string input)
    {
        // There are multiple ways how to achieve similar, "stateless", behaviour. We can pass around positions, or remaining strings directly.
        // By using a custom type, we can show how to use list pattern matching on custom types,
        // and also we can help with a bit of performance due to the nature of the implementation
        var current = new StringReader(input);

        // By default, lists are not created with a capacity, and on first add, an array of size 4 is created underneath.
        // Then everytime the capacity is reached, the array is doubled in size.
        // By specifying a reasonable amount, we avoid the unnecessary allocations and improve performance.
        var result = new List<IToken>(64);

        // the `Next` method returns a tuple that we can destructure and pattern match at the same time
        while (Next(current) is (var token and not EoFToken, var remaining))
        {
            result.Add(token);
            current = remaining;
        }

        result.Add(Tokens.EoF);
        return result.ToArray();
    }

    [Pure]
    // We are returning a `ValueTuple` that is a tuple with named fields, which is a common way of returning multiple parameters.
    // It can also be used in destructuring and pattern matching
    private static (IToken Token, StringReader Remaining) Next(StringReader input)
        => MatchToken(input.SkipWhile(char.IsWhiteSpace));


    [Pure]
    private static (IToken Token, StringReader Remaining) MatchToken(StringReader input)
    {
        // Our first pattern matching. We are using list-based patterns (because of the `[]` syntax) with positional and range patterns.
        return input switch
        {
            ['!', '=', .. var remaining] => (Tokens.NotEqual, remaining),
            ['=', '=', .. var remaining] => (Tokens.Equal, remaining),
            ['=', .. var remaining] => (Tokens.Assign, remaining),
            ['+', .. var remaining] => (Tokens.Plus, remaining),
            ['-', .. var remaining] => (Tokens.Dash, remaining),
            ['!', .. var remaining] => (Tokens.Bang, remaining),
            ['*', .. var remaining] => (Tokens.Asterisk, remaining),
            ['/', .. var remaining] => (Tokens.ForwardSlash, remaining),
            ['<', .. var remaining] => (Tokens.LessThan, remaining),
            ['>', .. var remaining] => (Tokens.GreaterThan, remaining),
            [',', .. var remaining] => (Tokens.Comma, remaining),
            [';', .. var remaining] => (Tokens.Semicolon, remaining),
            ['(', .. var remaining] => (Tokens.LParen, remaining),
            [')', .. var remaining] => (Tokens.RParen, remaining),
            ['{', .. var remaining] => (Tokens.LSquirly, remaining),
            ['}', .. var remaining] => (Tokens.RSquirly, remaining),

            [var c, ..]
                // `when` is used to add additional runtime checks to the pattern.
                when IsLetterOrUnderscore(c)
                && input.ReadWhile(IsLetterDigitOrUnderscore) is var (identifier, remaining)
                => (MatchKeywordOrIdentifier(identifier), remaining),

            [var c, ..]
                when char.IsDigit(c)
                && input.ReadWhile(char.IsDigit) is var (integer, remaining)
                => (Tokens.Integer(int.Parse(integer)), remaining),

            [var c, .. var remaining]
                => (Tokens.Illegal(c), remaining),

            [] => (Tokens.EoF, input)
        };
    }

    [Pure]
    private static IToken MatchKeywordOrIdentifier(string input)
    {
        return input switch
        {
            "fn" => Tokens.Function,
            "let" => Tokens.Let,
            "true" => Tokens.True,
            "false" => Tokens.False,
            "if" => Tokens.If,
            "else" => Tokens.Else,
            "return" => Tokens.Return,
            _ => Tokens.Identifier(input.ToString())
        };
    }

    [Pure]
    private static bool IsLetterOrUnderscore(char c) => char.IsLetter(c) || c is '_';

    [Pure]
    private static bool IsLetterDigitOrUnderscore(char c) => char.IsLetterOrDigit(c) || c is '_';
}