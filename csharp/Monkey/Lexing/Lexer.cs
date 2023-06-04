namespace Monkey.Lexing;

public static class Lexer
{
    public static IToken[] ParseTokens(string input)
    {
        var current = 0;
        var result = new List<IToken>(64);

        while (Next(input.AsSpan(current)) is (token: var token and not EoFToken, length: var length))
        {
            current += length;
            result.Add(token);
        }

        result.Add(Tokens.EoF);
        return result.ToArray();
    }

    private static (IToken token, int length) Next(ReadOnlySpan<char> input)
    {
        var whiteSpaceLength = ReadWhile(input, char.IsWhiteSpace);
        var (token, length) = MatchToken(input[whiteSpaceLength..]);
        return (token, length + whiteSpaceLength);
    }

    private static (IToken token, int length) MatchToken(ReadOnlySpan<char> input)
    {
        return input switch
        {
            ['!', '=', ..] => (Tokens.NotEqual, 2),
            ['=', '=', ..] => (Tokens.Equal, 2),
            ['=', ..] => (Tokens.Assign, 1),
            ['+', ..] => (Tokens.Plus, 1),
            ['-', ..] => (Tokens.Dash, 1),
            ['!', ..] => (Tokens.Bang, 1),
            ['*', ..] => (Tokens.Asterisk, 1),
            ['/', ..] => (Tokens.ForwardSlash, 1),
            ['<', ..] => (Tokens.LessThan, 1),
            ['>', ..] => (Tokens.GreaterThan, 1),
            [',', ..] => (Tokens.Comma, 1),
            [';', ..] => (Tokens.Semicolon, 1),
            ['(', ..] => (Tokens.LParen, 1),
            [')', ..] => (Tokens.RParen, 1),
            ['{', ..] => (Tokens.LSquirly, 1),
            ['}', ..] => (Tokens.RSquirly, 1),

            [var c, ..]
                when char.IsLetter(c)
                && ReadWhile(input, char.IsLetterOrDigit) is >0 and var length
                => (MatchKeywordOrIdentifier(input[..length]), length),

            _ when ReadWhile(input, char.IsDigit) is >0 and var length
                => (Tokens.Integer(int.Parse(input[..length])), length),

            [var c, ..] => (Tokens.Illegal(c), 1),

            [] => (Tokens.EoF, 0)
        };
    }

    private static IToken MatchKeywordOrIdentifier(ReadOnlySpan<char> input)
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

    private static int ReadWhile(ReadOnlySpan<char> input, Func<char, bool> predicate)
    {
        var i = 0;

        while (i < input.Length && predicate(input[i]))
        {
            i++;
        }

        return i;
    }
}