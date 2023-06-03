namespace Monkey;

public static class Lexer
{
    public static IEnumerable<Token> ParseTokens(string input)
    {
        var current = 0;
        
        while (Next(input.AsSpan(current)) is (token: var token and not EoF, length: var length))
        {
            current += length;
            yield return token;
        }

        yield return Token.EoF;
    }
    
    private static (Token token, int length) Next(ReadOnlySpan<char> input)
    {
        var whiteSpaceLength = ReadWhile(input, char.IsWhiteSpace);
        var (token, length) = MatchToken(input[whiteSpaceLength..]);
        return (token, length + whiteSpaceLength);
    }
    
    private static (Token token, int length) MatchToken(ReadOnlySpan<char> input)
    {
        return input switch
        {                
            ['!', '=', ..] => (Token.NotEqual, 2),
            ['=', '=', ..] => (Token.Equal, 2),
            ['=', ..] => (Token.Assign, 1),
            ['+', ..] => (Token.Plus, 1),
            ['-', ..] => (Token.Dash, 1),
            ['!', ..] => (Token.Bang, 1),
            ['*', ..] => (Token.Asterisk, 1),
            ['/', ..] => (Token.ForwardSlash, 1),
            ['<', ..] => (Token.LessThan, 1),
            ['>', ..] => (Token.GreaterThan, 1),
            [',', ..] => (Token.Comma, 1),
            [';', ..] => (Token.Semicolon, 1),
            ['(', ..] => (Token.LParen, 1),
            [')', ..] => (Token.RParen, 1),
            ['{', ..] => (Token.LSquirly, 1),
            ['}', ..] => (Token.RSquirly, 1),
            
            [var c, ..]
                when char.IsLetter(c)
                && ReadWhile(input, char.IsLetterOrDigit) is >0 and var length 
                => (MatchKeywordOrIdentifier(input[..length]), length),
            
            _ when ReadWhile(input, char.IsDigit) is >0 and var length 
                => (Token.Integer(int.Parse(input[..length])), length),
            
            [var c, ..] => (Token.Illegal(c), 1),
            
            [] => (Token.EoF, 0)
        };
    }

    private static Token MatchKeywordOrIdentifier(ReadOnlySpan<char> input)
    {
        return input switch
        {
            "fn" => Token.Function,
            "let" => Token.Let,
            "true" => Token.True,
            "false" => Token.False,
            "if" => Token.If,
            "else" => Token.Else,
            "return" => Token.Return,
            _ => Token.Identifier(input.ToString())
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