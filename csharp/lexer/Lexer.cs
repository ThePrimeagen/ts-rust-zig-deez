namespace lexer;

public static class Lexer
{
    public static IEnumerable<Tokens> ParseTokens(string input)
    {
        foreach (var c in input)
        {
            yield return c switch
            {
                '=' => Tokens.Equal,
                '+' => Tokens.Plus,
                ',' => Tokens.Comma,
                ';' => Tokens.Semicolon,
                '(' => Tokens.LParen,
                ')' => Tokens.RParen,
                '{' => Tokens.LSquirly,
                '}' => Tokens.RSquirly,
                _ => Tokens.Illegal
            };
        }
    
        yield return Tokens.Eof;
    }
}