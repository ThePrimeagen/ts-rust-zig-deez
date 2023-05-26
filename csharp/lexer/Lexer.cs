namespace lexer;

public static class Lexer
{
    public static IEnumerable<(Tokens, object? value)> ParseTokens(string input)
    {
        var position = 0;
        while (position < input.Length)
        {
            var c = input[position];
            
            if(char.IsWhiteSpace(c))
            {
                position++;
                continue;
            }
            
            if (char.IsLetter(c))
            {
                var start = position;
                while (position < input.Length && (char.IsLetterOrDigit(input[position]) || input[position] == '_'))
                {
                    position++;
                }

                var value = input.Substring(start, position - start);
                var token = value switch
                {
                    "let" => Tokens.Let,
                    "fn" => Tokens.Function,
                    _ => Tokens.Identifier
                };
                yield return (token, value);
                continue;
            }
            
            if (char.IsDigit(c))
            {
                var start = position;

                while (position < input.Length && char.IsDigit(input[position]))
                {
                    position++;
                }

                var value = input.Substring(start, position - start);
                yield return (Tokens.Integer, int.Parse(value));
                continue;
            }

            if (c == '\"')
            {
                var start = position + 1; // Skip the opening quote
                position++;

                while (position < input.Length && input[position] != '\"')
                {
                    position++;
                }

                if (position >= input.Length)
                {
                    throw new Exception("Unterminated string literal.");
                }

                var value = input.Substring(start, position - start);
                position++; // Skip the closing quote

                yield return (Tokens.String, value);
                position++;
                continue;
            }
            
            //Single char tokens
            yield return c switch
            {
                '=' => (Tokens.Equal, '='),
                '+' => (Tokens.Plus, '+'),
                ',' => (Tokens.Comma, ','),
                ';' => (Tokens.Semicolon, ';'),
                '(' => (Tokens.LParen, '('),
                ')' => (Tokens.RParen, ')'),
                '{' => (Tokens.LSquirly, '{'),
                '}' => (Tokens.RSquirly, '}'),
                _ => (Tokens.Illegal, c)
            };
            
            position++;
        }
            
    
        yield return (Tokens.Eof, null);
    }
}