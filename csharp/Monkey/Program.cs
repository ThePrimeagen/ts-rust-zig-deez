using Monkey;

Console.WriteLine("Welcome to Monkey REPL:");

while (Console.ReadLine() is { Length: > 0 } input)
{
    foreach (var token in Lexer.ParseTokens(input))
    {
        Console.WriteLine(token);
    }
}