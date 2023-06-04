using Monkey.Lexing;
using Monkey.Parsing;

Console.WriteLine("Welcome to Monkey REPL:");

while (Console.ReadLine() is { Length: > 0 } input)
{
    foreach (var token in Parser.Parse(Lexer.ParseTokens(input)))
    {
        Console.WriteLine(token.Format());
    }
}