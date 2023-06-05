using Monkey;

while (true)
{
    Console.Write(">> ");
    var line = Console.ReadLine();
    if (line is null)
        break;

    var lexer = new Lexer(line);
    var token = lexer.NextToken();
    while (token.Type != TokenType.Eof)
    {
        Console.WriteLine($" {token}");
        token = lexer.NextToken();
    }
}
