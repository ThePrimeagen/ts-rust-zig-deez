using monkey;

var user = Environment.UserName;

Console.WriteLine($"Hello {user}! This is the Monkey programming language!");
Console.WriteLine("Feel free to type in commands");

const string PROMPT = ">> ";

while (true)
{ 
    Console.Write(PROMPT);
    var line = Console.ReadLine();

    if(line is null)
    {
        break;
    }

    var lexer = new Lexer(line);

    foreach(var token in lexer.ParseTokens())
    {
        Console.WriteLine(token);
    }
}