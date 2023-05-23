using lexer;

const string testInput = "=+(){},;";

var tokens = Lexer.ParseTokens(testInput);

foreach (var token in tokens)
{
    Console.WriteLine(token);
}

