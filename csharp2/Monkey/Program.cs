using Monkey;

const string testInput = """
let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
""";

var lexer = new Lexer(testInput);
var token = lexer.NextToken();
while (token.Type != TokenType.Eof)
{
    Console.WriteLine(token);
    token = lexer.NextToken();
}
Console.WriteLine(token);
