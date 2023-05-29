using lexer;

const string testInput = """
let hello = "world";
let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
""";

var lexer = new Lexer(testInput);

TokenInfo tok;
while ((tok = lexer.NextToken()).Type != Token.Eof)
{
    Console.WriteLine(tok.ToString());
}