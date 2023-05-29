using lexer;

const string testInput = """
let five = 5;
let ten = 10;
let add = fn(x, y) {
    x + y;
};
let result = add(five, ten);
""";

var tokens = new Lexer(testInput).ParseTokens();

foreach(var token in tokens)
{ 
    Console.WriteLine(token);
}