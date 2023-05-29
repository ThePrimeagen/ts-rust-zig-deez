namespace lexer.tests;

public class Tests
{
    [SetUp]
    public void Setup()
    {
    }

    [Test]
    public void Test1()
    {
        const string testInput = "=+(){},;";

        var result = new Lexer(testInput).ParseTokens();

        var expectedResult = new List<TokenInfo>
        {
            new TokenInfo(Token.Equal),
            new TokenInfo(Token.Plus),
            new TokenInfo(Token.LParen),
            new TokenInfo(Token.RParen),
            new TokenInfo(Token.LSquirly),
            new TokenInfo(Token.RSquirly),
            new TokenInfo(Token.Comma),
            new TokenInfo(Token.Semicolon),
            new TokenInfo(Token.Eof)
        };

        Assert.That(result, Is.EquivalentTo(expectedResult));
    }
    
    [Test]
    public void Test2()
    {
        const string testInput = """
                                let five = 5;
                                let ten = 10;
                                let add = fn(x, y) {
                                    x + y;
                                };
                                let result = add(five, ten);
                                """;

        var tokens = new Lexer(testInput).ParseTokens();

        var expectedResult = new List<TokenInfo>
        {
            new TokenInfo(Token.Let),        // let
            new TokenInfo(Token.Ident, "five"), // five
            new TokenInfo(Token.Equal),      // =
            new TokenInfo(Token.Integer, 5),    // 5
            new TokenInfo(Token.Semicolon),  // ;
            new TokenInfo(Token.Let),        // let
            new TokenInfo(Token.Ident, "ten"), // ten
            new TokenInfo(Token.Equal),      // =
            new TokenInfo(Token.Integer, 10),    // 10
            new TokenInfo(Token.Semicolon),  // ;
            new TokenInfo(Token.Let),        // let
            new TokenInfo(Token.Ident, "add"), // add
            new TokenInfo(Token.Equal),      // =
            new TokenInfo(Token.Function),   // fn
            new TokenInfo(Token.LParen),     // (
            new TokenInfo(Token.Ident, "x"), // x
            new TokenInfo(Token.Comma),      // ,
            new TokenInfo(Token.Ident, "y"), // y
            new TokenInfo(Token.RParen),     // )
            new TokenInfo(Token.LSquirly),   // {
            new TokenInfo(Token.Ident, "x"), // x
            new TokenInfo(Token.Plus),       // +
            new TokenInfo(Token.Ident, "y"), // y
            new TokenInfo(Token.Semicolon),  // ;
            new TokenInfo(Token.RSquirly),   // }
            new TokenInfo(Token.Semicolon),  // ;
            new TokenInfo(Token.Let),        // let
            new TokenInfo(Token.Ident, "result"), // result
            new TokenInfo(Token.Equal),      // =
            new TokenInfo(Token.Ident, "add"), // add
            new TokenInfo(Token.LParen),     // (
            new TokenInfo(Token.Ident, "five"), // five
            new TokenInfo(Token.Comma),      // ,
            new TokenInfo(Token.Ident, "ten"), // ten
            new TokenInfo(Token.RParen),     // )
            new TokenInfo(Token.Semicolon),  // ;
            new TokenInfo(Token.Eof)
        };

        
        Assert.That(tokens, Is.EquivalentTo(expectedResult));
    }
}