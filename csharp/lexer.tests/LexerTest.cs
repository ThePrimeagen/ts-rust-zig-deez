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
            new TokenInfo(Token.Assign),
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
                                !-/*5;
                                5 < 10 > 5;
                                
                                if (5 < 10) {
                                return true;
                                } else {
                                return false;
                                }
                                
                                10 == 10;
                                10 != 9;
                                """;

        var tokens = new Lexer(testInput).ParseTokens();

        var expectedResult = new List<TokenInfo>
        {
            new TokenInfo(Token.Let),        // let
            new TokenInfo(Token.Ident, "five"), // five
            new TokenInfo(Token.Assign),      // =
            new TokenInfo(Token.Integer, 5),    // 5
            new TokenInfo(Token.Semicolon),  // ;
            new TokenInfo(Token.Let),        // let
            new TokenInfo(Token.Ident, "ten"), // ten
            new TokenInfo(Token.Assign),      // =
            new TokenInfo(Token.Integer, 10),    // 10
            new TokenInfo(Token.Semicolon),  // ;
            new TokenInfo(Token.Let),        // let
            new TokenInfo(Token.Ident, "add"), // add
            new TokenInfo(Token.Assign),      // =
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
            new TokenInfo(Token.Assign),      // =
            new TokenInfo(Token.Ident, "add"), // add
            new TokenInfo(Token.LParen),     // (
            new TokenInfo(Token.Ident, "five"), // five
            new TokenInfo(Token.Comma),      // ,
            new TokenInfo(Token.Ident, "ten"), // ten
            new TokenInfo(Token.RParen),     // )
            new TokenInfo(Token.Semicolon),  // ;
            new TokenInfo(Token.Bang),  // !
            new TokenInfo(Token.Minus),  // -
            new TokenInfo(Token.Slash),  // /
            new TokenInfo(Token.Asterisk),  // *
            new TokenInfo(Token.Integer, 5), // 5
            new TokenInfo(Token.Semicolon), // ;
            new TokenInfo(Token.Integer, 5), // 5
            new TokenInfo(Token.LT), // <
            new TokenInfo(Token.Integer, 10), // 10
            new TokenInfo(Token.GT), // >
            new TokenInfo(Token.Integer, 5), // 5
            new TokenInfo(Token.Semicolon), // ;
            new TokenInfo(Token.If), // if
            new TokenInfo(Token.LParen), // (
            new TokenInfo(Token.Integer, 5), // 5
            new TokenInfo(Token.LT), // <
            new TokenInfo(Token.Integer, 10), // 10
            new TokenInfo(Token.RParen), // )
            new TokenInfo(Token.LSquirly), // {
            new TokenInfo(Token.Return), // return
            new TokenInfo(Token.True), // true
            new TokenInfo(Token.Semicolon), // ;
            new TokenInfo(Token.RSquirly), // }
            new TokenInfo(Token.Else), // else
            new TokenInfo(Token.LSquirly), // {
            new TokenInfo(Token.Return), // return
            new TokenInfo(Token.False), // false
            new TokenInfo(Token.Semicolon), // ;
            new TokenInfo(Token.RSquirly), // }
            new TokenInfo(Token.Eof),
            new TokenInfo(Token.Integer, 10), // 10
            new TokenInfo(Token.EQ), // ==
            new TokenInfo(Token.Integer, 10), // 10
            new TokenInfo(Token.Semicolon), // ;
            new TokenInfo(Token.Integer, 10), // 10
            new TokenInfo(Token.NOT_EQ), // !=
            new TokenInfo(Token.Integer, 9), // 9
            new TokenInfo(Token.Semicolon), // ;

        };

        
        Assert.That(tokens, Is.EquivalentTo(expectedResult));
    }
}