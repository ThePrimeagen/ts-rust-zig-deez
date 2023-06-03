using Monkey;

namespace MonkeyTests;

[UsesVerify]
public class LexerTests
{
    [Fact]
    public Task ParseTokens_ShouldParseSimpleCharacters() 
        => VerifyLexerOn("=+(){},;");

    [Fact]
    public Task ParseTokens_ShouldParseDigitsEndingWithLettersAsNumbersAndIdentifiers() 
        => VerifyLexerOn("123456abc");
    
    [Fact]
    public Task ParseTokens_ShouldParseDigitsStartingWithLetterAsIdentifier() 
        => VerifyLexerOn("a123456");

    [Fact]
    public Task ParseTokens_ShouldParseAnExampleProgram()
    {
        const string input = """
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
        
        return VerifyLexerOn(input);
    }

    private static Task VerifyLexerOn(string input)
        => VerifyTokens(Lexer.ParseTokens(input));
    
    private static Task VerifyTokens(IEnumerable<Token> tokens) 
        => Verify(tokens.Select(token => token.ToString()));
}