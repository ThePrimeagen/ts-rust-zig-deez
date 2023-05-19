import { TokenType, Tokenizer } from "..";

test("test getNextToken()", function() {
    const input = `=+(){},;`;

    const tokens = [
        TokenType.Equal,
        TokenType.Plus,
        TokenType.LParen,
        TokenType.RParen,
        TokenType.LSquirly,
        TokenType.RSquirly,
        TokenType.Comma,
        TokenType.Semi,
    ];

    const lexer = new Tokenizer(input);

    for (const token of tokens) {
        expect(lexer.getNextToken().type).toBe(token);
    }

});

