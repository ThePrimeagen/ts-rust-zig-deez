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
        TokenType.Semicolon,
    ];

    const lexer = new Tokenizer(input);

    for (const token of tokens) {
        expect(lexer.getNextToken().type).toBe(token);
    }

});


test("test getNextToken() complete", function() {
    const input =`let five = 5;
        let ten = 10;
        let add = fn(x, y) {
            x + y;
        };
        let result = add(five, ten);`;

    var lex = new Tokenizer(input);

    var tokens = [
        {type: TokenType.Let, literal: "let"},
        { type: TokenType.Ident, literal: "five" },
        {type: TokenType.Equal, literal: "="},
        { type: TokenType.Int, literal: "5" },
        {type: TokenType.Semicolon, literal: ";"},
        {type: TokenType.Let, literal: "let"},
        { type: TokenType.Ident, literal: "ten" },
        {type: TokenType.Equal, literal: "="},
        { type: TokenType.Int, literal: "10" },
        {type: TokenType.Semicolon, literal: ";"},
        {type: TokenType.Let, literal: "let"},
        { type: TokenType.Ident, literal: "add" },
        {type: TokenType.Equal, literal: "="},
        {type: TokenType.Function, literal: "fn"},
        {type: TokenType.LParen, literal: "("},
        { type: TokenType.Ident, literal: "x" },
        {type: TokenType.Comma, literal: ","},
        { type: TokenType.Ident, literal: "y" },
        {type: TokenType.RParen, literal: ")"},
        {type: TokenType.LSquirly, literal: "{"},
        { type: TokenType.Ident, literal: "x" },
        {type: TokenType.Plus, literal: "+"},
        { type: TokenType.Ident, literal: "y" },
        {type: TokenType.Semicolon, literal: ";"},
        {type: TokenType.RSquirly, literal: "}"},
        {type: TokenType.Semicolon, literal: ";"},
        {type: TokenType.Let, literal: "let"},
        { type: TokenType.Ident, literal: "result" },
        {type: TokenType.Equal, literal: "="},
        { type: TokenType.Ident, literal: "add" },
        {type: TokenType.LParen, literal: "("},
        { type: TokenType.Ident, literal: "five" },
        {type: TokenType.Comma, literal: ","},
        { type: TokenType.Ident, literal: "ten" },
        {type: TokenType.RParen, literal: ")"},
        {type: TokenType.Semicolon, literal: ";"},
        {type: TokenType.Eof, literal: "eof"},
    ];

    for (const token of tokens) {
        expect(lex.getNextToken()).toEqual(token);
    }
});
