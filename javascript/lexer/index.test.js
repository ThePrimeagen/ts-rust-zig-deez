const { tokenTypes, Lexer } = require(".");

test("get_next_token", () => {
    const input = "=+(){},;";
    const lexer = new Lexer(input);

    const tokens = [
        tokenTypes.EQUAL,
        tokenTypes.PLUS,
        tokenTypes.LPAREN,
        tokenTypes.RPAREN,
        tokenTypes.LBRACE,
        tokenTypes.RBRACE,
        tokenTypes.COMMA,
        tokenTypes.SEMI,
    ];

    for (const token of tokens) {
        const nextToken = lexer.nextToken();
        expect(nextToken).toEqual(token);
    }
});

test("get_next_complete", () => {
    const input = `let five = 5;
    let ten = 10;
    let add = fn(x, y) {
      x + y;
    };
    let result = add(five, ten);`;

    const lexer = new Lexer(input);

    const tokens = [
        tokenTypes.LET,
        { type: tokenTypes.IDENT, value: "five" },
        tokenTypes.EQUAL,
        { type: tokenTypes.INT, value: "5" },
        tokenTypes.SEMI,
        tokenTypes.LET,
        { type: tokenTypes.IDENT, value: "ten" },
        tokenTypes.EQUAL,
        { type: tokenTypes.INT, value: "10" },
        tokenTypes.SEMI,
        tokenTypes.LET,
        { type: tokenTypes.IDENT, value: "add" },
        tokenTypes.EQUAL,
        tokenTypes.FUNCTION,
        tokenTypes.LPAREN,
        { type: tokenTypes.IDENT, value: "x" },
        tokenTypes.COMMA,
        { type: tokenTypes.IDENT, value: "y" },
        tokenTypes.RPAREN,
        tokenTypes.LBRACE,
        { type: tokenTypes.IDENT, value: "x" },
        tokenTypes.PLUS,
        { type: tokenTypes.IDENT, value: "y" },
        tokenTypes.SEMI,
        tokenTypes.RBRACE,
        tokenTypes.SEMI,
        tokenTypes.LET,
        { type: tokenTypes.IDENT, value: "result" },
        tokenTypes.EQUAL,
        { type: tokenTypes.IDENT, value: "add" },
        tokenTypes.LPAREN,
        { type: tokenTypes.IDENT, value: "five" },
        tokenTypes.COMMA,
        { type: tokenTypes.IDENT, value: "ten" },
        tokenTypes.RPAREN,
        tokenTypes.SEMI,
        tokenTypes.EOF,
    ];

    for (const token of tokens) {
        const nextToken = lexer.nextToken();
        expect(nextToken).toEqual(token);
    }
});
