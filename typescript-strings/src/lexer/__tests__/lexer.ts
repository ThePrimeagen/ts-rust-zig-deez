import { Keyword, Punctuation, tokenIsIdentifier, tokenIsKeyword, tokenIsPunctuation, tokenIsInt, Tokenizer } from "..";

test("test getNextToken()", function() {
    const input = `=+(){},;`;

    const tokens: Punctuation[] = [
        "=",
        "+",
        "(",
        ")",
        "{",
        "}",
        ",",
        ";"
    ];

    const lexer = new Tokenizer(input);

    for (const token of tokens) {
        const tok = lexer.getNextToken();        
        expect(tok).toBe(token);
    }

});


test("test getNextToken() complete", function() {
    const input =`let five = 5;
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
        `;

    var lex = new Tokenizer(input);

    function keyword(word: Keyword) {
        return (tok: string) => expect(tokenIsKeyword(tok, word));
    }

    function ident(word: string) {
        expect(tokenIsIdentifier(word));
        return (tok: string) => expect(tok).toBe(word);
    }

    function int(word: string) {
        expect(tokenIsInt(word));
        return (tok: string) => expect(tok).toBe(word);
    }

    function punc(word: Punctuation) {
        return (tok: string) => expect(tokenIsPunctuation(tok, word));
    }

    var expected = [
        keyword("let"),
        ident("five"),
        punc("="),
        int("5"),
        punc(";"),
        keyword("let"),
        ident("ten"),
        punc("="),
        int("10"),
        punc(";"),
        keyword("let"),
        ident("add"),
        punc("="),
        keyword("fn"),
        punc("("),
        ident("x"),
        punc(","),
        ident("y"),
        punc(")"),
        punc("{"),
        ident("x"),
        punc("+"),
        ident("y"),
        punc(";"),
        punc("}"),
        punc(";"),
        keyword("let"),
        ident("result"),
        punc("="),
        ident("add"),
        punc(")"),
        ident("five"),
        punc(","),
        ident("ten"),
        punc(")"),
        punc(";"),

        punc("!"),
        punc("-"),
        punc("/"),
        punc("*"),
        int("5"),
        punc(";"),
        int("5"),
        punc("<"),
        int("10"),
        punc(">"),
        int("5"),
        punc(";"),

        keyword("if"),
        punc(")"),
        int("5"),
        punc("<"),
        int("10"),
        punc(")"),
        punc("{"),
        keyword("return"),
        keyword("true"),
        punc(";"),
        punc("}"),
        keyword("else"),
        punc("{"),
        keyword("return"),
        keyword("false"),
        punc(";"),
        punc("}"),

        int("10"),
        punc("=="),
        int("10"),
        punc(";"),
        int("10"),
        punc("!="),
        int("9"),
        punc(";"),
    ];

    for (const func of expected) {
        func(lex.getNextToken());
    }

    expect(lex.getNextToken()).toBe("\0");
});
