module lexer;

import token;

struct Lexer
{
    string input;
    int position;
    int readPosition;
    byte ch;

    // this(string input) {
    //     this.input = input;
    // }
}

/// TestNextToken
unittest
{
    Lexer lex = Lexer("=+(){},;");
    const input = `=+(){},;`;

    Token[] tests = [
        {ASSIGN, "="},
        {PLUS, "+"},
        {LPAREN, "("},
        {RPAREN, ")"},
        {LBRACE, "{"},
        {RBRACE, "}"},
        {COMMA, ","},
        {SEMICOLON, ";"},
        {EOF, ""},
    ];

    Lexer l = input;

    foreach(t; tests)
    {
        const tok = l.NextToken();

        assert(tok.type == t.type,
            format("Wrong token type %s expected %s", tok.type, t.type)
        );
        assert(tok.literal == t.literal,
            format("Wrong literal %s expected %s", tok.literal, t.literal)
        );
    }

    // assert(lex("let x = 5 + 5;") == [LET, IDENTIFIER("x"), EQUAL_SIGN, INTEGER(5), PLUS_SIGN, INTEGER(5), SEMICOLON]);
}
