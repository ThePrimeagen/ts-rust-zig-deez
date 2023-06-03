def TestNextToken() {
    input = '=+(){},;';

    def tests = [
        // idk why vim indent weirdly like this
        new Token(TokenType.ASSIGN, "="),
            new Token(TokenType.PLUS, "+"),
            new Token(TokenType.LPAREN, "("),
            new Token(TokenType.RPAREN, ")"),
            new Token(TokenType.LBRACE, "{"),
            new Token(TokenType.RBRACE, "}"),
            new Token(TokenType.COMMA, ","),
            new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.EOF, "")
    ] as Token[];

    l = new Lexer(input);

    for (expectedToken in tests) {
        tok = l.NextToken();
        assert(tok.Type == expectedToken.Type);
        assert(tok.Literal?.trim() == expectedToken.Literal?.trim());
    }
    println("=== TEST 1 PASSED ===")
}

def TestMoreTokens() {
    input = """let five = 5; let ten = 10; let add = fn(x, y) { x + y; }; let result = add(five, ten); """;

    Token[] tests = [
        new Token(TokenType.LET, "let"),
            new Token(TokenType.IDENT, "five"),
            new Token(TokenType.ASSIGN, "="),
            new Token(TokenType.INT, "5"),
            new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.LET, "let"),
            new Token(TokenType.IDENT, "ten"),
            new Token(TokenType.ASSIGN, "="),
            new Token(TokenType.INT, "10"),
            new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.LET, "let"),
            new Token(TokenType.IDENT, "add"),
            new Token(TokenType.ASSIGN, "="),
            new Token(TokenType.FUNCTION, "fn"),
            new Token(TokenType.LPAREN, "("),
            new Token(TokenType.IDENT, "x"),
            new Token(TokenType.COMMA, ","),
            new Token(TokenType.IDENT, "y"),
            new Token(TokenType.RPAREN, ")"),
            new Token(TokenType.LBRACE, "{"),
            new Token(TokenType.IDENT, "x"),
            new Token(TokenType.PLUS, "+"),
            new Token(TokenType.IDENT, "y"),
            new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.RBRACE, "}"),
            new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.LET, "let"),
            new Token(TokenType.IDENT, "result"),
            new Token(TokenType.ASSIGN, "="),
            new Token(TokenType.IDENT, "add"),
            new Token(TokenType.LPAREN, "("),
            new Token(TokenType.IDENT, "five"),
            new Token(TokenType.COMMA, ","),
            new Token(TokenType.IDENT, "ten"),
            new Token(TokenType.RPAREN, ")"),
            new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.EOF, "")
                ];

    l = new Lexer(input);

    for (expectedToken in tests) {
        tok = l.NextToken();

        assert(tok.Type == expectedToken.Type);
        assert(tok.Literal?.trim() == expectedToken.Literal?.trim());
    }

    println("=== TEST 2 PASSED ===")
}

def TestEvenMoreTokens() {
    input = """let five = 5; let ten = 10; let add = fn(x, y) { x + y; }; let result = add(five, ten); !-/*5; 5 < 10 > 5; """;
    Token[] tests = [
        new Token(TokenType.LET, "let"),
            new Token(TokenType.IDENT, "five"),
            new Token(TokenType.ASSIGN, "="),
            new Token(TokenType.INT, "5"),
            new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.LET, "let"),
            new Token(TokenType.IDENT, "ten"),
            new Token(TokenType.ASSIGN, "="),
            new Token(TokenType.INT, "10"),
            new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.LET, "let"),
            new Token(TokenType.IDENT, "add"),
            new Token(TokenType.ASSIGN, "="),
            new Token(TokenType.FUNCTION, "fn"),
            new Token(TokenType.LPAREN, "("),
            new Token(TokenType.IDENT, "x"),
            new Token(TokenType.COMMA, ","),
            new Token(TokenType.IDENT, "y"),
            new Token(TokenType.RPAREN, ")"),
            new Token(TokenType.LBRACE, "{"),
            new Token(TokenType.IDENT, "x"),
            new Token(TokenType.PLUS, "+"),
            new Token(TokenType.IDENT, "y"),
            new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.RBRACE, "}"),
            new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.LET, "let"),
            new Token(TokenType.IDENT, "result"),
            new Token(TokenType.ASSIGN, "="),
            new Token(TokenType.IDENT, "add"),
            new Token(TokenType.LPAREN, "("),
            new Token(TokenType.IDENT, "five"),
            new Token(TokenType.COMMA, ","),
            new Token(TokenType.IDENT, "ten"),
            new Token(TokenType.RPAREN, ")"),
            new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.BANG, "!"),
            new Token(TokenType.MINUS, "-"),
            new Token(TokenType.SLASH, "/"),
            new Token(TokenType.ASTERISK, "*"),
            new Token(TokenType.INT, "5"),
            new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.INT, "5"),
            new Token(TokenType.LT, "<"),
            new Token(TokenType.INT, "10"),
            new Token(TokenType.GT, ">"),
            new Token(TokenType.INT, "5"),
            new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.EOF, "")
                ];

    l = new Lexer(input);

    for (expectedToken in tests) {
        tok = l.NextToken();

        assert(tok.Type == expectedToken.Type);
        assert(tok.Literal?.trim() == expectedToken.Literal?.trim());
    }

    println("=== TEST 3 PASSED ===")
}

def TestEvenEvenMoreTokens() {
    input = """let five = 5; let ten = 10; let add = fn(x, y) { x + y; }; let result = add(five, ten); !-/*5; 5 < 10 > 5; if (5 < 10) { return true; } else { return false; } """;

    Token[] tests = [
        new Token(TokenType.LET, "let"), new Token(TokenType.IDENT, "five"), new Token(TokenType.ASSIGN, "="),
            new Token(TokenType.INT, "5"), new Token(TokenType.SEMICOLON, ";"), new Token(TokenType.LET, "let"),
            new Token(TokenType.IDENT, "ten"), new Token(TokenType.ASSIGN, "="), new Token(TokenType.INT, "10"),
            new Token(TokenType.SEMICOLON, ";"), new Token(TokenType.LET, "let"), new Token(TokenType.IDENT, "add"),
            new Token(TokenType.ASSIGN, "="), new Token(TokenType.FUNCTION, "fn"), new Token(TokenType.LPAREN, "("),
            new Token(TokenType.IDENT, "x"), new Token(TokenType.COMMA, ","), new Token(TokenType.IDENT, "y"),
            new Token(TokenType.RPAREN, ")"), new Token(TokenType.LBRACE, "{"), new Token(TokenType.IDENT, "x"),
            new Token(TokenType.PLUS, "+"), new Token(TokenType.IDENT, "y"), new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.RBRACE, "}"), new Token(TokenType.SEMICOLON, ";"), new Token(TokenType.LET, "let"),
            new Token(TokenType.IDENT, "result"), new Token(TokenType.ASSIGN, "="), new Token(TokenType.IDENT, "add"),
            new Token(TokenType.LPAREN, "("), new Token(TokenType.IDENT, "five"), new Token(TokenType.COMMA, ","),
            new Token(TokenType.IDENT, "ten"), new Token(TokenType.RPAREN, ")"), new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.BANG, "!"), new Token(TokenType.MINUS, "-"), new Token(TokenType.SLASH, "/"),
            new Token(TokenType.ASTERISK, "*"), new Token(TokenType.INT, "5"), new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.INT, "5"), new Token(TokenType.LT, "<"), new Token(TokenType.INT, "10"),
            new Token(TokenType.GT, ">"), new Token(TokenType.INT, "5"), new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.IF, "if"), new Token(TokenType.LPAREN, "("), new Token(TokenType.INT, "5"),
            new Token(TokenType.LT, "<"), new Token(TokenType.INT, "10"), new Token(TokenType.RPAREN, ")"),
            new Token(TokenType.LBRACE, "{"), new Token(TokenType.RETURN, "return"), new Token(TokenType.TRUE, "true"),
            new Token(TokenType.SEMICOLON, ";"), new Token(TokenType.RBRACE, "}"), new Token(TokenType.ELSE, "else"),
            new Token(TokenType.LBRACE, "{"), new Token(TokenType.RETURN, "return"), new Token(TokenType.FALSE, "false"),
            new Token(TokenType.SEMICOLON, ";"), new Token(TokenType.RBRACE, "}"),
            new Token(TokenType.EOF, "")
                ];

    l = new Lexer(input);

    for (expectedToken in tests) {
        tok = l.NextToken();

        assert(tok.Type == expectedToken.Type);
        assert(tok.Literal?.trim() == expectedToken.Literal?.trim());
    }

    println("=== TEST 4 PASSED ===")
}

def TestEvenEvenEvenMoreTokens() {
    input = """let five = 5; let ten = 10; let add = fn(x, y) { x + y; }; let result = add(five, ten); !-/*5; 5 < 10 > 5; if (5 < 10) { return true; } else { return false; } 10 == 10; 10 != 9; """;

    Token[] tests = [
        new Token(TokenType.LET, "let"), new Token(TokenType.IDENT, "five"), new Token(TokenType.ASSIGN, "="),
            new Token(TokenType.INT, "5"), new Token(TokenType.SEMICOLON, ";"), new Token(TokenType.LET, "let"),
            new Token(TokenType.IDENT, "ten"), new Token(TokenType.ASSIGN, "="), new Token(TokenType.INT, "10"),
            new Token(TokenType.SEMICOLON, ";"), new Token(TokenType.LET, "let"), new Token(TokenType.IDENT, "add"),
            new Token(TokenType.ASSIGN, "="), new Token(TokenType.FUNCTION, "fn"), new Token(TokenType.LPAREN, "("),
            new Token(TokenType.IDENT, "x"), new Token(TokenType.COMMA, ","), new Token(TokenType.IDENT, "y"),
            new Token(TokenType.RPAREN, ")"), new Token(TokenType.LBRACE, "{"), new Token(TokenType.IDENT, "x"),
            new Token(TokenType.PLUS, "+"), new Token(TokenType.IDENT, "y"), new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.RBRACE, "}"), new Token(TokenType.SEMICOLON, ";"), new Token(TokenType.LET, "let"),
            new Token(TokenType.IDENT, "result"), new Token(TokenType.ASSIGN, "="), new Token(TokenType.IDENT, "add"),
            new Token(TokenType.LPAREN, "("), new Token(TokenType.IDENT, "five"), new Token(TokenType.COMMA, ","),
            new Token(TokenType.IDENT, "ten"), new Token(TokenType.RPAREN, ")"), new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.BANG, "!"), new Token(TokenType.MINUS, "-"), new Token(TokenType.SLASH, "/"),
            new Token(TokenType.ASTERISK, "*"), new Token(TokenType.INT, "5"), new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.INT, "5"), new Token(TokenType.LT, "<"), new Token(TokenType.INT, "10"),
            new Token(TokenType.GT, ">"), new Token(TokenType.INT, "5"), new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.IF, "if"), new Token(TokenType.LPAREN, "("), new Token(TokenType.INT, "5"),
            new Token(TokenType.LT, "<"), new Token(TokenType.INT, "10"), new Token(TokenType.RPAREN, ")"),
            new Token(TokenType.LBRACE, "{"), new Token(TokenType.RETURN, "return"), new Token(TokenType.TRUE, "true"),
            new Token(TokenType.SEMICOLON, ";"), new Token(TokenType.RBRACE, "}"), new Token(TokenType.ELSE, "else"),
            new Token(TokenType.LBRACE, "{"), new Token(TokenType.RETURN, "return"), new Token(TokenType.FALSE, "false"),
            new Token(TokenType.SEMICOLON, ";"), new Token(TokenType.RBRACE, "}"), new Token(TokenType.INT, "10"),
            new Token(TokenType.EQ, "=="), new Token(TokenType.INT, "10"), new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.INT, "10"), new Token(TokenType.NOT_EQ, "!="), new Token(TokenType.INT, "9"),
            new Token(TokenType.SEMICOLON, ";"),
            new Token(TokenType.EOF, "")
                ];

    l = new Lexer(input);

    for (expectedToken in tests) {
        tok = l.NextToken();

        assert(tok.Type == expectedToken.Type);
        assert(tok.Literal?.trim() == expectedToken.Literal?.trim());
    }

    println("=== TEST 5 PASSED ===")
};

TestNextToken();
TestMoreTokens();
TestEvenMoreTokens();
TestEvenEvenMoreTokens();
TestEvenEvenEvenMoreTokens();
