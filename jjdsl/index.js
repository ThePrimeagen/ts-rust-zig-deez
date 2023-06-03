function test_next_token_function() {
    var basic_tokens = "=+(){},;";
    var lexer = new Lexer(basic_tokens);
    var expected_tokens = ["=", "+", "(", ")", "{", "}", ",", ";"];
    for (var index = 0; index < expected_tokens.length; index++) {
        var next_token = lexer.nextToken().tokenType;
        if (next_token !== expected_tokens[index]) {
            return "FAILED TO PARSE TOKEN: " + next_token + " EXPECTED: " + expected_tokens[index];
        }
    }
    return null;
}

function test_get_next_complete() {
    var input = "let five = 5;" +
        "let ten = 10;" +
        "let add = fn(x, y) {" +
        "x + y;" +
        "};" +
        "let result = add(five, ten);";

    var lexer = new Lexer(input);

    var expected_tokens = [
        new Token("LET", undefined),
        new Token("IDENT", "five"),
        new Token("=", undefined),
        new Token("INT", "5"),
        new Token(";", undefined),
        new Token("LET", undefined),
        new Token("IDENT", "ten"),
        new Token("=", undefined),
        new Token("INT", "10"),
        new Token(";"),
        new Token("LET"),
        new Token("IDENT", "add"),
        new Token("=", undefined),
        new Token("FUNCTION", undefined),
        new Token("(", undefined),
        new Token("IDENT", "x"),
        new Token(",", undefined),
        new Token("IDENT", "y"),
        new Token(")", undefined),
        new Token("{", undefined),
        new Token("IDENT", "x"),
        new Token("+", undefined),
        new Token("IDENT", "y"),
        new Token(";", undefined),
        new Token("}", undefined),
        new Token(";", undefined),
        new Token("LET", undefined),
        new Token("IDENT", "result"),
        new Token("=", undefined),
        new Token("IDENT", "add"),
        new Token("(", undefined),
        new Token("IDENT", "five"),
        new Token(",", undefined),
        new Token("IDENT", "ten"),
        new Token(")", undefined),
        new Token(";", undefined),
        new Token("EOF", "")
    ];
    for (var index = 0; index < expected_tokens.length; index++) {
        var next_token = lexer.nextToken();
        if (!next_token.softCheck(expected_tokens[index])) {
            return "FAILED TO PARSE TOKEN: " + next_token.readable() + " EXPECTED [" + index + "]: " + expected_tokens[index].readable();
        }
    }
    return null;
}

function test() {
    var results = test_next_token_function();
    if (results !== null) {
        return results;
    }
    results = test_get_next_complete();
    if (results !== null) {
        return results;
    }
    return "Tests Passed";
}