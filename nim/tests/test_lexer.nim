import unittest
import lexer

suite "Lexer tests":
    test "get_next_token with identifiers and literals":
        let input = "let five = 5; let ten = 10; fn add(x, y) { x + y; };"
        var lexer = newLexer(input)

        let tokens = @[
            Token(kind: tkLet),
            Token(kind: tkIdent, value: "five"),
            Token(kind: tkEqual),
            Token(kind: tkInt, value: "5"),
            Token(kind: tkSemicolon),
            Token(kind: tkLet),
            Token(kind: tkIdent, value: "ten"),
            Token(kind: tkEqual),
            Token(kind: tkInt, value: "10"),
            Token(kind: tkSemicolon),
            Token(kind: tkFunction),
            Token(kind: tkIdent, value: "add"),
            Token(kind: tkLparen),
            Token(kind: tkIdent, value: "x"),
            Token(kind: tkComma),
            Token(kind: tkIdent, value: "y"),
            Token(kind: tkRparen),
            Token(kind: tkLSquirly),
            Token(kind: tkIdent, value: "x"),
            Token(kind: tkPlus),
            Token(kind: tkIdent, value: "y"),
            Token(kind: tkSemicolon),
            Token(kind: tkRSquirly),
            Token(kind: tkSemicolon),
        ]

        for token in tokens:
            let next_token = lexer.next_token()
            check next_token.kind == token.kind
            if token.kind in {tkIdent, tkInt}:
                echo token
                # check next_token.value == token.value

