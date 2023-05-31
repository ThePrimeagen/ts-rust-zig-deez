import unittest
import lexer
import options
import sequtils

suite "Lexer tests":


    test "get_next_token with literals":
        let input = "=+(){},;";

        let expected_tokens_0 = @[
            Token(kind: tkEqual),
            Token(kind: tkPlus),
            Token(kind: tkLParen),
            Token(kind: tkRParen),
            Token(kind: tkLSquirly),
            Token(kind: tkRSquirly),
            Token(kind: tkComma),
            Token(kind: tkSemicolon),
            Token(kind: tkEof)
        ];

        var lex = newLexer(input)
        var tokens_0 = lex.tokens().toSeq()

        check tokens_0 == expected_tokens_0

    test "get_next_token with identifiers and literals":

        let input = """let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);"""
            
        let expected_tokens_1 = @[
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
            Token(kind: tkLet),
            Token(kind: tkIdent, value: "add"),
            Token(kind: tkEqual),
            Token(kind: tkFunction),
            Token(kind: tkLParen),
            Token(kind: tkIdent, value: "x"),
            Token(kind: tkComma),
            Token(kind: tkIdent, value: "y"),
            Token(kind: tkRParen),
            Token(kind: tkLSquirly),
            Token(kind: tkIdent, value: "x"),
            Token(kind: tkPlus),
            Token(kind: tkIdent, value: "y"),
            Token(kind: tkSemicolon),
            Token(kind: tkRSquirly),
            Token(kind: tkSemicolon),
            Token(kind: tkLet),
            Token(kind: tkIdent, value: "result"),
            Token(kind: tkEqual),
            Token(kind: tkIdent, value: "add"),
            Token(kind: tkLParen),
            Token(kind: tkIdent, value: "five"),
            Token(kind: tkComma),
            Token(kind: tkIdent, value: "ten"),
            Token(kind: tkRParen),
            Token(kind: tkSemicolon),
            Token(kind: tkEof),
        ]

        var lex = newLexer(input)
        var tokens_1 = lex.tokens().toSeq()

        check tokens_1 == expected_tokens_1
