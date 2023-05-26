-module(tokenizer_tests).
-include_lib("eunit/include/eunit.hrl").

tokenize_test_1() ->
    Input = "let x = 10;",
    ExpectedTokens = [
        #token{type = let, literal = "let"},
        #token{type = ident, literal = "x"},
        #token{type = equal, literal = "="},
        #token{type = int, literal = "10"},
        #token{type = semicolon, literal = ";"}
    ],
    Tokens = tokenizer:tokenizer(Input),
    ?assertEqual(ExpectedTokens, Tokens).

tokenize_test_2() ->
    Input = "fn add(a, b) { return a + b; }",
    ExpectedTokens = [
        #token{type = function, literal = "fn"},
        #token{type = ident, literal = "add"},
        #token{type = lparen, literal = "("},
        #token{type = ident, literal = "a"},
        #token{type = comma, literal = ","},
        #token{type = ident, literal = "b"},
        #token{type = rparen, literal = ")"},
        #token{type = lsquirly, literal = "{"},
        #token{type = ident, literal = "return"},
        #token{type = ident, literal = "a"},
        #token{type = plus, literal = "+"},
        #token{type = ident, literal = "b"},
        #token{type = semicolon, literal = ";"},
        #token{type = rsquirly, literal = "}"}
    ],
    Tokens = tokenizer:tokenizer(Input),
    ?assertEqual(ExpectedTokens, Tokens).

tokenize_test_3() ->
    Input = "abc123",
    ExpectedTokens = [
        #token{type = ident, literal = "abc123"}
    ],
    Tokens = tokenizer:tokenizer(Input),
    ?assertEqual(ExpectedTokens, Tokens).

tokenize_test_4() ->
    Input = "123",
    ExpectedTokens = [
        #token{type = int, literal = "123"}
    ],
    Tokens = tokenizer:tokenizer(Input),
    ?assertEqual(ExpectedTokens, Tokens).

tokenize_test_5() ->
    Input = "invalid$token",
    ExpectedTokens = [
        #token{type = illegal, literal = "i"},
        #token{type = illegal, literal = "n"},
        #token{type = illegal, literal = "v"},
        #token{type = illegal, literal = "a"},
        #token{type = illegal, literal = "l"},
        #token{type = illegal, literal = "i"},
        #token{type = illegal, literal = "d"},
        #token{type = illegal, literal = "$"},
        #token{type = illegal, literal = "t"},
        #token{type = illegal, literal = "o"},
        #token{type = illegal, literal = "k"},
        #token{type = illegal, literal = "e"},
        #token{type = illegal, literal = "n"}
    ],
    Tokens = tokenizer:tokenizer(Input),
    ?assertEqual(ExpectedTokens, Tokens).

tokenize_test_6() ->
    Input = "",
    ExpectedTokens = [
        #token{type = eof, literal = "eof"}
    ],
    Tokens = tokenizer:tokenizer(Input),
    ?assertEqual(ExpectedTokens, Tokens).

run_tests() ->
    io:format("Running tokenizer tests...~n"),
    Result = eunit:test(tokenizer_tests, [verbose]),
    case Result of
    {ok, _} -> 
        io:format("All tests passed.~n");
    {error, _} -> 
        io:format("Some tests failed.~n")
end.