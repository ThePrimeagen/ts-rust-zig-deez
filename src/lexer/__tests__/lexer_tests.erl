-module(lexer_tests).
-include_lib("eunit/include/eunit.hrl").

get_next_token_test() ->
    Input = "=+(){},;",
    Lexer = lexer:init(Input),
    ExpectedTokens = [equal, plus, lparen, rparen, lsquirly, rsquirly, comma, semicolon],
    lists:foreach(fun(Expected) ->
                      {Token, Lexer1} = lexer:next_token(Lexer),
                      ?assertEqual(Expected, Token),
                      Lexer = Lexer1
                  end, ExpectedTokens).
