-module(lexer).
-export([init/1, next_token/1]).

-record(lexer, {position = 0, read_position = 0, ch = 0, input = []}).

%% Types
-type token() :: ident | integer | illegal | eof |
                equal | plus | comma | semicolon |
                lparen | rparen | lsquirly | rsquirly |
                function | let.

init(Input) ->
    Lexer = #lexer{input = Input},
    read_char(Lexer).

next_token(#lexer{ch = Ch} = Lexer) ->
    Token = case Ch of
                ${$=} -> equal;
                ${$+} -> plus;
                ${$(} -> lparen;
                ${$)} -> rparen;
                ${${} -> lsquirly;
                ${$}} -> rsquirly;
                ${$,} -> comma;
                ${$;} -> semicolon;
                0     -> eof;
                _     -> illegal
            end,
    {Token, read_char(Lexer#lexer{ch = Token})}.

read_char(#lexer{read_position = RP, input = Input} = Lexer) ->
    if
        RP >= length(Input) ->
            Lexer#lexer{ch = 0};
        true ->
            {Ch, _} = lists:split_at(RP, Input),
            Lexer#lexer{ch = Ch, read_position = RP + 1}
    end
