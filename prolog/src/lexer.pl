:- module(lexer, [lex/2]).

drop(0, In, Out) :- In = Out.
drop(N, In, Out) :-
    [_|T] = In,
    N > 0,
    NN is N - 1,
    drop(NN, T, Out).

drop_while(Pred, In, Out) :-
    (In = [] -> Out = []);
    [H|T] = In,
    call(Pred, H) ->
        drop_while(Pred, T, Out);
        Out = In.

is_code_space(Code) :-
    char_code(Char,Code),
    char_type(Char, white).

list_take_while([], _, [], []).
list_take_while([H|T], Pred, Taken, Rest) :- 
    call(Pred, H) -> (
            Taken = [H | TTaken],
            list_take_while(T, Pred, TTaken, Rest)
        );
        (
            Taken = [],
            Rest = [H|T]
        ).

lex_while(In, Pred, Rest, Result) :-
    list_take_while(In, Pred, ResultCodes, Rest),
    ResultCodes \= [],
    string_codes(Result, ResultCodes).


is_code_digit(Code) :-
    char_code(Char, Code),
    is_digit(Char, 10, _).

lex_int(In, Rest, Result) :-
    lex_while(In, is_code_digit, Rest, Result).


is_code_identifier(Code) :-
    is_alpha(Code).

lex_identifier(In, Rest, Result) :-
    lex_while(In, is_code_identifier, Rest, Result).


lex_literal(In, Rest, Result) :-
    Literals = [
        (assign, "="),
        (plus, "+"),
        (comma, ","),
        (semicolon, ";"),
        (lparen, "("),
        (rparen, ")"),
        (lbrace, "{"),
        (rbrace, "}"),
        (let: "let"),
        (function: "fn")
    ],
    findall(
        (R, Literal),
        (
            member(Literal, Literals),
            (_, Repr) = Literal,
            string_codes(Repr, ReprCodes),

            prefix(ReprCodes, In),

            length(ReprCodes, ReprLen),
            drop(ReprLen, In, R)
        ),
        [(Rest, Result)|_]
    ).

lex_illegal(In, [], Result) :-
    string_codes(Result, In).

lex_eof([], [], '').

lex_next(In, Rest, LexResult) :-
    lex_int(In, Rest, Result) -> LexResult = (int, Result);
    lex_literal(In, Rest, Result) -> LexResult = Result;
    lex_identifier(In, Rest, Result) -> LexResult = (ident, Result);
    lex_illegal(In, Rest, Result) -> LexResult = (illegal, Result).

lex_all_codes(Input, Result) :-
    (Input = [] -> Result = [(eof, '')]);
    lex_next(Input, NextInput, LexResult),
    drop_while(is_code_space, NextInput, ClearedNextInput),
    lex_all_codes(ClearedNextInput, Acc),
    Result = [LexResult|Acc].

lex(Input, Result) :-
    string_codes(Input, InputCodes),
    drop_while(is_code_space, InputCodes, ClearedInputCodes),
    lex_all_codes(ClearedInputCodes, Result).



:- begin_tests(lex).

test(lex) :-
    Input = "let five = 5;",
    Expected = [(ident,"let"),(ident,"five"),(assign,"="),(int,"5"),(semicolon,";"),(eof,'')],
    lex(Input, Obtained),
    assertion(Expected == Obtained).


test(lex) :-
    Input = "let add = fn(x, y) { x + y; };",
    Expected = [
        (ident,"let"),(ident,"add"),(assign,"="),
        (ident,"fn"),(lparen,"("),(ident,"x"),(comma,","),(ident,"y"),(rparen,")"),(lbrace,"{"),
        (ident,"x"),(plus,"+"),(ident,"y"),(semicolon,";"),
        (rbrace,"}"),(semicolon,";")
        ,(eof,'')
    ],
    lex(Input, Obtained),
    assertion(Expected == Obtained).

test(lex) :-
    Input = "let five = 5;\c
        let ten = 10;\c
        let add = fn(x, y) {\c
            x + y;\c
        };\c
        let result = add(five, ten);",
    Expected = [
        (ident,"let"),(ident,"five"),(assign,"="),(int,"5"),(semicolon,";"),
        (ident,"let"),(ident,"ten"),(assign,"="),(int,"10"),(semicolon,";"),
        (ident,"let"),(ident,"add"),(assign,"="),
        (ident,"fn"),(lparen,"("),(ident,"x"),(comma,","),(ident,"y"),(rparen,")"),
        (lbrace,"{"),(ident,"x"),(plus,"+"),(ident,"y"),(semicolon,";"),(rbrace,"}"),(semicolon,";"),
        (ident,"let"),(ident,"result"),(assign,"="),
        (ident,"add"),(lparen,"("),(ident,"five"),(comma,","),(ident,"ten"),(rparen,")"),(semicolon,";"),(eof,'')
    ],
    lex(Input, Obtained),
    assertion(Expected == Obtained).


:- end_tests(lex).

