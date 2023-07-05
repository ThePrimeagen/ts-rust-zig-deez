:- use_module(library(readutil)).
:- use_module(lib/lexer, [lex/2]).
:- use_module(lib/parser, [parse/2]).
:- use_module(lib/evaluator, [evaluate/2]).

main :- repeat, process_line.

process_line :-
    read_line_to_codes(user_input, StringCodes),
    once(lex(StringCodes, Tokens)),
    once(parse(Tokens, Ast)),
    once(evaluate(Ast, Value)),
    Value \= nil,
    write("-> "), write(Value), nl,
    !, fail.
