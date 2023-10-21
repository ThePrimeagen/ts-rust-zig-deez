:- use_module(library(readutil)).
:- use_module(lib/lexer, [lex/2]).
:- use_module(lib/parser, [parse/2]).
:- use_module(lib/evaluator, [evaluate/2]).

main :- repeat, process_line.

process_line :-
    read_line_to_codes(user_input, StringCodes),
    lex(StringCodes, Tokens),
    parse(Tokens, Ast),
    evaluate(Ast, Value),
    write("-> "), write(Value), nl,
    !, fail.
