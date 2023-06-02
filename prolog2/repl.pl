:- use_module(library(readutil)).
:- use_module(lexer, [lex/2]).

main :- repeat, process_line.

process_line :- 
    read_line_to_codes(user_input, StringCodes),
    lex(StringCodes, Tokens),
    write_tokens(Tokens), !, fail.

write_tokens([eof]).
write_tokens([Token | Ts]) :-
    write(Token), nl,
    write_tokens(Ts).
