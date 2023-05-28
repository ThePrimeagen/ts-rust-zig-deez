:- module(tokenizer, [tokenize/2]).

tokenize(StringCodes, Tokens) :- phrase(tok(Tokens), StringCodes).

tok([Token | Ts]) --> [C], {lex(C, Token)}, !, tok(Ts).
tok([Token | Ts]) --> ch(alpha, [C|Cs]), {lex([C|Cs], Token)}, !, tok(Ts).
tok([int(Number) | Ts]) --> ch(digit, [C|Cs]), {number_codes(Number, [C|Cs])}, !, tok(Ts).
tok([ident(Atom) | Ts]) --> ch(alpha, [C|Cs]), {atom_codes(Atom, [C|Cs])}, !, tok(Ts).
tok(Ts) --> ch(space, [_]), !, tok(Ts).
tok([eof]) --> [].

ch(Type, [C|Cs]) --> [C], {code_type(C, Type)}, !, ch(Type, Cs).
ch(_, []) --> [].

lex(61, equal).
lex(43, plus).
lex(44, comma).
lex(59, semicolon).
lex(40, lparen).
lex(41, rparen).
lex(123, lsquirly).
lex(125, rsquirly).
lex([108, 101, 116], let).
lex([102, 110], function).
