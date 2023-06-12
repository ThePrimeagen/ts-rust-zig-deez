:- module(lexer, [lex/2]).

lex(StringCodes, Tokens) :- phrase(tok(Tokens), StringCodes).

tok([Token | Ts]) --> ch(punct, [C|Cs]), {lexicon([C|Cs], Token)}, !, tok(Ts).
tok([Token | Ts]) --> ch(alpha, [C|Cs]), {lexicon([C|Cs], Token)}, !, tok(Ts).
tok([int(Number) | Ts]) --> ch(digit, [C|Cs]), {number_codes(Number, [C|Cs])}, !, tok(Ts).
tok([ident(Atom) | Ts]) --> ch(alpha, [C|Cs]), {atom_codes(Atom, [C|Cs])}, !, tok(Ts).
tok(Ts) --> ch(space, [_|_]), !, tok(Ts).
tok([eof]) --> [].

ch(Type, [C|Cs]) --> [C], {code_type(C, Type)}, ch(Type, Cs).
ch(_, []) --> [].

lexicon([61], assign).
lexicon([43], plus).
lexicon([44], comma).
lexicon([59], semicolon).
lexicon([40], lparen).
lexicon([41], rparen).
lexicon([123], lsquirly).
lexicon([125], rsquirly).
lexicon([33], bang).
lexicon([45], dash).
lexicon([47], fslash).
lexicon([42], asterisk).
lexicon([60], lt).
lexicon([62], gt).
lexicon([61, 61], eq).
lexicon([33, 61], neq).
% lexicon([62, 61], geq).
% lexicon([60, 61], leq).
lexicon([108, 101, 116], let).
lexicon([102, 110], function).
lexicon([105, 102], if).
lexicon([101, 108, 115, 101], else).
lexicon([116, 114, 117, 101], true).
lexicon([102, 97, 108, 115, 101], false).
lexicon([114, 101, 116, 117, 114, 110], return).
