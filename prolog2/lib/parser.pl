:- module(parser, [parse/2]).

parse(Tokens, program(Stms)) :- phrase(stm_list_until(Stms, eof), Tokens), !.

stm(assignment(ident(I), E)) --> [let, ident(I), assign], eq_expr(E), ([semicolon]; []).
stm(if(C, block(S1), block(S2))) --> [if, lparen], eq_expr(C), [rparen], block_stms(S1), ([else], block_stms(S2); {S2 = []}).
stm(return(E)) --> [return], eq_expr(E), ([semicolon]; []).
stm(E) --> eq_expr(E), ([semicolon]; []).

eq_expr(E) --> comp_expr(P),
    ({E = op(eq, P, R)}, [eq], eq_expr(R)
    ;{E = op(neq, P, R)}, [neq], eq_expr(R)
    ;{E = P}).

comp_expr(E) --> sum_expr(P),
    ({E = op(lt, P, R)}, [lt], comp_expr(R)
    ;{E = op(gt, P, R)}, [gt], comp_expr(R)
    ;{E = P}).

sum_expr(E) --> prod_expr(P),
    ({E = op(add, P, R)}, [plus], sum_expr(R)
    ;{E = op(sub, P, R)}, [dash], sum_expr(R)
    ;{E = P}).

prod_expr(E) --> call_expr(P),
    ({E = op(mult, P, R)}, [asterisk], prod_expr(R)
    ;{E = op(div, P, R)}, [fslash], prod_expr(R)
    ;{E = P}).

call_expr(E) --> prefix_expr(P), 
    ({E = call(P, A)}, [lparen], expr_list_until(A, rparen)
    ;{E = index(P, I)}, [lbracket] , eq_expr(I), [rbracket]
    ;{E = P}).

prefix_expr(op(not, E)) --> [bang], prefix_expr(E).
prefix_expr(op(neg, E)) --> [dash], prefix_expr(E).
prefix_expr(E) --> atomic_expr(E).

atomic_expr(ident(I)) --> [ident(I)].
atomic_expr(bool(true)) --> [true].
atomic_expr(bool(false)) --> [false].
atomic_expr(int(V)) --> [int(V)].
atomic_expr(string(S)) --> [string(S)].
atomic_expr(list(L)) --> [lbracket], expr_list_until(L, rbracket).
atomic_expr(hash(L)) --> [lsquirly], kv_list_until(L, rsquirly).
atomic_expr(function(P, block(S))) --> [function, lparen], ident_list_until(P, rparen), block_stms(S).
atomic_expr(E) --> [lparen], eq_expr(E), [rparen].

block_stms(S) --> [lsquirly], stm_list_until(S, rsquirly).

stm_list_until([], End) --> [End].
stm_list_until([S|Ss], End) --> stm(S), stm_list_until(Ss, End).

ident_list_until([], End) --> [End].
ident_list_until([ident(I)|Is], End) --> [ident(I)], ([comma]; []), ident_list_until(Is, End).

expr_list_until([], End) --> [End].
expr_list_until([E|Es], End) --> eq_expr(E), ([comma]; []), expr_list_until(Es, End).

kv_list_until([], End) --> [End].
kv_list_until([K, V|Ts], End) --> eq_expr(K), [colon], eq_expr(V), ([comma]; []), kv_list_until(Ts, End).
