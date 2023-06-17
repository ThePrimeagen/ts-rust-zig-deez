:- module(evaluator, [evaluate/2]).
:- dynamic bindings/3.

evaluate(program(Stms), Value) :-
    evaluate_program_stms(Stms, env(program, nil), Tmp),
    make_printable(Tmp, Value).

evaluate(block(Stms), Env, Value) :-
    evaluate_block_stms(Stms, Env, Value).

evaluate(assignment(ident(Key), Exp), env(Inner, Outer), nil) :-
    evaluate(Exp, env(Inner, Outer), Value),
    assert_unique(Inner, Key, Value).

evaluate(return(Exp), Env, Value) :-
    evaluate(Exp, Env, Value).

evaluate(if(Cond, Left, Right), Env, Value) :-
    evaluate(Cond, Env, CValue),
    (\+ is_false(CValue) ->
        evaluate(Left, Env, Value);
        evaluate(Right, Env, Value)).

evaluate(call(F, Args), Env, Value) :-
    evaluate(F, Env, FValue),
    evaluate_expr_list(Args, Env, AValues),
    evaluate(call(FValue, AValues), Env, Value).

evaluate(call(len, [Str]), _, Value) :- string(Str), string_length(Str, Value).
evaluate(call(len, [[list|L]]), _, Value) :- length(L, Value).
evaluate(call(len, [[hash|L]]), _, Value) :- length(L, Tmp), Value is Tmp / 2.
evaluate(call(len, _), _, nil).
evaluate(call(first, [[list,H|_]]), _, H).
evaluate(call(first, _), _, nil).
evaluate(call(rest, [[list,_|Ts]]), _, [list|Ts]).
evaluate(call(rest, _), _, nil).
evaluate(call(push, [[list|L], E]), _, L2) :- append([list|L], [E], L2).
evaluate(call(push, [[hash|H], K, V]), _, H2) :- append([hash|H], [K, V], H2).
evaluate(call(push, _), _, nil).
evaluate(call(puts, L), _, nil) :- write_list(L).

evaluate(call(function(Params, Body, FOuter), Args), _, Value) :-
    term_hash(function(Args, Body), FInner),
    asset_unique_all(FInner, Params, Args),
    evaluate(Body, env(FInner, FOuter), Value).

evaluate(index(L, Index), Env, Value) :-
    evaluate(L, Env, LValue),
    evaluate(Index, Env, IValue),
    evaluate(index(LValue, IValue), Env, Value).

evaluate(index([list|L], Index), _, Value) :- nth0(Index, L, Value).
evaluate(index([hash|H], Key), _, Value) :- append(_, [Key, Value|_], H).
evaluate(index(_, _), _, nil).

evaluate(op(not, Left), Env, Value) :-
    evaluate(Left, Env, LValue),
    do_not(LValue, Value).

evaluate(op(neg, Left), Env, Value) :-
    evaluate(Left, Env, LValue), 
    do_neg(LValue, Value).

evaluate(op(ROp, RLeft, RRight), Env, Value) :-
    op_reassoc(op(ROp, RLeft, RRight), op(LOp, LLeft, LRight)),
    evaluate(LLeft, Env, LValue),
    evaluate(LRight, Env, RValue),
    do_op(LOp, LValue, RValue, Value).

evaluate(int(X), _, X).
evaluate(bool(X), _, X).
evaluate(string(X), _, X).
evaluate(list(X), Env, [list|E]) :- evaluate_expr_list(X, Env, E).
evaluate(hash(X), Env, [hash|E]) :- evaluate_expr_list(X, Env, E).
evaluate(ident(Key), env(Inner, Outer), Value) :- bindings(Inner, Key, Value); bindings(Outer, Key, Value).
evaluate(ident(len), _, len).
evaluate(ident(first), _, first).
evaluate(ident(rest), _, rest).
evaluate(ident(push), _, push).
evaluate(ident(puts), _, puts).
evaluate(function(Params, Body), env(Inner, _), function(Params, Body, Inner)).

evaluate_program_stms([], _, nil).
evaluate_program_stms([S], Env, Value) :- evaluate(S, Env, Value), !.
evaluate_program_stms([return(Expr)|_], Env, Value) :- evaluate(Expr, Env, Value), !.
evaluate_program_stms([S|Ss], Env, Value) :- evaluate(S, Env, _), evaluate_program_stms(Ss, Env, Value).

evaluate_block_stms([], _, nil).
evaluate_block_stms([S|_], Env, Value) :- evaluate(S, Env, Value), Value \= nil, !.
evaluate_block_stms([_|Ss], Env, Value) :-  evaluate_block_stms(Ss, Env, Value).

evaluate_expr_list([], _, []).
evaluate_expr_list([A|As], Env, [V|Vs]) :- evaluate(A, Env, V), evaluate_expr_list(As, Env, Vs).

assert_unique(Env, Key, Value) :-
    bindings(Env, Key, PValue),
    retract(bindings(Env, Key, PValue)),
    assert(bindings(Env, Key, Value)), !.

assert_unique(Env, Key, Value) :-
    assert((bindings(Env, Key, Value))).

asset_unique_all(_, [], []).
asset_unique_all(Env, [ident(K)|Ks], [V|Vs]) :-
    assert_unique(Env, K, V),
    asset_unique_all(Env, Ks, Vs).

write_list([]).
write_list([H|T]) :- 
    make_printable(H, Tmp), 
    writenl(Tmp), nl, 
    write_list(T).

make_printable([list|L], L) :- !.
make_printable([hash|H], H) :- !.
make_printable(X, X).

op_reassoc(op(Op1, X, op(Op2, Y, Z)), op(Op2, op(Op1, X, Y), Z)) :- op_prec(Op1, P), op_prec(Op2, P), !.
op_reassoc(Op, Op).

op_prec(eq, 0).
op_prec(neq, 0).
op_prec(lt, 1).
op_prec(gt, 1).
op_prec(add, 2).
op_prec(sub, 2).
op_prec(mult, 3).
op_prec(div, 3).

do_not(A, true) :- is_false(A).
do_not(A, false) :- \+ is_false(A).
do_neg(A, NA) :- integer(A), NA is -A.
do_neg(A, nil) :- \+ integer(A).
do_op(eq, Left, Right, Value) :- is_eq(Left, Right, Value).
do_op(neq, Left, Right, Value) :- is_neq(Left, Right, Value).
do_op(lt, Left, Right, Value) :- is_lt(Left, Right, Value).
do_op(gt, Left, Right, Value) :- is_gt(Left, Right, Value).
do_op(add, Left, Right, Value) :- string(Left), string(Right), string_concat(Left, Right, Value).
do_op(add, Left, Right, Value) :- number(Left), number(Right), Value is Left + Right.
do_op(sub, Left, Right, Value) :- Value is Left - Right.
do_op(div, Left, Right, Value) :- Value is Left / Right.
do_op(mult, Left, Right, Value) :- Value is Left * Right.

is_false(A) :- A = false; A = nil.
is_eq(A, A, true) :- !.
is_eq(_, _, false).
is_neq(A, A, false) :- !.
is_neq(_, _, true).
is_lt(A, B, true) :- are_integers(A, B), A < B.
is_lt(A, B, false) :- are_integers(A, B), A >= B.
is_lt(A, B, nil) :- \+ are_integers(A, B).
is_gt(A, B, true) :- are_integers(A, B), A > B.
is_gt(A, B, false) :- are_integers(A, B), A =< B.
is_gt(A, B, nil) :- \+ are_integers(A, B).
are_integers(A, B) :- integer(A), integer(B).
