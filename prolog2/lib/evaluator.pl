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
    CValue \= false, CValue \= nil ->
        evaluate(Left, Env, Value);
        evaluate(Right, Env, Value).

evaluate(op(Op, Left), Env, Value) :-
    evaluate(Left, Env, LValue),
    do_op(Op, LValue, Value).

evaluate(op(ROp, RLeft, RRight), Env, Value) :-
    reassociate(op(ROp, RLeft, RRight), op(LOp, LLeft, LRight)),
    evaluate(LLeft, Env, LValue),
    evaluate(LRight, Env, RValue),
    do_op(LOp, LValue, RValue, Value).

evaluate(index(L, Index), Env, Value) :-
    evaluate(L, Env, LValue),
    evaluate(Index, Env, IValue),
    evaluate(index(LValue, IValue), Env, Value).

evaluate(index([list|L], Index), _, Value) :- nth0(Index, L, Value).
evaluate(index([hash|H], Key), _, Value) :- append(_, [Key, Value|_], H).
evaluate(index(_, _), _, nil).

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
evaluate(call(puts, []), _, nil).
evaluate(call(puts, [H|T]), _, nil) :- make_printable(H, Tmp), writeln(Tmp), evaluate(call(puts, T), _, nil).

evaluate(call(function(Params, Body, FOuter), Args), _, Value) :-
    term_hash(function(Args, Body), FInner),
    asset_unique_all(FInner, Params, Args),
    evaluate(Body, env(FInner, FOuter), Value).

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

make_printable([list|L], L) :- !.
make_printable([hash|H], H) :- !.
make_printable(X, X).

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


reassociate(op(Op1, X, op(Op2, Y, Z)), op(Op2, op(Op1, X, Y), Z)) :- op_prec(Op1, P), op_prec(Op2, P), !.
reassociate(Op, Op).

op_prec(eq, 0).
op_prec(neq, 0).
op_prec(lt, 1).
op_prec(gt, 1).
op_prec(add, 2).
op_prec(sub, 2).
op_prec(mult, 3).
op_prec(div, 3).

do_op(not, L, true) :- L = false; L = nil.
do_op(not, L, false) :- L \= false, L \= nil.
do_op(neg, L, V) :- number(L), V is -L.
do_op(eq, L, R, true) :- L = R.
do_op(eq, L, R, false) :- L \= R.
do_op(neq, L, R, true) :- L \= R.
do_op(neq, L, R, false) :- L = R.
do_op(lt, L, R, true) :- numbers(L, R), L < R.
do_op(lt, L, R, false) :- numbers(L, R), L >= R.
do_op(gt, L, R, true) :- numbers(L, R), L > R.
do_op(gt, L, R, false) :- numbers(L, R), L =< R.
do_op(add, L, R, V) :- strings(L, R), string_concat(L, R, V).
do_op(add, L, R, V) :- numbers(L, R), V is L + R.
do_op(sub, L, R, V) :- numbers(L, R), V is L - R.
do_op(div, L, R, V) :- numbers(L, R), V is L / R.
do_op(mult, L, R, V) :- numbers(L, R), V is L * R.

numbers(A, B) :- number(A), number(B).
strings(A, B) :- string(A), string(B).
