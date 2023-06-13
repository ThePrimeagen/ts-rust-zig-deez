:- module(evaluator, [evaluate/2]).
:- dynamic bindings/3.

evaluate(program(Stms), Value) :-
    evaluate_program_stms(Stms, env(program, nil), Value).

evaluate(block(Stms), Env, Value) :-
    evaluate_block_stms(Stms, Env, Value).

evaluate(assignment(ident(Key), Exp), env(Inner, Outer), nil) :-
    evaluate(Exp, env(Inner, Outer), Value),
    assert_unique(Inner, Key, Value).

evaluate(return(Exp), Env, Value) :-
    evaluate(Exp, Env, Value).

evaluate(if(Cond, Left, _), Env, Value) :-
    evaluate(Cond, Env, CValue),
    \+ is_false(CValue),
    evaluate(Left, Env, Value).

evaluate(if(Cond, _, Right), Env, Value) :-
    evaluate(Cond, Env, CValue),
    is_false(CValue),
    evaluate(Right, Env, Value).

evaluate(function(Params, Body), env(Inner, _), function(Params, Body, Inner)).

evaluate(call(ident(Key), Args), env(Inner, Outer), Value) :-
    (bindings(Inner, Key, F); bindings(Outer, Key, F)),
    evaluate(call(F, Args), env(Inner, Outer), Value).

evaluate(call(function(Params, Body, FOuter), Args), env(Inner, Outer), Value) :-
    evaluate_function_args(Args, env(Inner, Outer), AValues),
    term_hash(function(AValues, Body), FInner),
    assert_all(FInner, Params, AValues),
    evaluate(Body, env(FInner, FOuter), Value).

evaluate(op(not, Atom), Env, Value) :-
    evaluate(Atom, Env, AValue),
    do_not(AValue, Value).

evaluate(op(neg, Atom), Env, Value) :-
    evaluate(Atom, Env, AValue),
    do_neg(AValue, Value).

% parse tree is right-associative, op_reassoc makes infix operations left-associative
evaluate(op(Op, Left, Right), Env, Value) :-
    op_reassoc(op(Op, Left, Right), LOp),
    evaluate_op(LOp, Env, Value).

evaluate(int(X), _, X).
evaluate(bool(X), _, X).
evaluate(ident(Key), env(Inner, Outer), Value) :- bindings(Inner, Key, Value); bindings(Outer, Key, Value).

evaluate_program_stms([], _, nil).
evaluate_program_stms([S], Env, Value) :- evaluate(S, Env, Value).
evaluate_program_stms([return(Expr)|_], Env, Value) :- evaluate(Expr, Env, Value).
evaluate_program_stms([S|Ss], Env, Value) :- evaluate(S, Env, _), evaluate_program_stms(Ss, Env, Value).

evaluate_block_stms([], _, nil).
evaluate_block_stms([S|_], Env, Value) :- evaluate(S, Env, Value), Value \= nil.
evaluate_block_stms([S|Ss], Env, Value) :- evaluate(S, Env, nil), evaluate_block_stms(Ss, Env, Value).

evaluate_function_args([], _, []).
evaluate_function_args([A|As], Env, [V|Vs]) :- evaluate(A, Env, V), evaluate_function_args(As, Env, Vs).

op_reassoc(op(Op1, X, op(Op2, Y, Z)), op(Op2, op(Op1, X, Y), Z)) :- op_prec(Op1, P), op_prec(Op2, P), !.
op_reassoc(Op, Op).

evaluate_op(op(eq, Left, Right), Env, Value) :-
    evaluate(Left, Env, LValue),
    evaluate(Right, Env, RValue),
    is_eq(LValue, RValue, Value).

evaluate_op(op(neq, Left, Right), Env, Value) :-
    evaluate(Left, Env, LValue),
    evaluate(Right, Env, RValue),
    is_neq(LValue, RValue, Value).

evaluate_op(op(lt, Left, Right), Env, Value) :-
    evaluate(Left, Env, LValue),
    evaluate(Right, Env, RValue),
    is_lt(LValue, RValue, Value).

evaluate_op(op(gt, Left, Right), Env, Value) :-
    evaluate(Left, Env, LValue),
    evaluate(Right, Env, RValue),
    is_gt(LValue, RValue, Value).

evaluate_op(op(add, Left, Right), Env, Value) :-
    evaluate(Left, Env, LValue),
    evaluate(Right, Env, RValue),
    Value is LValue + RValue.

evaluate_op(op(sub, Left, Right), Env, Value) :-
    evaluate(Left, Env, LValue),
    evaluate(Right, Env, RValue),
    Value is LValue - RValue.

evaluate_op(op(div, Left, Right), Env, Value) :-
    evaluate(Left, Env, LValue),
    evaluate(Right, Env, RValue),
    Value is LValue / RValue.

evaluate_op(op(mult, Left, Right), Env, Value) :-
    evaluate(Left, Env, LValue),
    evaluate(Right, Env, RValue),
    Value is LValue * RValue.

assert_unique(Env, Key, Value) :-
    bindings(Env, Key, PValue),
    retract(bindings(Env, Key, PValue)),
    assert(bindings(Env, Key, Value)), !.

assert_unique(Env, Key, Value) :-
    assert((bindings(Env, Key, Value))).

assert_all(_, [], []).
assert_all(Env, [ident(K)|Ks], [V|Vs]) :-
    assert_unique(Env, K, V),
    assert_all(Env, Ks, Vs).

do_not(A, true) :- is_false(A).
do_not(A, false) :- \+ is_false(A).

is_false(A) :- A = false; A = nil.

do_neg(A, NA) :- integer(A), NA is -A.
do_neg(A, nil) :- \+ integer(A).

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

op_prec(eq, 0).
op_prec(neq, 0).
op_prec(lt, 1).
op_prec(gt, 1).
op_prec(add, 2).
op_prec(sub, 2).
op_prec(mult, 3).
op_prec(div, 3).


