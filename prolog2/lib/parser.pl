:- module(parser, [parse/2]).

parse(Tokens, Ast) :- phrase(program(Ast), Tokens).

program(program(Stms)) --> program_stms(Stms).

stm(E) --> assignment_stm(E). 
stm(E) --> block_stm(E). 
stm(E) --> expr_stm(E). 
stm(E) --> return_stm(E). 

assignment_stm(assignment(ident(I), E)) --> [let, ident(I), assign], expr(E), ([semicolon]; []).
return_stm(return(E)) --> [return], expr(E), ([semicolon]; []).
block_stm(block(S)) --> [lsquirly], block_stms(S).
expr_stm(E) --> expr(E), ([semicolon]; []).

expr(E) --> if_expr(E). 
expr(E) --> fn_expr(E). 
expr(E) --> eq_expr(E).

if_expr(if(C, B1, B2)) --> [if, lparen], expr(C), [rparen], block_stm(B1), [else], block_stm(B2).
if_expr(if(C, B1, block([]))) --> [if, lparen], expr(C), [rparen], block_stm(B1).

fn_expr(function(P, B)) --> [function, lparen], fn_params(P), block_stm(B).

eq_expr(E) --> comp_expr(E).
eq_expr(op(eq, L, R)) --> comp_expr(L), [eq], eq_expr(R).
eq_expr(op(neq, L, R)) --> comp_expr(L), [neq], eq_expr(R).

comp_expr(E) --> sum_expr(E).
comp_expr(op(gt, L, R)) --> sum_expr(L), [gt], comp_expr(R).
comp_expr(op(lt, L, R)) --> sum_expr(L), [lt], comp_expr(R).

sum_expr(op(sub, L, R)) --> prod_expr(L), [dash], sum_expr(R). % try infix dash first, this makes it slower
sum_expr(E) --> prod_expr(E).
sum_expr(op(add, L, R)) --> prod_expr(L), [plus], sum_expr(R).

prod_expr(E) --> prefix_expr(E).
prod_expr(op(mult, L, R)) --> prefix_expr(L), [asterisk], prod_expr(R).
prod_expr(op(div, L, R)) --> prefix_expr(L), [fslash], prod_expr(R).

prefix_expr(E) --> atomic_expr(E).
prefix_expr(op(not, E)) --> [bang], prefix_expr(E).
prefix_expr(op(neg, E)) --> [dash], prefix_expr(E).

atomic_expr(E) --> call_expr(E), !.
atomic_expr(int(V)) --> [int(V)].
atomic_expr(bool(true)) --> [true].
atomic_expr(bool(false)) --> [false].
atomic_expr(ident(I)) --> [ident(I)].

call_expr(call(ident(F), A)) --> [ident(F), lparen], call_args(A).
call_expr(call(F, A)) --> fn_expr(F), [lparen], call_args(A).
call_expr(E) --> [lparen], eq_expr(E), [rparen].


program_stms([]) --> [eof].
program_stms([S|Ss]) --> stm(S), program_stms(Ss).

block_stms([]) --> [rsquirly].
block_stms([S|Ss]) --> stm(S), block_stms(Ss).

fn_params([]) --> [rparen].
fn_params([ident(P)|Ps]) --> [ident(P)], ([comma]; []), fn_params(Ps).

call_args([]) --> [rparen].
call_args([A|As]) --> expr(A), ([comma]; []),  call_args(As).
