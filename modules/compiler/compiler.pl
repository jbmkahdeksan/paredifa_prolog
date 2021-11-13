:- module(compiler, [begin_compile/2, postOrder/2]).
:- use_module(fa, [json_to_fa/2, fa_to_json/2, state_new_id/1, fa_new_id/1]).
:- [opers].

/*
   FAjson = fa{
        id:fa01, 
        vocabulary:[0,1], 
        states:[s0, s1, s2, s3], 
        initial:s0, finals:[s2, s3],
        moves:[s0/0==>s1]
    }
*/

begin_compile(Input, FA) :-
    postOrder(Input, Post),
    maplist(to_fa, Post, [FA]).


to_fa(Token, FA) :-
    atom(Token), !,
    fa_atomic(Token, FA).

to_fa(Oper, Oper). 


fa_atomic(Atomic, FA) :-
    fa_new_id(Id),
    state_new_id(S0),
    state_new_id(S1),
    format(atom(Move), '~w/~w==>~w',[S0,Atomic,S1]),
    FA = fa{
        id:Id, 
        vocabulary:[Atomic], 
        states:[S0, S1], 
        initial:S0, finals:[S1],
        moves:[Move]
    }

. 

% %%%% ^ handler %%%%%
% fa_concat(One, Two, FA).

% %%%% | handler %%%%%
% fa_union(One, Two, FA).

% %%%% +,*,? handler %%%%%
% fa_plus(FA, Plusified).
% fa_star(FA, Stared).
% fa_hook(FA, Hooked).



%%%% POST-ORDER %%%%%

postOrder(Expr, Output) :- 
    postOrder(Expr, [], Post), 
    flatten(Post, Output).

%%% 'Atomic-values' handler %%%%
postOrder(Expr, Acc, [Acc | Expr]) :-
    atom(Expr);number(Expr), !.

%%% IN-FIX Operators handler %%%%    
postOrder(Expr, Acc, [PostLR | Oper]) :-
    Expr=..[Oper, Left, Right],
    infix(Oper), !,
    postOrder(Left, Acc, PostLeft), 
    postOrder(Right, PostLeft, PostLR).

%%% POST-FIX Operators handler %%%%
postOrder(Expr, Acc, [PostLeft | Oper]) :-
    Expr=..[Oper, Left],
    postOrder(Left, Acc, PostLeft).






