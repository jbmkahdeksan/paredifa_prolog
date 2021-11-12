:- module(compiler, [begin_compile/2, postOrder/2]).
:- use_module(fa, [json_to_fa/2, fa_to_json/2]).
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
    postOrder(Input, FA).


fa_atomic().

%%%% ^ handler %%%%%
fa_concat().

%%%% | handler %%%%%
fa_union().

%%%% +,*,? handler %%%%%
fa_kleene().



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






