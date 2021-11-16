:- module(postorder, [postOrder/2]).
:- [opers].

%%%% POST-ORDER %%%%%

postOrder(Expr, Output) :- 
    postOrder(Expr, [], Output).

%%% 'Atomic-values' handler %%%%
postOrder(Expr, Acc, [Expr | Acc]) :-
    atom(Expr);number(Expr), !.

%%% IN-FIX Operators handler %%%%    
postOrder(Expr, Acc, PostLR) :-
    Expr=..[Oper, Left, Right],
    infix(Oper), !,
    postOrder(Right, [Oper | Acc], PostRight), 
    postOrder(Left, PostRight, PostLR).

%%% POST-FIX Operators handler %%%%
postOrder(Expr, Acc, PostLeft) :-
    Expr=..[Oper, Left],
    postOrder(Left, [Oper | Acc], PostLeft).
