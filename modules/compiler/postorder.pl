:- module(postorder, [postOrder/2]).
:- [opers].

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
