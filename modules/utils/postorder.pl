/**
 * Description:
 * Post-order Tree traversal utility
 * EIF400 -- Paradigmas de Programacion
 * @since II Term - 2021
 * @authors Team 01-10am
 *  - Andres Alvarez Duran     117520958
 *  - Joaquin Barrientos Monge 117440348
 *  - Oscar Ortiz Chavarria    208260347
 *  - David Zarate Marin       116770797
 **/

:- module(postorder, [postOrder/2]).

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
