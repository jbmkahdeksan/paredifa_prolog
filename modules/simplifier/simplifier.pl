/**
 * Description:
 * RegEx simplifier module
 * EIF400 -- Paradigmas de Programacion
 * @since II Term - 2021
 * @authors Team 01-10am
 *  - Andres Alvarez Duran 117520958
 *  - Joaquin Barrientos Monge 117440348
 *  - Oscar Ortiz Chavarria 208260347
 *  - David Zarate Marin 116770797
 **/

:- module(simplifier, [begin_simplify/2]).


begin_simplify(Tree, Out) :- 
    % handles simplification with | operator scenarios
    simplify_union(Tree, P), 
    simplify_union(P, Out).  

begin_simplify(Tree, Out) :-
    % handles simplification with ^ operator scenarios
    simplify_concat(Tree, P),
    simplify_concat(P, Out)
.  

begin_simplify(Tree, Out) :-
    %handles simplification with postfix opertators scenarios
    simplify_kleene(Tree, P),
    simplify_kleene(P, Out)
.  

begin_simplify(A, A).

%%%%%%%%%%%% Kleene Simplifier %%%%%%%%%%%%%%
% ----> Hook
simplify_kleene(?(?(A)), T) :-
    begin_simplify(A, AS), begin_simplify(?(AS), T)
. 
    
simplify_kleene(?(+(A)), T) :-
    begin_simplify(A, AS), begin_simplify(*(AS), T)
. 

simplify_kleene(?(*(A)), T) :-
    begin_simplify(A, AS), begin_simplify(*(AS), T)
. 

simplify_kleene(?(A), ?(AS)) :-
    begin_simplify(A, AS)
. 

% ----> Plus
simplify_kleene(+(+(A)), Z) :-
    begin_simplify(A, AS), begin_simplify(+(AS), Z)
. 

simplify_kleene(+(?(A)), T) :-
    begin_simplify(A, AS), begin_simplify(*(AS), T)
. 

simplify_kleene(+(*(A)), T) :-
    begin_simplify(A, AS), begin_simplify(*(AS), T)
. 

simplify_kleene(+(A), +(AS)) :-
    begin_simplify(A, AS)
. 

% ----> Star
simplify_kleene(*(*(A)), Z) :-
    begin_simplify(A, AS), begin_simplify(*(AS), Z)
. 

simplify_kleene(*(+(A)), T) :-
    begin_simplify(A, AS), begin_simplify(*(AS), T)
. 

simplify_kleene(*(?(A)), T) :-
    begin_simplify(A, AS), begin_simplify(*(AS), T)
. 

simplify_kleene(*(A), *(AS)) :-
    begin_simplify(A, AS)
. 

%%%%%%%%%%%% Union Simplifier %%%%%%%%%%%%%%

simplify_union(?(A)|+(A), T) :- begin_simplify(*(A), T).
% ?A|+A != A+|A? 

simplify_union(A|*(A), T) :- begin_simplify(*(A), T).
simplify_union(*(A)|A, T) :- begin_simplify(*(A), T).

simplify_union(A|+(A), T) :- begin_simplify(*(A), T).
simplify_union(+(A)|A, T) :- begin_simplify(*(A), T).

simplify_union(A|A, T) :- begin_simplify(A, T).

simplify_union(A|B, AS|BS) :-
   begin_simplify(A, AS), begin_simplify(B, BS)
. 

%%%%%%%%%%%% Concat Simplifier %%%%%%%%%%%%%%

simplify_concat(A^ *(A), T) :- begin_simplify(+(A), T).
simplify_concat(*(A)^ A, T) :- begin_simplify(*(A), T).
simplify_concat(*(A)^ *(A), T) :- begin_simplify(*(A), T).

simplify_concat(A^ +(A), Z) :- begin_simplify(+(A), Z).
simplify_concat(+(A)^ A, Z) :- begin_simplify(+(A), Z).
simplify_concat(+(A)^ +(A), Z) :- begin_simplify(+(A), Z).

simplify_concat(A^ *(B), T) :-
    A=..[^, L, R], R == B,
    begin_simplify(L^ *(B), T)  
. 

simplify_concat(A^ +(B), T) :- 
    A=..[^, L, R], R == B,
    begin_simplify(L^ +(B), T)  
. 

simplify_concat(*(A)^ B, T) :- 
    B=..[^, L, R], L == A,
    begin_simplify(*(A)^ R, T)  
. 

simplify_concat(+(A)^ B, T) :- 
    B=..[^, L, R], L == A,
    begin_simplify(+(A)^ R, T)  
. 

simplify_concat(A^ B, AS ^ BS) :-
    begin_simplify(A, AS), begin_simplify(B, BS)
. 