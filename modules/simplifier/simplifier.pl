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
    simplify_concat(P, Out).  

begin_simplify(Tree, Out) :-
    %handles simplification with postfix opertators scenarios
    simplify_kleene(Tree, Out).  

begin_simplify(A, A).

%%%%%%%%%%%% Kleene Simplifier %%%%%%%%%%%%%%
% ----> Hook
simplify_kleene(?(?(A)), Z) :-
    begin_simplify(A, AS), begin_simplify(?(AS), Z)
    . 
    
simplify_kleene(?(+(A)), T) :-
    begin_simplify(A, AS), begin_simplify(*(AS), T)
. 

simplify_kleene(?(*(A)), T) :-
    begin_simplify(A, AS), begin_simplify(*(AS), T)
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


%%%%%%%%%%%% Union Simplifier %%%%%%%%%%%%%%

simplify_union(?(A)|+(A), T) :- begin_simplify(*(A), T).
% ?A|+A != A+|A?

simplify_union(A|*(A), T) :- begin_simplify(*(A), T).
simplify_union(*(A)|A, T) :- begin_simplify(*(A), T).

simplify_union(A|+(A), T) :- begin_simplify(*(A), T).
simplify_union(+(A)|A, T) :- begin_simplify(*(A), T).

simplify_union(A|A, Z) :- begin_simplify(A, Z).

simplify_union(A|B, AS|BS) :-
    begin_simplify(A, AS), begin_simplify(B, BS)
. 

%%%%%%%%%%%% Concat Simplifier %%%%%%%%%%%%%%

simplify_concat(A^ *(A), T) :- begin_simplify(*(A), T).
simplify_concat(*(A)^A, T) :- begin_simplify(*(A), T).
simplify_concat(*(A)^ *(A), T) :- begin_simplify(*(A), T).

simplify_concat(A^ +A, Z) :- begin_simplify(+(A), Z).
simplify_concat(+A^A, Z) :- begin_simplify(+(A), Z).
simplify_concat(+A^ +A, Z) :- begin_simplify(+(A), Z).


simplify_concat(A^B, AS^BS) :-
    begin_simplify(A, AS), begin_simplify(B, BS)
. 

/*
* Important Disclosure:
* In order to simplify expression such as a^a^a*^a, is necessary the use of 
* some sort of memory, like a stack. Due to limitations of time, we couldn't 
* cover this scenario in this sprint. Applying this will make a hug difference
* that's why is something we keep in mind.
*/


