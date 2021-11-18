:- module(simplifier, [begin_simplify/2]).


begin_simplify(Tree, Out) :-
    simplify_union(Tree, Out1), 
    simplify_union(Out1, Out).  %handles A|A, A|B, A?|A+

begin_simplify(Tree, Out) :-
    simplify_kleene(Tree, Out).  %handles A**, A++, A??, A?+, A+?

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

simplify_union(A|A, Z) :- begin_simplify(A, Z).

simplify_union(A|B, AS|BS) :-
    begin_simplify(A, AS), begin_simplify(B, BS)
. 




