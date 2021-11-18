:- module(simplifier, [begin_simplify/2]).


begin_simplify(?(?(A)), Z) :-
    begin_simplify(A, AS), begin_simplify(?(AS), Z)
. 

begin_simplify(+(+(A)), Z) :-
    begin_simplify(A, AS), begin_simplify(+(AS), Z)
. 

begin_simplify(*(*(A)), Z) :-
    begin_simplify(A, AS), begin_simplify(*(AS), Z)
. 

begin_simplify(A|A, Z) :- begin_simplify(A, Z).

begin_simplify(A|B,T) :-
    begin_simplify(A, AS), begin_simplify(AS, AZ),
    begin_simplify(B, BS), begin_simplify(BS, BZ), 
    begin_simplify(AZ|BZ, T)
. 


begin_simplify(A, A).




