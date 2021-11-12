:- [opers].

/*
A model for FA using predicates
*/

:- dynamic dyn_fa_initial/2.
:- dynamic dyn_fa_final/2.
:- dynamic dyn_fa_moves/4.
:- dynamic dyn_fa_states/2.
:- dynamic dyn_fa_symbols/2.

%%%%%%%%%%%%%%%%%%%%% CONSTRUCTOR %%%%%%%%%%%%%%%%%%%%%%%%%
fa_new_id(Id) :- gensym('$fa_', Id).

%%%%%%%%%%%%%%%%%%%%% STATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fa_set_states(FA, S) :-
    fa_insert_state_once(FA, S).

fa_insert_state_once(FA, S) :- 
    dyn_fa_states(FA, S), !.

fa_insert_state_once(FA, S) :-
    assert(dyn_fa_states(FA, S)).

%%%%%%%%%%%%%%%%%%%%%% INICIALES %%%%%%%%%%%%%%%%%%%%%%%%
fa_set_initial(FA, S0) :- 
    retractall(dyn_fa_initial(FA, _)),
    assert(dyn_fa_initial(FA, S0)).

%%%%%%%%%%%%%%%%%%%%%% FINALES %%%%%%%%%%%%%%%%%%%%%%%
fa_set_finals(FA, Sf) :-
    dyn_fa_final(FA, Sf), !.

fa_set_finals(FA, Sf) :-
    assert(dyn_fa_final(FA, Sf)).

%%%%%%%%%%%%%%%%%%%%%% MOVES %%%%%%%%%%%%%%%%%%%%%%%%%
fa_set_moves(FA, X/S ==> Y) :- 
    fa_insert_move_once(FA, X, S, Y).

fa_insert_move_once(FA, X, S, Y) :-
    dyn_fa_moves(FA, X, S, Y), !.

fa_insert_move_once(FA, X, S, Y) :-
    assert(dyn_fa_moves(FA, X, S, Y)).

fa_move_to_term(FA, X/S ==> Y) :-
    dyn_fa_moves(FA, X, S, Y).

%%%%%%%%%%%%%%%%%%%%%% VOCAB %%%%%%%%%%%%%%%%%%%%%%%%%
fa_set_vocab(FA, Symb) :-
    fa_insert_symb_once(FA, Symb).

fa_insert_symb_once(FA, Symb) :-
    dyn_fa_symbols(FA, Symb), !.

fa_insert_symb_once(FA, Symb) :-
    assert(dyn_fa_symbols(FA, Symb)).

%%%%%%%%%%%%%%%%%%%%% GETTERS %%%%%%%%%%%%%%%%%%%%%%%%%%%
fa_initial(FA, S0) :- 
    dyn_fa_initial(FA, S0).


fa_finals(FA, L)  :- 
    findall(F, dyn_fa_final(FA, F), L).

fa_states(FA, L) :-
    findall(S, dyn_fa_states(FA, S), L).

%%%%%%%%%%%%%%%%%%% TRANSFORMERS %%%%%%%%%%%%%%%%%%%%%%%%

json_to_fa(JsonDict, FA) :- 
        _{ 
            vocabulary: Vocab, 
            states: States, 
            initial: S0, 
            finals: Finals,
            moves: Moves
        } :< JsonDict,
        writeln('*** Testing fa_from_json ***'),
        fa_new_id(FA),
        fa_set_initial(FA, S0),
        forall(member(S, States), fa_set_states(FA, S)),
        forall(member(F, Finals), fa_set_finals(FA, F)),
        forall(member(M, Moves), fa_set_moves(FA, M)),   
        forall(member(V, Vocab), fa_set_vocab(FA, V))     
. 

fa_to_json(FA, JSON) :-
    
. 