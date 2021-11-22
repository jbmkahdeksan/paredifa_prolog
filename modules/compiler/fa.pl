/**
 * Description:
 * Object orietation module for an automata
 * EIF400 -- Paradigmas de Programacion
 * @since II Term - 2021
 * @authors Team 01-10am
 *  - Andres Alvarez Duran 117520958
 *  - Joaquin Barrientos Monge 117440348
 *  - Oscar Ortiz Chavarria 208260347
 *  - David Zarate Marin 116770797
 **/

:- module(fa, [json_to_fa/2, fa_to_json/2, 
               fa_set_vocab/2, normalize_json/2,
               fa_new_id/1, state_new_id/1,
               fa_initial/2, fa_finals/2, 
               fa_states/2, fa_moves/2, 
               fa_vocab/2, search_move/4, 
               fa_set_states/2, fa_set_moves/2,
               fa_insert_move_once/4, 
               fa_set_finals/2, fa_set_initial/2]).
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
state_new_id(Id) :- gensym('s', Id).

fa_set_states(FA, S) :-
    fa_insert_state_once(FA, S).

fa_insert_state_once(FA, S) :- 
    dyn_fa_states(FA, S), !.

fa_insert_state_once(FA, S) :-
    assert(dyn_fa_states(FA, S)).

%%%%%%%%%%%%%%%%%%%%%% INITIALS %%%%%%%%%%%%%%%%%%%%%%%%
fa_set_initial(FA, S0) :- 
    retractall(dyn_fa_initial(FA, _)),
    assert(dyn_fa_initial(FA, S0)).

%%%%%%%%%%%%%%%%%%%%%% FINALS %%%%%%%%%%%%%%%%%%%%%%%
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
fa_set_vocab(FA, L) :-
    forall(member(S, L), fa_set_symbol(FA, S)).

fa_set_symbol(FA, Symb) :-
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

fa_vocab(FA, L) :-
    findall(S, dyn_fa_symbols(FA, S), L).

fa_moves(FA, L) :-
    findall(X/S==>Y, dyn_fa_moves(FA, X, S, Y), L).

search_move(FA, X, Y, Z) :-
    fa_moves(FA, L),
    member(X/Y==>Z, L).


%%%%%%%%%%%%%%%%%%% TRANSFORMERS %%%%%%%%%%%%%%%%%%%%%%%%

json_to_fa(JsonDict, FA) :- 
    _{ 
        vocabulary: Vocab, 
        states: States, 
        initial: S0, 
        finals: Finals, 
        moves: Moves
    } :< JsonDict,
    fa_new_id(FA),
    fa_set_vocab(FA, Vocab),
    fa_set_initial(FA, S0),
    forall(member(S, States), fa_set_states(FA, S)),
    forall(member(F, Finals), fa_set_finals(FA, F)),
    forall(member(M, Moves), (atom_to_term(M, T, _), fa_set_moves(FA, T)))
. 

fa_to_json(FA, JSON) :-
    fa_initial(FA, S0),
    fa_finals(FA, Finals),
    fa_states(FA, States),
    fa_vocab(FA, Vocab),
    fa_moves(FA, Moves),
    maplist([M, Out] >> format(atom(Out),'~w',[M]), Moves, Edges),
    maplist([State, StateAtom] >> format(atom(StateAtom),'~w', [State]), States, DFAstates),
    maplist([Final, FinalAtom] >> format(atom(FinalAtom),'~w', [Final]), Finals, DFAfinals),
    JSON = json{
        id:FA,
        vocabulary: Vocab, 
        states: DFAstates, 
        initial: S0, 
        finals: DFAfinals,
        moves: Edges
    }
. 

normalize_json(JsonDict, Norm) :- 
    % when numbers are part of the vocabulary, Prolog treats them as numbers
    % but the program needs them to be atoms.
    _{ 
        vocabulary: Vocab, 
        states: States, 
        initial: S0, 
        finals: Finals, 
        moves: Moves
    } :< JsonDict,
    fa_new_id(FA),
    maplist([Symbol, Sym]>> atom_to_term(Symbol, Sym, _), Vocab, V), include(number, V, Nums),
    subtract(V, Nums, Alphabet), maplist([In, At]>>atom_number(At, In), Nums, AL), union(Alphabet, AL, AV),
    fa_set_vocab(FA, AV),
    atom_to_term(S0, I, _), fa_set_initial(FA, I),
    forall(member(State, States), (atom_to_term(State, S, _), fa_set_states(FA, S))),
    forall(member(Final, Finals), (atom_to_term(Final, F, _), fa_set_finals(FA, F))),
    forall(member(Move, Moves),   (atom_to_term(Move, M, _), fa_set_moves(FA, M))),

    fa_to_json(FA, Norm)
. 