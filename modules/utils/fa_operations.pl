/**
 * Description:
 * Utils for FA Operations used in compiler.
 * EIF400 -- Paradigmas de Programacion
 * @since II Term - 2021
 * @authors Team 01-10am
 *  - Andres Alvarez Duran     117520958
 *  - Joaquin Barrientos Monge 117440348
 *  - Oscar Ortiz Chavarria    208260347
 *  - David Zarate Marin       116770797
 **/

:- module(fa_operations, [new_postInitial_moves/4,
                          subtract_initial_state/3,
                          find_loops/2, format_move/4]).


is_move_postInitial(M, B) :-
    atom_to_term(M, X/_ ==> _, _), 
    member(X, [B.initial])
.  


new_postInitial_moves(concat, A, B, Moves) :-
    %finds all PostInitial moves in B
    include([M] >> is_move_postInitial(M, B), B.moves, FromInitial),

    %substract those moves from B
    subtract(B.moves, FromInitial, MovesB), 

    %finds the destination of those moves and stores it in PostStates
    maplist([M, Z] >> (atom_to_term(M, _/_==>Z, _)), 
                            FromInitial, PostStates),

    %creates moves from every final state in A 
    %to every PostState with the symbols from B.vocab
    findall(Move, (member(X, A.finals), 
                    member(Z, PostStates),                    
                    member(Y, B.vocabulary),
                    format_move(X, Y, Z, Move)), 
                    MovesA),

    %appends
    append(A.moves, MovesA, Connect), 
    append(Connect, MovesB, Moves)        
. 

new_postInitial_moves(union, A, B, Moves) :-
    %finds all PostInitial moves in B
    include([M] >> is_move_postInitial(M, B), B.moves, FromInitial),

    maplist([M, Out] >> (atom_to_term(M, _/Y ==> Z, _), 
                            format_move(A.initial, Y, Z, Out)),
                            FromInitial, MovesB),

    append(A.moves, MovesB, Moves)        
. 


subtract_initial_state(A, B, States) :-
    ord_subtract(B.states, [B.initial], NoInitial),
    append(A.states, NoInitial, States)
. 

find_loops(A, Loops) :-
    %finds moves with initial origin state
    include([Move]>>(atom_to_term(Move, X/_==>_,_), 
                        member(X, [A.initial])), A.moves, Pivot),  

    %extracts the symbols from the moves obtained in the previous step
    maplist([Move, Y]>>(atom_to_term(Move, _/Y==>_,_)), Pivot, Ys), 
    %extracts the destination states from the moves obtained
    maplist([Move, Z]>>(atom_to_term(Move, _/_==>Z,_)), Pivot, Zs), 

    %creates new moves based on the lists obtained above
    findall(Loop, (member(Y, Ys), member(Z, Zs), 
                    member(Final, A.finals), 
                    format_move(Final, Y, Z, Loop)), Loops)
. 

format_move(X, Y, Z, Move) :-
    format(atom(Move), '~w/~w==>~w',[X, Y, Z])
. 