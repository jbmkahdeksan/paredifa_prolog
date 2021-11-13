/**
 * EIF400 - II Ciclo 2021.
 * Joaquin Barrientos   
**/

:- module(stack, [
    new_stack/1, is_empty/1, 
    top_stack/2, push_stack/2,
    size/2, pop_stack/2
]).

:- dynamic stack/3.

new_stack(ID) :- gensym('#stack', ID),
                 assert(stack(ID, [], 0)).

is_empty(ID) :- stack(ID, [], _).

top_stack(ID, T) :- is_empty(ID), throw('Stack is empty');
                    stack(ID, [T | _], _).

pop_stack(ID, E) :- is_empty(ID), throw('Stack is empty');
                    retract(stack(ID, [E | T], N)), NewL is N - 1,
                    assert(stack(ID, T, NewL)).

push_stack(ID, E) :- retract(stack(ID, L, N)), NewL is N + 1,
                     assert(stack(ID, [E | L], NewL)).

size(ID, N) :- stack(ID, _, N).