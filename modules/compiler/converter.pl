:- module(to_dfa, [convert/2]).
:- [opers].
:- use_module(fa, [json_to_fa/2, fa_to_json/2, 
                   fa_new_id/1, state_new_id/1,
                   fa_initial/2, fa_finals/2, 
                   fa_states/2, fa_moves/2, 
                   fa_vocab/2, search_move/4, 
                   fa_set_states/2, fa_set_vocab/2,
                   fa_set_moves/2, 
                   fa_set_finals/2, fa_set_initial/2]).

:- use_module(utils(stack), [
    new_stack/1, is_empty/1, 
    top_stack/2, push_stack/2,
    size/2, pop_stack/2
]).



convert(NFA, JSON) :-
    fa_new_id(DFA),   

    json_to_fa(NFA, Obj),
    construct(Obj, DFA),   

    fa_to_json(DFA, JSON)
.    


construct(NFA, DFA) :-
    fa_initial(NFA, S0),
    new_stack(Stack), 
    push_stack(Stack, [S0]),   

    format(atom(Initial), '[~w]', [S0]),
    fa_set_initial(DFA, Initial),

    fa_vocab(NFA, Vocab),
    fa_set_vocab(DFA, Vocab),

    creator(Stack, DFA, NFA),

    fa_finals(NFA, NFAfinals),
    fa_states(DFA, DFAstates),
    findall(State, (member(State, DFAstates),
                    member(X, State),
                    member(X, NFAfinals)),                   
                    Finals),
    forall(member(F, Finals), fa_set_finals(DFA, F))
.

creator(Stack, _, _) :- is_empty(Stack), !.

creator(Stack, DFA, NFA) :-
    pop_stack(Stack, Current), 
    add_state(DFA, Current),

    fa_vocab(DFA, Vocab), 
    maplist([Y, Z] >> (finder(NFA, Y, Current, Z)), Vocab, Zs), %finds neighbors Z.
    
    forall(member(Zetita, Zs), stack_handler(Stack, DFA, Zetita)), % conditional push z
   
    maplist([Y, Z, Term] >> transformer(Current, Y, Z, Term), Vocab, Zs, Miracle), %creates moves type Current/Y ==> Z, for each Y and Z (a Zip).
    forall(member(T, Miracle), (fa_set_moves(DFA, T))), %assert moves 

    creator(Stack, DFA, NFA) %recursive call to the stack.
.


%Finds neighbors for each State in stack. 

finder(NFA, Y, Current, List) :- 
    atom_number(Y, N),
    findall(Z, (member(X, Current), search_move(NFA, X, N, Z)), List),
    nth0(0, List, _).
    

finder(NFA, Y, Current, void) :-
    findall(Z, (member(X, Current), search_move(NFA, X, Y, Z)), _).



%Transforms the Current state on stack into a Move for each Y in Vocab and for each discovered neighbor Z.
transformer(Current, Y, Z, Current/Y==>Z).


%Conditional push on stack
stack_handler(_, DFA, Z) :-
    fa_states(DFA, States), 
    member(Z, States), !.

stack_handler(Stack, _, Z) :-
    push_stack(Stack, Z)
.

%Adds a state to the DFA
add_state(DFA, Current) :-
    fa_set_states(DFA, Current)
.  






