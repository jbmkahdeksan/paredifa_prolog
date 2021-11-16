:- module(to_dfa, [convert/2]).
:- [opers].
:- use_module(fa, [json_to_fa/2, fa_to_json/2, 
                   fa_new_id/1, state_new_id/1,
                   fa_initial/2, fa_finals/2, 
                   fa_states/2, fa_moves/2, 
                   fa_vocab/2, search_move/4]).

:- use_module(utils(stack), [
    new_stack/1, is_empty/1, 
    top_stack/2, push_stack/2,
    size/2, pop_stack/2
]).

convert(NFA, DFA) :-
    json_to_fa(NFA, FA),
    new_stack(Stack), 
    push_stack(Stack, [NFA.initial]),
    create_nfa(Stack, FA, States),
    fa_new_id(Id),
    
    DFA = dfa{
        id: Id,
        vocabulary: NFA.vocabulary, 
        states: States, 
        initial: [NFA.initial], 
        finals: NFA.finals,
        moves: NFA.moves
    }
.    

create_nfa(Stack, FA, States) :- 
    pop_stack(Stack, Current),
    fa_vocab(FA, Vocab),
    findall(List, 
        (member(Y,Vocab), 
         member(X, Current), 
         setof((Y, Z), (search_move(FA, X, Y, Z)), List)), 
        States).  


    

% find_states(Stack, NFA, Y, Current) :-
%     findall(Z, )
%     push_stack(Stack, Zs).

%join_states
%empty_state
%set_final
    