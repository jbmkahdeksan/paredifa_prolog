:- module(compiler, [begin_compile/2, postOrder/2]).
:- use_module(fa, [json_to_fa/2, fa_to_json/2, 
                   state_new_id/1, fa_new_id/1]).
:- use_module(postorder, [postOrder/2]).
:- use_module(utils(stack), [
    new_stack/1, is_empty/1, 
    top_stack/2, push_stack/2,
    size/2, pop_stack/2
]).
:- [opers].


begin_compile(Input, NFA) :-
    new_stack(Stack),
    postOrder(Input, Post),
    maplist(to_fa, Post, FA),
    forall(member(F, FA), fa_handler(Stack, F, NFA)).


%%%%%%%%%% MAPPER %%%%%%%%%%
to_fa(Token, FA) :-
    atom(Token), !,
    fa_atomic(Token, FA).
to_fa(Oper, Oper).


%%%%%%%% HANDLER %%%%%%%%%%%
fa_handler(Stack, A, Out) :-
    infix(A), !, fa_infix(Stack, Out).

fa_handler(Stack, A, Out) :-
    postfix(A), !, fa_postfix(Stack, Out).

fa_handler(Stack, A, _) :-
    push_stack(Stack, A).



%%%%%%%%%%% Infix Handler %%%%%%%%%%%
fa_infix(Stack, '|', Union) :-
    pop_stack(Stack, A),
    pop_stack(Stack, B),
    fa_union(A, B, Union).

fa_infix(Stack, '^', Concat) :-
    pop_stack(Stack, A),
    pop_stack(Stack, B),
    fa_concat(A, B, Concat).



%%%%%%%%%% Atomic Handler %%%%%%%%%%%
fa_atomic(Atomic, FA) :-
    fa_new_id(Id),
    state_new_id(S0),
    state_new_id(S1),
    format(atom(Move), '~w/~w==>~w',[S0,Atomic,S1]),
    list_to_set([Atomic], Vocab),    
    FA = fa{
        id:Id, 
        vocabulary:Vocab, 
        states:[S0, S1], 
        initial:S0, finals:[S1],
        moves:[Move]
    },
    push_stack(Union)
. 

% %%%% ^ handler %%%%%
% fa_concat(A, B, Concat) :-
%     fa_new_id(Id),
%     union(A.vocabulary,B.vocabulary, Vocab),
%     append(A.states, B.states, States),
%     exclude()
%     Concat = concat{
%       id:Id,
%       vocabulary: Vocab,
%       states: States,
%       initial: A.initial, finals: B.finals,
%       moves: 
%     }
%.

%%%% | handler %%%%%
fa_union(A, B, Union) :-
    fa_new_id(Id),
    union(A.vocabulary,B.vocabulary, Vocab),
    append(A.states, B.states, States),
    append(A.finals, B.finals, Finals),
    append(A.moves, B.moves, Moves),
    Union = union{
      id:Id,
      vocabulary: Vocab,
      states: States,
      initial:A.initial, finals: Finals,
      moves: Moves
    }
    .



% %%%% +,*,? handler %%%%%
% fa_plus(FA, Plusified).
% fa_star(FA, Stared).
% fa_hook(FA, Hooked).









