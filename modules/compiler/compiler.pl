:- module(compiler, [begin_compile/2, postOrder/2]).
:- use_module(fa, [json_to_fa/2, fa_to_json/2, 
                   state_new_id/1, fa_new_id/1]).
:- use_module(postorder, [postOrder/2]).
:- use_module(utils(stack), [
    new_stack/1, is_empty/1, 
    top_stack/2, push_stack/2,
    size/2, pop_stack/2
]).
:- use_module(utils(extra_basics), [isDigit/1, isLetter/1]).
:- [opers].


begin_compile(Input, NFA) :-
    reset_gensym('$fa_'), reset_gensym('s'),
    new_stack(Stack),
    postOrder(Input, Post),
    maplist(to_fa, Post, FA),
    forall(member(F, FA), fa_handler(Stack, F)),
    pop_stack(Stack, NFA)
    .


%%%%%%%%%% MAPPER %%%%%%%%%%
to_fa(Token, FA) :-
    isDigit(Token), !,
    fa_atomic(Token, FA).

to_fa(Token, FA) :-
    isLetter(Token), !,
    fa_atomic(Token, FA).

to_fa(Oper, Oper).


%%%%%%%% HANDLER %%%%%%%%%%%
fa_handler(Stack, A) :-
    infix(A), !, fa_infix(Stack, A).

fa_handler(Stack, A) :-
    postfix(A), !, fa_postfix(Stack, A).

fa_handler(Stack, A) :-
    push_stack(Stack, A).


%%%%%%%%%%% Infix Handler %%%%%%%%%%%
fa_infix(Stack, '|') :-
    fa_union(Stack).

fa_infix(Stack, '^') :-
    fa_concat(Stack).

%%%%%%%%%%% Postfix Handler %%%%%%%%%%%
fa_postfix(Stack, '+') :-
    fa_plus(Stack).

fa_postfix(Stack, '*') :-
    fa_star(Stack).

fa_postfix(Stack, '?') :-
    fa_hook(Stack).




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
    }
. 


%%% Concat handler %%%%%
fa_concat(Stack) :-
    pop_stack(Stack, B),
    pop_stack(Stack, A),
    fa_new_id(Id),

    union(A.vocabulary,B.vocabulary, Vocab),

    exclude([State] >> member(State, A.finals), A.states, NewA),
    append(NewA, B.states, States),

    maplist([M, Out] >> (final_concat_handler(A, M, B, Out)),
                        A.moves, MovesA),
    append(MovesA, B.moves, Moves),

    Concat = concat{
      id:Id,
      vocabulary: Vocab,
      states: States,
      initial:A.initial, finals: B.finals,
      moves: Moves
    },
    push_stack(Stack, Concat)
    .


%%%% Union handler %%%%%
fa_union(Stack) :-
    pop_stack(Stack, B),
    pop_stack(Stack, A),
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
    },
    push_stack(Stack, Union)
    .



% %%%% +,*,? handler %%%%%
% fa_plus(Stack).
% fa_star(Stack).
% fa_hook(Stack).

final_concat_handler(A, M, B, Out) :-
    op(650, xfx, '==>'),
    atom_to_term(M, X/Y ==> Z, _),  %  Not working!
    member(Z, A.finals), !,
    format(atom(Out), '~w/~w==>~w',[X,Y,B.initial])
    .

final_concat_handler(_, M, _, M).
    











