:- module(compiler, [begin_compile/2, postOrder/2]).

:- [opers].
:- use_module(fa, [state_new_id/1, fa_new_id/1]).

:- use_module(postorder, [postOrder/2]).
:- use_module(converter, [convert/2]).
:- use_module(utils(stack), [
    new_stack/1, is_empty/1, 
    top_stack/2, push_stack/2,
    size/2, pop_stack/2
]).

:- use_module(utils(extra_basics), [isDigit/1, isLetter/1]).



begin_compile(Input, DFA) :-
    op(650, xfx, '==>'),
    reset_gensym('s'),
    new_stack(Stack),
    postOrder(Input, Post),
    maplist(to_fa, Post, FA),
    forall(member(F, FA), fa_handler(Stack, F)),
    pop_stack(Stack, NFA),
    convert(NFA, DFA)
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

    union(A.vocabulary, B.vocabulary, Vocab),

    subtract(B.states, [B.initial], NewB),
    append(A.states, NewB, States),   

    findall(M, (member(M, B.moves), final_concat_handler(B, M)), PostInitials),

    subtract(B.moves, PostInitials, MovesB),
    maplist([M, Z] >> (atom_to_term(M, _/_==>Z, _)), PostInitials, PostStates),
    

    findall(Move, (member(Z, PostStates), 
                   member(X, A.finals), 
                   member(Y, B.vocabulary),
                   format(atom(Move), '~w/~w==>~w', [X, Y, Z])), 
                   MovesA),

    append(A.moves, MovesA, Connect),

    append(Connect, MovesB, Moves),

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

    exclude([State] >> member(State, [B.initial]), B.states, NewB),
    append(A.states, NewB, States),

    maplist([M, Out] >> (initial_union_handler(B, M, A, Out)),
                         B.moves, MovesB),

    append(A.moves, MovesB, Moves),
    append(A.finals, B.finals, Finals),

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
fa_plus(Stack):-
    pop_stack(Stack, A),
    fa_new_id(Id),
    
    include([Move]>>(atom_to_term(Move, X/_==>_,_), member(X, [A.initial])), A.moves, Pivot),   
    maplist([Move, Y]>>(atom_to_term(Move, _/Y==>_,_)), Pivot, Ys),
    maplist([Move, Z]>>(atom_to_term(Move, _/_==>Z,_)), Pivot, Zs),

    subtract(A.finals, [A.initial], Finals),

    findall(Loop, (member(Y, Ys), member(Z, Zs), member(Final, Finals), 
                   format(atom(Loop),'~w/~w==>~w',[Final, Y, Z])), Loops),

    append(A.moves, Loops, Moves),
    list_to_set(Moves, MovesSet),

    Plus = plus{
        id:Id,
        vocabulary: A.vocabulary,
        states: A.states,
        initial:A.initial, finals: A.finals,
        moves: MovesSet
    },
    push_stack(Stack, Plus).

fa_hook(Stack) :-
    pop_stack(Stack, A),
    fa_new_id(Id),
    append([A.initial], A.finals, Finals),
    Hook = hook{
      id:Id,
      vocabulary: A.vocabulary,
      states: A.states,
      initial:A.initial, finals: Finals,
      moves: A.moves
    },
    push_stack(Stack, Hook).

fa_star(Stack):-
    fa_hook(Stack),
    fa_plus(Stack),
    fa_new_id(Id),
    pop_stack(Stack, A),   

    Star = star{
        id:Id,
        vocabulary: A.vocabulary,
        states: A.states,
        initial:A.initial, finals: A.finals,
        moves: A.moves
    },
    push_stack(Stack, Star).



%%%%%%%%  FA Operations %%%%%%%%%
final_concat_handler(B, M) :-
    atom_to_term(M, X/_ ==> _, _), 
    member(X, [B.initial]).


initial_union_handler(B, M, A, Out) :-
    atom_to_term(M, X/Y ==> Z, _), 
    member(X, [B.initial]), !,
    format(atom(Out), '~w/~w==>~w',[A.initial,Y,Z]).
initial_union_handler(_, M, _, M).













