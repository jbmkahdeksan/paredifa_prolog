/**
 * Description:
 * RegEx to NFA compiler. 
 * EIF400 -- Paradigmas de Programacion
 * @since II Term - 2021
 * @authors Team 01-10am
 *  - Andres Alvarez Duran     117520958
 *  - Joaquin Barrientos Monge 117440348
 *  - Oscar Ortiz Chavarria    208260347
 *  - David Zarate Marin       116770797
 **/

:- module(compiler, [begin_compile/2]).

:- use_module(fa, [state_new_id/1, fa_new_id/1]).
:- use_module(converter, [begin_convert/2]).

:- use_module(utils(fa_operations)).
:- use_module(utils(postorder), [postOrder/2]).
:- use_module(utils(stack), [new_stack/1, 
                             push_stack/2,
                             pop_stack/2]).

:- use_module(utils(extra_basics), [isDigit/1, isLetter/1]).

:- table fa_concat/2 as subsumptive.
:- table fa_union/2 as subsumptive.
:- table fa_plus/2 as subsumptive.
:- table fa_hook/2 as subsumptive.
:- table fa_star/2 as subsumptive.


begin_compile(Input, DFA) :-
    reset_gensym('s'),
    new_stack(Stack),
    postOrder(Input, Post),
    maplist(to_fa, Post, FA),
    forall(member(F, FA), fa_handler(Stack, F)),
    pop_stack(Stack, NFA),
    begin_convert(NFA, DFA)
.  

%%%%%%%%%% MAPPER %%%%%%%%%%
to_fa(Token, FA) :-
    isDigit(Token), !,
    fa_atomic(Token, FA)
. 

to_fa(Token, FA) :-
    isLetter(Token), !,
    fa_atomic(Token, FA)
. 

to_fa(Oper, Oper).

%%%%%%%% HANDLER %%%%%%%%%%%
fa_handler(Stack, A) :-
    infix(A), !, fa_infix(Stack, A)
. 

fa_handler(Stack, A) :-
    postfix(A), !, fa_postfix(Stack, A)
. 

fa_handler(Stack, A) :-
    push_stack(Stack, A)
. 

%%%%%%%%%%% Infix Handler %%%%%%%%%%%
fa_infix(Stack, '|') :- fa_union(Stack).

fa_infix(Stack, '^') :-  fa_concat(Stack).

%%%%%%%%%%% Postfix Handler %%%%%%%%%%%
fa_postfix(Stack, '+') :- fa_plus(Stack).

fa_postfix(Stack, '*') :- fa_star(Stack).

fa_postfix(Stack, '?') :- fa_hook(Stack).


%%%%%%%%%% Atomic Handler %%%%%%%%%%%
fa_atomic(empty, FA) :-
    fa_new_id(Id),
    state_new_id(S0),                 
    FA = fa{
        id:Id, 
        vocabulary: null, 
        states:[S0], 
        initial:S0, finals:[S0],
        moves: null
    }
. 

fa_atomic(Atomic, FA) :-
    fa_new_id(Id),
    state_new_id(S0),
    state_new_id(S1),
    format_move(S0, Atomic, S1, Move),
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

    ord_union(A.vocabulary, B.vocabulary, Vocab), 
    subtract_initial_state(A, B, States),
    new_postInitial_moves(concat, A, B, Moves), 

    (member(B.initial, B.finals) ->
        append(A.finals, B.finals, Finals);
        list_to_ord_set(B.finals, Finals)),

    Concat = concat{
        id: Id,
        vocabulary: Vocab,
        states: States,
        initial: A.initial, 
        finals: Finals,
        moves: Moves
    },
    push_stack(Stack, Concat)
    .

%%%% Union handler %%%%%
fa_union(Stack) :-
    pop_stack(Stack, B),
    pop_stack(Stack, A),
    fa_new_id(Id),

    ord_union(A.vocabulary, B.vocabulary, Vocab),
    subtract_initial_state(A, B, States),

    ord_union(A.finals, B.finals, Finals),
    new_postInitial_moves(union, A, B, Moves),

    Union = union{
        id:Id,
        vocabulary: Vocab,
        states: States,
        initial:A.initial, 
        finals: Finals,
        moves: Moves
    },
    push_stack(Stack, Union)
    .

%%%%% +,*,? handler %%%%%

%----> ? handler
fa_star(Stack):-
    fa_hook(Stack),
    fa_plus(Stack),
    fa_new_id(Id),
    pop_stack(Stack, A),   

    Star = star{
        id:Id,
        vocabulary: A.vocabulary,
        states: A.states,
        initial:A.initial, 
        finals: A.finals,
        moves: A.moves
    },
    push_stack(Stack, Star).

%----> + handler
fa_plus(Stack):-
    pop_stack(Stack, A),
    fa_new_id(Id),
    
    find_loops(A, Loops),
    append(A.moves, Loops, Moves), 
    list_to_set(Moves, MovesSet),

    Plus = plus{
        id:Id,
        vocabulary: A.vocabulary,
        states: A.states,
        initial: A.initial, 
        finals: A.finals,
        moves: MovesSet
    },
    push_stack(Stack, Plus)
.  

%----> ? handler
fa_hook(Stack) :-
    pop_stack(Stack, A),
    fa_new_id(Id),
    append([A.initial], A.finals, Finals),
    Hook = hook{
        id:Id,
        vocabulary: A.vocabulary,
        states: A.states,
        initial:A.initial, 
        finals: Finals,
        moves: A.moves
    },
    push_stack(Stack, Hook)
. 