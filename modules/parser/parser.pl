/*
A DCG parser for simple RE expressions
Assumes And (concat, ^) has greater precedence than or (|)
Assumes vocabulary accordingly to extra_basics module.
Grammar start no-terminal is re/3 (see below)
original @author loriacarlos@gmail.com
modified by @
@since 2021
*/

:- module(parser, [ begin_parse/2 ]).

:- use_module(utils(extra_basics),  [isCodeLetterOrDigit/1 as isCodeInVocabulary, 
                                     isCodePipe/1 as isPipe]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PARSER CONTROLLERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
begin_parse(Input, Tree) :-
   atom_codes(Input, Tokens),
   parseReFromTokens(Tokens, Tree)
.

parseReFromTokens(Tokens, Tree) :- re(Tree, Tokens, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% RE DCG GRAMMAR %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
re(RE) --> orReList(LRE), !, {build_re(LRE, RE)}.

orReList(RE) --> andReList(REL), !, restOrReList( LRE ), {build_orRe(REL, LRE, RE)}.
                     
restOrReList( L )  --> "|",  !, orReList( L ).
restOrReList([])   --> []. 

andReList( AndRE )          --> factorRe(REL), !, restAndReList(RER), {build_andRe(REL, RER, AndRE)}.
restAndReList( ^(L, R ) )   --> factorRe( L ), restAndReList(R).
restAndReList([]), [C]      --> [C], {isPipe(C)}, !.
restAndReList([])           --> [].


factorRe(VPF)   --> "(", !, re(V), ")", postReFactor(PF), 
                     {build_re_factor(V, PF, VPF)}.

factorRe(VPF)  --> atomRe(V),
                   postReFactor(PF), 
                   {build_re_factor(V, PF, VPF)}.
                
atomRe(V) --> [C], {isCodeInVocabulary(C)}, {atom_codes(V, [C])}.

postReFactor( +(L) )     --> "+", !, postReFactor( L ).
postReFactor( ?(L) )     --> "?", !, postReFactor( L ). 
postReFactor( *(L) )     --> "*", !, postReFactor( L ). 
postReFactor( [] ), [C]  --> [C], {\+ isCodeInVocabulary(C)}.
postReFactor( [] )       --> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Term Builders %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
build_re('|'(T), T) :- !.
build_re('|'(T, []), T) :- !.
build_re(LRE, LRE).

build_orRe(RE, [], RE) :- !.
build_orRe(RE, OrRE, (RE|OrRE) ).

build_andRe(RE, [], RE) :- !.
build_andRe(RE, ^(REL,RER), AndRe) :- build_andRe(^(RE, REL), RER, AndRe),!.
build_andRe(RE, AndRE, ^(RE, AndRE)).

build_re_factor(RE, +(L), (RE1)) :- build_re_factor(+(RE), L, RE1), !.
build_re_factor(RE, *(L), (RE1)) :- build_re_factor(*(RE), L, RE1), !.
build_re_factor(RE, ?(L), (RE1)) :- build_re_factor(?(RE), L, RE1), !.
build_re_factor(RE, [], RE).

