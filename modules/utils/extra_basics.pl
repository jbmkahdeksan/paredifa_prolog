/** <module> Extras to dcg/basics

@author loriacarlos@gmail.com
@since 2021

*/
:- module(extra_basics, [
    id/3,
    numeric/3,
    isCodeLetterOrDigit/1,
	isCodePipe/1,
    isDigit/1, isLetter/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PUBLIC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
id(Id, [C | R], O)       :-  isCodeLetter(C), 
                             extractGeneric(Id, [C | R], O, isCodeLetterOrDigit, atom_codes).
numeric(Num, [C | R], O) :-  isCodeDigit(C), 
                             extractGeneric(Num, [C | R], O, isCodeDigit, number_codes).


%%%%%%%%%%%%%%%%%%%%%%%%% PRIVATE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extractGeneric(Obj, I, O, TestCode, CodesToObj) :- extractingGeneric(ObjCodes, I, O, TestCode), 
                                                   call(CodesToObj, Obj, ObjCodes).
extractingGeneric([D | Obj], [D | I], O, TestCode ) :- call(TestCode, D), !,
                                                       extractingGeneric(Obj, I, O, TestCode ).
extractingGeneric([], I, I, _).


isCodeDigit(C) :- char_code(D, C), isDigit(D).
isDigit(D)   :- D @>= '0', D @=< '9'.

isCodeLetter(C) :- char_code(D, C), isLetter(D).

isLetter('_') :- !. 
isLetter('$') :- !. 
isLetter(D)  :- D @>='a', D @=< 'z', !.  % a .. z
isLetter(D)  :- D @>= 'A', D @=< 'Z'.    % A .. Z

isCodeLetterOrDigit(C) :- isCodeLetter(C), !.
isCodeLetterOrDigit(C) :- isCodeDigit(C).

isLetterOrDigit(C) :- isLetter(C),!.
isLetterOrDigit(D) :- isDigit(D),!.

isCodeQuote(C) :- char_code('"', C).
isCodePipe(C)  :- char_code('|', C).
