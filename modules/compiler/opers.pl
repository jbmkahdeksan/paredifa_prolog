%%%%%%%%%  Transition oper  %%%%%%%%%
:- op(650, xfx, '==>').

%%%%%%%%%  Oper Knowledge base  %%%%%%%%%
% ====> Infix
:- assert(infix('|')).
:- assert(infix('^')).
% ====> Postfix
:- assert(postfix('+')).
:- assert(postfix('*')).
:- assert(postfix('?')).