%%%%%%%%%  Oper Knowledge base  %%%%%%%%%
% ====> Infix
:- assert(infix('|')).
:- assert(infix('^')).
% ====> Postfix
:- assert(postfix('+')).
:- assert(postfix('*')).
:- assert(postfix('?')).


%%%%%%%%%  Transition oper  %%%%%%%%%
:- op(650, xfx, '==>').