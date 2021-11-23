/**
 * Description:
 * Operand knowledge base
 * EIF400 -- Paradigmas de Programacion
 * @since II Term - 2021
 * @authors Team 01-10am
 *  - Andres Alvarez Duran 117520958
 *  - Joaquin Barrientos Monge 117440348
 *  - Oscar Ortiz Chavarria 208260347
 *  - David Zarate Marin 116770797
 **/

% ====> Infix
:- assert(infix('|')).
:- assert(infix('^')).
% ====> Postfix
:- assert(postfix('+')).
:- assert(postfix('*')).
:- assert(postfix('?')).

%%%%%%%%%  Transition oper  %%%%%%%%%
:- op(650, xfx, '==>').