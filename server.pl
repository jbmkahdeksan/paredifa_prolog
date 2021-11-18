/**
 * @author Joaquin Barrientos 
 * @email joaquin2899 at gmail.com
 * @since 2021
 * **/

%some imports
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_head)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).

%creating shortcuts to modules
:- assert(file_search_path(utils, './modules/utils/')).   
:- assert(file_search_path(parser, './modules/parser/')).
:- assert(file_search_path(compiler, './modules/compiler/')).
:- assert(file_search_path(simplifier, './modules/simplifier/')).

%
:- use_module(parser(parser), [begin_parse/2]).
:- use_module(compiler(compiler), [begin_compile/2]).
:- use_module(compiler(converter), [begin_convert/2]).
:- use_module(simplifier(simplifier), [begin_simplify/2]).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(web, '/w', []).
mime:mime_extension('js', 'application/javascript'). 

:- initialization
    (current_prolog_flag(argv, [SPort | _]) -> true; SPort='9000'),
    atom_number(SPort, Port), 
    server(Port).

:- http_handler('/compiler', compile, []).
:- http_handler('/evaluator', evaluate, []).
:- http_handler('/simplifier', simplify, []).
:- http_handler('/converter', convert, []).


:- http_handler(web(.), serve_files, [prefix]).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

serve_files(Request) :-
    http_reply_from_files('web', [], Request).
serve_files(Request) :-
    http_404([], Request).  

compile(Request) :-         
    http_read_json_dict(Request, Data), %Data is a PL-Dict / Request is a JSON
    Value = Data.value,
    begin_parse(Value, Tree),

    
    begin_compile(Tree, FA),
    term_to_atom(Tree, Atom),
    
    Output = json{
        value: Value,
        tree:  Atom,
        fa: FA
    },

    % format(atom(Resp), '{
    %                     "done": "true",
    %                     "tree": "~w",
    %                     "msg": "Hello ~w"}', [Tree, you]),
    % atom_json_dict(Resp, Output, []), % Resp is an Atom / Output is a Dict

    reply_json(Output)
.


simplify(Request) :-
    http_read_json_dict(Request, Data), %Data is a PL-Dict / Request is a JSON
    Value = Data.value,
    begin_parse(Value, Tree),
    begin_simplify(Tree, Simp),
    %begin_compile(Simp, FA),
    term_to_atom(Simp, Atom),
    term_to_atom(Tree, Arbol),

    Output = json{
        tree:  Arbol,
        simplified: Atom
    },

    reply_json(Output)
. 


% evaluate(Request) :-
%     http_read_json_dict(Request, Data), %Data is a PL-Dict / Request is a JSON
%     Value = Data.value,
%     begin_parse(Value, Tree),
%     begin_simplify(Tree, Simp),
%     %begin_compile(Simp, FA),
%     term_to_atom(Simp, Atom),
%     term_to_atom(Tree, Arbol),

%     Output = json{
%         tree:  Arbol,
%         simplified: Atom
%     },

%     reply_json(Output)
% . 

% convert(Request) :-
%     http_read_json_dict(Request, Data), %Data is a PL-Dict / Request is a JSON
%     Value = Data.value,
%     begin_parse(Value, Tree),
%     begin_simplify(Tree, Simp),
%     %begin_compile(Simp, FA),
%     term_to_atom(Simp, Atom),
%     term_to_atom(Tree, Arbol),

%     Output = json{
%         tree:  Arbol,
%         simplified: Atom
%     },

%     reply_json(Output)
% . 
 