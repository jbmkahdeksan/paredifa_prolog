/**
 * @author Joaquin Barrientos 
 * @email joaquin2899 at gmail.com
 * @since 2021
 * **/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_head)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).


:- assert(file_search_path(utils, './modules/utils/')).   
:- assert(file_search_path(parser, './modules/parser/')).
:- assert(file_search_path(compiler, './modules/compiler/')).
:- assert(file_search_path(simplifier, './modules/simplifier/')).

:- use_module(parser(parser), [begin_parse/2]).
:- use_module(compiler(compiler), [begin_compile/2, postOrder/2]).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(web, '/w', []).
mime:mime_extension('js', 'application/javascript'). 

:- initialization
    (current_prolog_flag(argv, [SPort | _]) -> true; SPort='9000'),
    atom_number(SPort, Port),    
    server(Port).

:- http_handler('/compiler', compile, []).
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

    postOrder(Tree, Post),
    begin_compile(Tree, FA),
    term_to_atom(Tree, Atom),
    
    Output = json{
        tree:  [Atom],
        trail: Post,
        fa: FA
    },

    % format(atom(Resp), '{
    %                     "done": "true",
    %                     "tree": "~w",
    %                     "msg": "Hello ~w"}', [Tree, you]),
    % atom_json_dict(Resp, Output, []), % Resp is an Atom / Output is a Dict


    reply_json(Output).



