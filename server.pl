/**
 * Description:
 * Server of the program
 * EIF400 -- Paradigmas de Programacion
 * @since II Term - 2021
 * @authors Team 01-10am
 *  - Andres Alvarez Duran     117520958
 *  - Joaquin Barrientos Monge 117440348
 *  - Oscar Ortiz Chavarria    208260347
 *  - David Zarate Marin       116770797
 **/

% some imports
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).
:- use_module(library(http/html_head)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).

% creating shortcuts to modules
:- assert(file_search_path(utils, './modules/utils/')).   
:- assert(file_search_path(parser, './modules/parser/')).
:- assert(file_search_path(compiler, './modules/compiler/')).
:- assert(file_search_path(simplifier, './modules/simplifier/')).

% loading modules
:- [compiler(opers)].
:- use_module(parser(parser), [begin_parse/2]).
:- use_module(compiler(compiler), [begin_compile/2]).
:- use_module(compiler(converter), [begin_convert/2]).
:- use_module(compiler(fa), [normalize_json/2]).
:- use_module(simplifier(simplifier), [begin_simplify/2]).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(web, '/w', []).
mime:mime_extension('js', 'application/javascript'). 

:- initialization
    (current_prolog_flag(argv, [SPort | _]) -> true; SPort='9000'),
    atom_number(SPort, Port), 
    server(Port).

:- http_handler('/compiler', compile, []).    % regex --> dfa
:- http_handler('/simplifier', simplify, []). % simplify regex --> dfa
:- http_handler('/converter', convert, []).   % nfa --> dfa


:- http_handler(web(.), serve_files, [prefix]).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

serve_files(Request) :-
    http_reply_from_files('web', [], Request).
serve_files(Request) :-
    http_404([], Request).  

compile(Request) :-    
    %compiles without simplification     
    http_read_json_dict(Request, Data),
    begin_parse(Data.value, Tree),
    begin_compile(Tree, FA),
    term_to_atom(Tree, Atom),    
    Output = json{
        done : true,
        tree:  Atom,
        fa: FA
    },
    
    reply_json(Output)
.


simplify(Request) :-
    % simplifies the regex, then compiles it
    http_read_json_dict(Request, Data), 
    begin_parse(Data.value, Tree),
    begin_simplify(Tree, Simp),
    time(begin_compile(Simp, FA)),
    term_to_atom(Simp, Atom),
    Output = json{
        done: true,
        tree:  Atom,
        fa: FA
    },
    reply_json(Output)
. 

convert(Request) :-
    % converts a JSON NFA to a JSON DFA
    http_read_json_dict(Request, Data),     
    normalize_json(Data.value, NFA),
    begin_convert(NFA, DFA),
    Output = json{
        done: true,
        fa: DFA
    },
    reply_json(Output)
. 