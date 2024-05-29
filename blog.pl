:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_files)).
:- use_module(library(persistency)).


http:location(files, '/f', []).

:- persistent topic(title:atom, author:atom). 

serve_files(Request) :-
	 http_reply_from_files('assets', [], Request).

header -->
    html([\html_requires(files('style.css')), title("Blog")]).

body -->
    html(
        table(
            [tr([th('Topics'), th('Replies'), th('Author'), th('Last post')]), 
             tr([td('Tutorial - Prolog Web'), td(0), td('dollsteak'), td('-')])]
            )
        ).

main(Request) :- reply_html_page(\header, \body).

:- http_handler(files(.), serve_files, [prefix]).
:- http_handler(/, main, []).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

initialize(File) :-
        (
    exists_file(File) -> 
        db_attach(File,[])
        ;
        db_attach(File,[]),
        assert_topic('Claudio Vauchere','299155309481')
    ).

:- initialize(topicdb).
:- server(5002).
