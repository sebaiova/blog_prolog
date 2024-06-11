:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).

:- consult("database.pl").
:- consult("handlers.pl").

http:location(files, '/f', []).
serve_files(Request) :-
    http_reply_from_files('assets', [], Request).

:- http_handler(files(.), serve_files, [prefix]).
:- http_handler(root(.), root_handler, []).
:- http_handler(root(topic), topic_handler, []).
:- http_handler(root(comment), comment_handler, []).
:- http_handler(root(post), post_handler, []).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- initialize_db(topicdb).
:- server(5001).