:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- http_handler(files(.), serve_files, [prefix]).
:- http_handler(root(.), root_handler, []).
:- http_handler(root(topic), topic_handler, []).
:- http_handler(root(comment), comment_handler, []).
:- http_handler(root(post), post_handler, []).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- initialize(topicdb).
:- server(5000).