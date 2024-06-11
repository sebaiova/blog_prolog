:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dispatch)).

:- consult("html.pl").

root_handler(_Request) :- 
    reply_html_page(\header, \body_root).

topic_handler(Request) :- 
    http_parameters(Request,
        [
         id_topic(ID_TOPIC, [default(0), integer])
        ]),
    reply_html_page(\header, \body_topic(ID_TOPIC)).

post_handler(Request) :-
    http_parameters(Request,
        [
         title(TITLE, [default('')]), 
         text(TEXT, [default('')]),
         command(COMMAND, [default('')]),
         id_topic(ID_TOPIC, [default(0), integer])
        ]),   
    (COMMAND == 'Post' -> push_topic(TITLE, 'Me', TEXT); true),
    (COMMAND == 'Remove' -> pop_topic(ID_TOPIC); true),
    (COMMAND == 'Edit' -> edit_topic(ID_TOPIC, TITLE, TEXT); true),
    http_redirect(moved, '/', _).

comment_handler(Request) :-
    http_parameters(Request,
        [id_topic(ID_TOPIC, [default(0), integer]),
         text(TEXT, [default('a')]),
         command(COMMAND, [default('')]),
         id_comment(ID_COMMENT, [default(0), integer])
        ]),
    (COMMAND == 'POP' -> pop_comment(ID_COMMENT); true),
    (COMMAND == 'PUSH' -> push_comment(ID_TOPIC, 'Me', TEXT); true),
    (COMMAND == 'EDIT' -> edit_comment(ID_COMMENT, TEXT); true),
    atom_concat('/topic?id_topic=', ID_TOPIC, URL),
    http_redirect(see_other, URL, _).

