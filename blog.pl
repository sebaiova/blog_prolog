:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_parameters)).
:- use_module(library(persistency)).


http:location(files, '/f', []).

:- persistent topic(id:integer, title:atom, author:atom, text:atom).
:- persistent comment(id:integer, topic:integer, text:atom, author:atom). 
:- persistent topic_nextid(id:integer).
:- persistent comment_nextid(id:integer).
:- persistent comment_count(id_topic:integer, id:integer).

serve_files(Request) :-
	 http_reply_from_files('assets', [], Request).

header -->
    html(
        [\html_requires(files('style.css')), title("Blog"),
        a(href="/", h1("My Blog")),
        nav([a("Register"), "|", a("Login")])
    ]).

body_root -->
    html(
        [
        button([onclick="document.getElementById('topic-box').classList.remove('collapsed')"], "New Topic"), 
        \topic_box(0, "", "", "Post"),
        table(
            [
             tr([th('Topics'), th('Replies'), th('Author'), th('Last post')]), 
             \topics_rows]
            )
        ]).

body_topic(ID_TOPIC) -->
    { 
        topic(ID_TOPIC, Title, Author, Text),
        comment_count(ID_TOPIC, COMMENT_COUNT), 
        format(string(CommentString), "Comments(~w)", COMMENT_COUNT) 
    },
    html([
        table([
            tr([th(colspan=2, Title)]),
            tr([td(Author), td(span([label("Title: "), Title]))]),
            tr([td(''), td(Text)])]
        ),
        form([action="/post", method="POST"],  

        table(tr([td([
                    button([type="button", onclick="document.getElementById('topic-box').classList.remove('collapsed')"], "Edit Post"), 
                    input([type="hidden", name="command", value="Remove"]),
                    input([type="hidden", name="id_topic", value=ID_TOPIC]),
                    button([type="submit"], "Remove")])
                ]))),

        \topic_box(ID_TOPIC, Title, Text, "Edit"),
        table(tr(td([button(onclick="document.getElementById('comment-box').classList.remove('collapsed')", "Reply"), label(CommentString)]))),
        \comment_box(ID_TOPIC),
        \comment_rows(ID_TOPIC)
    ]).

comment_box(ID_TOPIC) -->
    html(
        form([id="comment-box", class="collapsed comment-form", action="/comment", method="POST"],
            table([
                tr(th(colspan=2,"Write your comment")),
                tr([td("Me"), td(rowspan=2, textarea([type="text", name="text", placeholder="Write something...", required=true], ""))]), 
                tr(td(img(src=""))), 
                
                tr([td(colspan=2,[
                    input([type="hidden", name="id_topic", value=ID_TOPIC]),
                    input([type="hidden", name="command", value="PUSH"]),
                    button(type="submit", "Post"),
                    button([type="button", onclick="document.getElementById('comment-box').classList.add('collapsed')"], "Cancel")])])
            ])
        )
    ).

topic_box(ID_TOPIC, TITLE, TEXT, COMMAND) -->
    html(
        form([id="topic-box", class="collapsed topic-form", action="/post", method="POST"],
            [table([
                tr(th("Write your post!")),
                tr(td(div(class="flex-wrapper", [label("Title: "), input([type="text", name="title", value=TITLE, placeholder="Write something...", required=true])]))),
                tr(td(textarea([name="text", required=true, placeholder="Write something..."], TEXT))),
                tr(td([
                    button([type="submit"], COMMAND), 
                    button([type="button", onclick="document.getElementById('topic-box').classList.add('collapsed')"], "Cancel")]))
                ]
            ),
            input([type="hidden", name="command", value=COMMAND]),
            input([type="hidden", name="id_topic", value=ID_TOPIC])
            ]
        )
    ).

topics_rows -->
    {
        findall(tr([td(a(href="/topic"+[id_topic=ID], Title)), td(COMMENT_COUNT), td(Author), td('-')]), 
                (
                    topic(ID, Title, Author, _), 
                    comment_count(ID, COMMENT_COUNT)
                ), 
            Rows)
    },
    html(Rows).

comment_rows(ID_TOPIC) -->
    {
        findall(
            REVERSE_ID-Row, 
            (   Row=div([
                form([action="/comment", method="POST", class="comment-form"],
                table([   
                    input([type="hidden", name="command", value="POP"]),
                    input([type="hidden", name="id_topic", value=ID_TOPIC]),
                    input([type="hidden", name="id_comment", value=ID_COMMENT]),
                    tr(th([colspan=2], Author)),
                    tr([td(img(src='')), td(Text)]),
                    tr([td(colspan=2, [button([type="button", onclick=EXPAND], "Edit"), button([type="submit"], "Remove") ])])
                ])),
                form([action="/comment", method="POST", class="comment-form collapsed", id=ID_EDIT],
                    table([
                        input([type="hidden", name="command", value="EDIT"]),
                        input([type="hidden", name="id_comment", value=ID_COMMENT]),
                        input([type="hidden", name="id_topic", value=ID_TOPIC]),
                        tr(th(colspan=2, r)),
                        tr([td(img(src='')), td(textarea([name="text", required=true, placeholder="Write something..."], Text))]),
                        tr([td(colspan=2, [button([type="commit"], "Edit"), button([type="button", onclick=COLLAPSE], "Cancel")])])
                        ]))
                ]),  
                comment(ID_COMMENT, ID_TOPIC, Text, Author),
                REVERSE_ID is ID_COMMENT*(-1),
                atom_concat('edit-id', ID_COMMENT, ID_EDIT),
                format(string(EXPAND), "document.getElementById('~w').classList.remove('collapsed')", ID_EDIT),
                format(string(COLLAPSE), "document.getElementById('~w').classList.add('collapsed')", ID_EDIT)
            ),
        Pairs),
        keysort(Pairs, SortedPairs),
        pairs_values(SortedPairs, Rows)
    },
    html(Rows).

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

push_topic(TITLE, AUTHOR, TEXT) :-
    retract_topic_nextid(ID_TOPIC),
    NEW_ID_TOPIC is ID_TOPIC + 1,
    assert_topic_nextid(NEW_ID_TOPIC),
    assert_topic(ID_TOPIC, TITLE, AUTHOR, TEXT),
    assert_comment_count(ID_TOPIC, 0).

edit_topic(ID, TITLE, TEXT) :-
    retract_topic(ID, _, AUTHOR, _),
    assert_topic(ID, TITLE, AUTHOR, TEXT).

pop_topic(ID_TOPIC) :-
    retract_topic(ID_TOPIC, _, _, _).

push_comment(ID_TOPIC, AUTHOR, TEXT) :-
    retract_comment_nextid(ID_COMMENT),
    NEW_ID_COMMENT is ID_COMMENT + 1,
    assert_comment_nextid(NEW_ID_COMMENT),
    retract_comment_count(ID_TOPIC, COMMENT_COUNT),
    NEW_COMMENT_COUNT is COMMENT_COUNT + 1,
    assert_comment_count(ID_TOPIC, NEW_COMMENT_COUNT),
    assert_comment(ID_COMMENT, ID_TOPIC, TEXT, AUTHOR).

pop_comment(ID_COMMENT) :-
    comment(ID_COMMENT, ID_TOPIC, _, _),
    retract_comment(ID_COMMENT, _, _, _),
    retract_comment_count(ID_TOPIC, COMMENT_COUNT),
    NEW_COMMENT_COUNT is COMMENT_COUNT - 1,
    assert_comment_count(ID_TOPIC, NEW_COMMENT_COUNT).

edit_comment(ID_COMMENT, TEXT) :-
    retract_comment(ID_COMMENT, ID_TOPIC, _, AUTHOR),
    assert_comment(ID_COMMENT, ID_TOPIC, TEXT, AUTHOR).

:- http_handler(files(.), serve_files, [prefix]).
:- http_handler(root(.), root_handler, []).
:- http_handler(root(topic), topic_handler, []).
:- http_handler(root(comment), comment_handler, []).
:- http_handler(root(post), post_handler, []).

server(Port) :-
    http_server(http_dispatch, [port(Port)]).

initialize(File) :-
    (
    exists_file(File) -> 
        db_attach(File,[])
        ;
        db_attach(File,[]),
        assert_topic_nextid(0),
        assert_comment_nextid(0),
        push_topic('Primer Post', 'dollsteak', 'Saludos a todos!\nNos vemos!'),
        push_comment(0, 'Law', 'Buen Post')
    ).

:- initialize(topicdb).
:- server(5000).


