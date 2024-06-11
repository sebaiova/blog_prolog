:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

:- consult("database.pl").

header -->
    html(
        [\html_requires(files("style.css")), title("Blog"),
        a(href="/", h1("OpenCL Forum")),
        nav([a("Register"), "|", a("Login")])
    ]).

body_root -->
    % / body html para frontpage
    html([
        button([onclick="document.getElementById('topic-box').classList.remove('collapsed')"], "New Topic"), 
        \topic_box(0, "", "", "Post"),
        table(
            [
             tr([th('Topics'), th('Replies'), th('Author')]), 
             \topics_rows]
            )
        ]).

body_topic(ID_TOPIC) -->
    { 
        topic(ID_TOPIC, Title, Author, Text),
        comment_count(ID_TOPIC, COMMENT_COUNT), 
        format(string(CommentString), "Comments(~w)", COMMENT_COUNT) 
    },
    % /topics body html, pagina para ver un topic determinado
    html([
        table([
            tr([th(colspan=2, Title)]),
            tr([td(Author), td(rowspan=2, Text)]),
            tr([td(img(src=""))])]
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
        % Form para crear comentarios
        form([id="comment-box", class="collapsed comment-form", action="/comment", method="POST"],
            table([
                tr(th(colspan=2,"Write your comment")),
                tr([td("Me"), td(rowspan=2, textarea([type="text", name="text", placeholder="Write something...", required=true], ""))]), 
                tr(td(img(src=""))), 
                
                tr([td(colspan=2,[
                    input([type="hidden", name="id_topic", value=ID_TOPIC]),
                    input([type="hidden", name="command", value="PUSH"]),
                    button(type="submit", "Confirm"),
                    button([type="button", onclick="document.getElementById('comment-box').classList.add('collapsed')"], "Cancel")])])
            ])
        )
    ).

topic_box(ID_TOPIC, TITLE, TEXT, COMMAND) -->
    html(
        % Form para crear/editar topics
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
        % Cada fila de topics para listarlos en la frontpage
        findall(tr([td(a(href="/topic"+[id_topic=ID], Title)), td(COMMENT_COUNT), td(Author)]), 
                (
                    topic(ID, Title, Author, _), 
                    comment_count(ID, COMMENT_COUNT)
                ), 
            Rows)
    },
    html(Rows).

comment_rows(ID_TOPIC) -->
    {
        % Busco todos los comentarios del topic ID_TOPIC,
        % y armo una lista de pares, que contenga la ID de cada comentario invertida (*-1)
        % y el html de cada fila de comentarios, para luego ordenarlos de forma descendiente
        % y que los comentarios mas nuevos aparezcan primero.
        % Hay dos forms, porque uno es el comentario, el otro es el plegable para editarlo, no alcance a modularizar bien
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
                        tr(th(colspan=2, "Editing comment...")),
                        tr([td(img(src='')), td(textarea([name="text", required=true, placeholder="Write something..."], Text))]),
                        tr([td(colspan=2, [button([type="commit"], "Confirm"), button([type="button", onclick=COLLAPSE], "Cancel")])])
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

