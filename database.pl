:- use_module(library(persistency)).

:- persistent topic(id:integer, title:atom, author:atom, text:atom).
:- persistent comment(id:integer, topic:integer, text:atom, author:atom). 
:- persistent topic_nextid(id:integer).
:- persistent comment_nextid(id:integer).
:- persistent comment_count(id_topic:integer, id:integer).

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


initialize_db(File) :-
(
exists_file(File) -> 
    db_attach(File,[])
    ;
    db_attach(File,[]),
    assert_topic_nextid(0),
    assert_comment_nextid(0),

    push_topic('Introduction to OpenCL', 'admin', 'OpenCL (Open Computing Language) is a framework for writing programs that execute across heterogeneous platforms consisting of CPUs, GPUs, and other processors.'),
    push_comment(0, 'user1', 'Great introduction! Looking forward to more posts on OpenCL.'),
    
    push_topic('OpenCL Memory Model', 'admin', 'Understanding the OpenCL memory model is crucial for writing efficient OpenCL programs. It includes global memory, constant memory, local memory, and private memory.'),
    push_comment(1, 'user2', 'This is very informative. Thanks for breaking it down!'),
    push_comment(1, 'user3', 'Can you provide more details on local memory?'),

    push_topic('OpenCL Kernels', 'admin', 'Kernels are the functions that run on OpenCL devices. They are written in OpenCL C and are executed by multiple work-items in parallel.'),
    push_comment(2, 'user4', 'Can you provide an example of a simple OpenCL kernel?'),
    push_comment(2, 'user5', 'How do you handle synchronization within a kernel?'),

    push_topic('OpenCL Platforms and Devices', 'admin', 'An OpenCL platform consists of a host and one or more OpenCL devices. Devices are divided into compute units, which are further divided into processing elements.'),
    push_comment(3, 'user6', 'I am a bit confused about the difference between compute units and processing elements. Can you clarify?'),

    push_topic('OpenCL Buffers and Images', 'admin', 'Buffers and images are the primary data structures used to transfer data between the host and OpenCL devices. Buffers are linear arrays, while images are multi-dimensional data structures.'),
    
    push_topic('OpenCL Command Queues', 'admin', 'Command queues are used to control the execution of kernels and the transfer of data. Commands are issued to a command queue and executed in-order or out-of-order.'),
    push_comment(5, 'user7', 'Can you explain the difference between in-order and out-of-order command queues?'),

    push_topic('OpenCL Event Management', 'admin', 'Events in OpenCL are used to synchronize operations in command queues. They help manage dependencies between commands and ensure correct execution order.'),
    push_comment(6, 'user8', 'How do you handle event synchronization in complex OpenCL programs?'),
    push_comment(6, 'user9', 'Can you provide an example of event usage?'),
    
    push_topic('OpenCL Error Handling', 'admin', 'Proper error handling in OpenCL is essential for debugging and ensuring the robustness of applications. The OpenCL API provides a set of error codes to help identify issues.'),

    push_topic('Performance Optimization in OpenCL', 'admin', 'Optimizing OpenCL programs involves tuning memory access patterns, balancing workloads, and minimizing synchronization overhead.'),
    push_comment(8, 'user10', 'Any tips for beginners on where to start with performance optimization?'),
    push_comment(8, 'user11', 'What are some common pitfalls in performance optimization?'),

    db_sync(gc)
).