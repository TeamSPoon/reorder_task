
:- use_module(threaded_attvar).
:- use_module(library(plunit)).

:- begin_tests(shared_sync_logic).

test(shared_variables_basic) :-
    A = _, B = _, C = _, D = _,
    shared_variables([foo(A,B), bar(B,C), baz(D)], Shared),
    assertion(Shared == [B]).

test(auto_copy_term_with_sync, [setup((A = _, B = _, Task = test(A,B))), true(Copy == test(A,B))]) :-
    engine_self(E),
    copy_term_with_sync(Task, Copy, E, E, auto).

:- end_tests(shared_sync_logic).
