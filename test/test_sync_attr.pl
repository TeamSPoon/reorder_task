
:- use_module(library(threaded_attvar)).

:- begin_tests(sync_attr).

test(sync_thread_variable_sharing) :-
    % Main thread variable
    A = _,
    % Create a thread that waits for A to become a
    sync_thread_create([A],
        (
            (sleep(1),(A == a -> assertz(shared_result(ok)) ; assertz(shared_result(fail))))
        ),
        TID,
        []
    ),
    % Delay and then bind A
    %sleep(0.2),
    A = a,
    thread_join(TID, _),
    shared_result(ok),
    retractall(shared_result(_)).

test(two_way_sync) :- do_two_way_sync.

:- end_tests(sync_attr).

do_two_way_sync:-
    A = _, B = _,
    sync_thread_create([A, B]
        (
            B = b,
            (A == a -> assertz(two_way(ok)) ; assertz(two_way(fail)))
        ),
        TID,
        []
    ),
    sleep(1.0),
    writeln(B),
    A = a,
    thread_join(TID, _),
    two_way(ok),
    retractall(two_way(_)).


