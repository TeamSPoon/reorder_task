
:- use_module(sync_attr).

:- begin_tests(sync_attr).

test(sync_thread_variable_sharing) :-
    % Main thread variable
    A = _,
    % Create a thread that waits for A to become a
    sync_thread_create(
        (
            A == a -> assertz(shared_result(ok)) ; assertz(shared_result(fail))
        ),
        TID,
        []
    ),
    % Delay and then bind A
    sleep(0.2),
    A = a,
    thread_join(TID, _),
    shared_result(ok),
    retractall(shared_result(_)).

test(two_way_sync) :-
    A = _, B = _,
    sync_thread_create(
        (
            B = b,
            A == a -> assertz(two_way(ok)) ; assertz(two_way(fail))
        ),
        TID,
        []
    ),
    sleep(0.2),
    A = a,
    thread_join(TID, _),
    two_way(ok),
    retractall(two_way(_)).

:- end_tests(sync_attr).
