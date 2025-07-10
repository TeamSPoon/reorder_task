% ------------------------------------------------------------------
% Tests for threaded_attvar module
% ------------------------------------------------------------------

:- module(sync_attr_tests, []).

:- use_module(library(plunit)).
:- use_module(library(threaded_attvar)).

:- dynamic(shared_result/1).
:- dynamic(two_way/1).

:- begin_tests(sync_attr).

%= Test: sync_thread_variable_sharing
%
% Verifies that a thread sees the binding of a shared variable
% that occurs after the thread starts.
test(sync_thread_variable_sharing) :- fail,
    A = _,
    sync_thread_create([A],
        (
            on_bind(A,
                ( A == a ->
                    assertz(shared_result(ok))
                ;   assertz(shared_result(fail))
                ))
        ),
        TID,
        []
    ),
    A = a,
    thread_join(TID, _),
    assertion(shared_result(ok)),
    retractall(shared_result(_)).

%= Test: on_bind_basic
%
% Verifies that on_bind/2 triggers when the variable is bound.
test(on_bind_basic) :-
    A = _,
    sync_thread_create(
        (
            on_bind(A, assertz(shared_result(yes)))
        ),
        TID,
        []
    ),
    sleep(0.1),
    A = 42,
    thread_join(TID, _),
    assertion(shared_result(yes)),
    retractall(shared_result(_)).

%= Test: previous_value_tracking
%
% Confirms that the registry tracks the previously known value
% of a synchronized variable correctly.
test(previous_value_tracking) :-
    A = _,
    sync_thread_create([A], (A = hello), TID, []),
    A = goodbye,
    thread_join(TID, _),
    get_attr(A, threaded_attvar, binding(VarID, _)),
    lookup_sync_var(VarID, A),
    assertion(A == goodbye).  % RealVar is the value

%= Test: two_way_sync
%
% Verifies that two shared variables can synchronize bidirectionally.
test(two_way_sync) :-
    A = _, B = _,
    sync_thread_create([A, B],
        (
            B = b,
            on_bind(A,
                ( A == a ->
                    assertz(two_way(ok))
                ;   assertz(two_way(fail))
                ))
        ),
        TID,
        []
    ),
    sleep(0.1),
    writeln(B),
    A = a,
    thread_join(TID, _),
    assertion(two_way(ok)),
    retractall(two_way(_)).

:- end_tests(sync_attr).

