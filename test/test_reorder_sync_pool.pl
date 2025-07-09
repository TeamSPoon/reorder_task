
:- use_module(reorder_sync_pool).
:- use_module(reorder_task_static).
:- use_module(reorder_executor).
:- use_module(library(plunit)).

:- begin_tests(sync_pool).

sample_tasks([
    step_a_requires_1_bound(1, A),
    step_abd_requires_2_bound(2, A, B, D),
    step_was_ac_requires_1_bound(1, A, C),
    step_was_db_requires_1_bound(1, D, B)
]).

test(run_sync_pool_optimal_threads, [setup((sample_tasks(Tasks), C = c, D = d)), true(length(Handles, 2))]) :-
    sample_tasks(Tasks),
    run_sync_task_pool_optimal(2, Tasks, Handles),
    forall(member(TID, Handles), thread_join(TID, _)).

:- end_tests(sync_pool).


:- begin_tests(group_parallel_stages_disjunction).
:- [reorder_task:prolog/reorder_task_static].

test(split_and_group_disjunctions) :-
    group_parallel_stages(
        [(foo(X) :- a(X) ; b(X)), (bar(Y) :- baz(Y))],
        [], Stages, Left),
    assertion(Stages \= []),
    assertion(Left == []).

:- end_tests(group_parallel_stages_disjunction).
