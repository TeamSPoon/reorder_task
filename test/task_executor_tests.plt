
:- begin_tests(task_executor).

:- use_module(task_order_static).
:- use_module(task_executor).
:- dynamic called_step/2.

% Sample test setup
get_sample_task([A,B,C,D], TaskList, PreTest, FinalTest) :-
    TaskList = [
        step_a_requires_1_bound(1, A),
        step_abd_requires_2_bound(2, A, B, D),
        step_was_ac_requires_1_bound(1, A, C),
        step_was_db_requires_1_bound(1, D, B)
    ],
    PreTest = (C = c, D = d),
    FinalTest = ([A,B,C,D] == [a,b,c,d]).

% Step predicates
step_a_requires_1_bound(1, A) :-
    must_have_bound(step_a_requires_1_bound(A), [A], 1),
    A = a, assertz(called_step(4, step_a_requires_1_bound(1, A))).

step_abd_requires_2_bound(2, A, B, D) :-
    must_have_bound(step_abd_requires_2_bound(A, B, D), [A, B, D], 2),
    A = a, B = b, D = d,
    assertz(called_step(3, step_abd_requires_2_bound(2, A, B, D))).

step_was_ac_requires_1_bound(1, A, C) :-
    must_have_bound(step_was_ac_requires_1_bound(A, C), [A, C], 1),
    A = a, C = c,
    assertz(called_step(1, step_was_ac_requires_1_bound(1, A, C))).

step_was_db_requires_1_bound(1, D, B) :-
    must_have_bound(step_was_db_requires_1_bound(D, B), [D, B], 1),
    D = d, B = b,
    assertz(called_step(2, step_was_db_requires_1_bound(1, D, B))).

must_have_bound(Goal, Vars, Threshold) :-
    include(nonvar, Vars, Bound),
    length(Bound, Len),
    ( Len >= Threshold -> true
    ; (format("[WAIT] ~w needs ~w bound~n", [Goal, Threshold]), fail)
    ).

test(static_sequential_exec) :-
    get_sample_task(Vars, TaskList, PreTest, FinalTest),
    run_static_sequential(TaskList, PreTest, FinalTest).

test(static_parallel_exec) :-
    get_sample_task(Vars, TaskList, PreTest, FinalTest),
    run_static_parallel(TaskList, PreTest, FinalTest).

test(dynamic_ready_loop_exec) :-
    get_sample_task(Vars, TaskList, PreTest, FinalTest),
    run_dynamic_ready_loop(TaskList, PreTest, FinalTest).

test(dynamic_parallel_ready_exec) :-
    get_sample_task(Vars, TaskList, PreTest, FinalTest),
    run_dynamic_parallel_ready(TaskList, PreTest, FinalTest).

:- end_tests(task_executor).
