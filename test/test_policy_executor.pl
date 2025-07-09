
:- begin_tests(policy_executor).
:- [reorder_task:prolog/policy_executor].

a :- format("a started~n"), sleep(1), format("a done~n").
b :- format("b started~n"), sleep(2), format("b done~n").
c :- format("c started~n"), sleep(1), format("c done~n").

test(run_parallel_and_sequential_stages) :-
    Stages = [
        stage(parallel, [a, b]),
        stage(sequential, [c])
    ],
    policy_executor:run_stages(Stages).

test(run_critical_section) :-
    Stages = [stage(critical_section, [a, b, c])],
    policy_executor:run_stages(Stages).

:- end_tests(policy_executor).
