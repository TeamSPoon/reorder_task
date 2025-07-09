
:- begin_tests(trace_executor).
:- [reorder_task:prolog/trace_executor].

a :- format("a logic~n"), sleep(1).
b :- format("b logic~n"), sleep(2).
c :- format("c logic~n"), sleep(1).

test(run_traced_execution) :-
    Stages = [
        stage(parallel, [a, b]),
        stage(sequential, [c])
    ],
    run_traced_stages(Stages).

:- end_tests(trace_executor).
