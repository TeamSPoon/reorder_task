
:- begin_tests(trace_executor_timestamps).
:- [reorder_task:prolog/trace_executor].

a :- sleep(0.1), format("running a~n").
b :- sleep(0.2), format("running b~n").

test(logs_include_timestamps) :-
    retractall(trace_event(_)),
    run_traced_stages([stage(parallel, [a, b])]),
    findall(TS, trace_event(event(_, TS)), Timestamps),
    maplist(number, Timestamps),
    length(Timestamps, N),
    assertion(N >= 2).  % start + done for each

:- end_tests(trace_executor_timestamps).
