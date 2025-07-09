
:- use_module(reorder_executor).
:- use_module(library(plunit)).

:- begin_tests(executor_inline).

record_task(X) :-
    nb_setval(recorded, X).

test(inline_rewrites_unsafe_calls, [setup(nb_delete(recorded)), true(Recorded == hello)]) :-
    run_static_sequential([record_task(hello)], true, nb_getval(recorded, Recorded)).

:- end_tests(executor_inline).


:- begin_tests(group_expansion).
:- [reorder_task:prolog/reorder_task_static].

test(multiple_groupables_from_safe_disjunction) :-
    expand_all_groupables(
        [(foo(X) :- a(X) ; b(X)), (bar(Y) :- baz(Y))],
        Gs),
    assertion(Gs == [
        groupable(foo(X) :- a(X)),
        groupable(foo(X) :- b(X)),
        groupable(bar(Y) :- baz(Y))
    ]).

:- end_tests(group_expansion).
