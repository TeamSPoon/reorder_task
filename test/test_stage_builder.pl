
:- begin_tests(stage_builder).
:- [reorder_task:prolog/stage_builder].

a(X) :- true.
b(X) :- true.
c(Y) :- true.
d(X,Y) :- true.

test(stage_creation_by_policy) :-
    build_execution_stages([
        is_parallel(a(X)),
        is_shared(b(X)),
        is_sequential(c(Y)),
        d(X,Y)
    ], Stages),
    member(stage(parallel, [a(X)]), Stages),
    member(stage(shared, [b(X)]), Stages),
    member(stage(sequential, [c(Y)]), Stages),
    member(stage(none, [d(X,Y)]), Stages).

test(stage_dependency_ordering) :-
    build_execution_stages([a(X), b(X), c(Y), d(X,Y)], Stages),
    Stages = [stage(none, [a(X)]), _, _, _],
    member(stage(none, [b(X)]), Stages),
    member(stage(none, [c(Y)]), Stages),
    member(stage(none, [d(X,Y)]), Stages).

:- end_tests(stage_builder).
