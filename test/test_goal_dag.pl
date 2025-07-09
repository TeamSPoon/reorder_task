
:- begin_tests(goal_dag).
:- [reorder_task:prolog/goal_dag].
:- [reorder_task:prolog/annotated_goal].

a(X) :- true.
b(X) :- true.
c(Y) :- true.
d(X,Y) :- true.

test(dag_build_with_shared_vars) :-
    build_goal_dag([a(X), b(X), c(Y)], Dag, Vars),
    assertion(Vars == [X, Y]),
    member((a(X), _, _, []), Dag),
    member((b(X), _, _, [AIndex]), Dag),
    member((c(Y), _, _, []), Dag),
    AIndex == 0.

test(dag_with_dependencies_and_policy) :-
    build_goal_dag([is_parallel(a(X)), is_shared(b(X)), is_sequential(c(Y)), d(X,Y)], Dag, _),
    member((a(X), _, Policies1, []), Dag),
    member((b(X), _, Policies2, [I]), Dag),
    member((c(Y), _, Policies3, []), Dag),
    member((d(X,Y), _, _, Indices), Dag),
    member(parallel, Policies1),
    member(shared, Policies2),
    member(sequential, Policies3),
    member(I, Indices).

:- end_tests(goal_dag).
