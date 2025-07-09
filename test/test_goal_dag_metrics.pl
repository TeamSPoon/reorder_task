
:- begin_tests(goal_dag_metrics).
:- [reorder_task:prolog/goal_dag].

test(dag_nodes_have_dependencies) :-
    build_goal_dag([
        is_parallel(a(X)),
        b(X),
        is_sequential(c(Y))
    ], Dag, Vars),
    assertion(Vars == [X, Y]),
    member((b(X), _, _, Deps), Dag),
    assertion(Deps \= []).

test(all_nodes_present) :-
    build_goal_dag([a(X), b(X), c(Y)], Dag, _),
    length(Dag, 3),
    maplist(arg(1), Dag, [a(X), b(X), c(Y)]).

:- end_tests(goal_dag_metrics).
