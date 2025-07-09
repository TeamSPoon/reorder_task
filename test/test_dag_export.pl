
:- begin_tests(dag_export).
:- [reorder_task:prolog/dag_export].

a(X) :- true.
b(X) :- true.
c(Y) :- true.

test(export_dag_dot_file) :-
    export_goal_dag_dot([
        is_parallel(a(X)),
        b(X),
        is_sequential(c(Y))
    ], 'test_output.dot').

:- end_tests(dag_export).
