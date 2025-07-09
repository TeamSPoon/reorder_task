
:- begin_tests(complex_disjunction_tracing).
:- [reorder_task:prolog/reorder_task_static].

test(debug_expand_complex) :-
    Clauses = [
        (f(X) :- a(X) ; b(X)),
        (g(Y) :- (p(Y), q(Y)) ; (r(Y), !)),
        (h(Z) :- x(Z))
    ],
    debug_expand_all_groupables(Clauses, _).

:- end_tests(complex_disjunction_tracing).
