
:- begin_tests(annotated_goal).
:- [reorder_task:prolog/annotated_goal].

test(extract_policies_simple) :-
    Goal = is_parallel(foo(X)),
    annotated_goal(Goal, Stripped, Vars, Policies),
    assertion(Stripped == foo(X)),
    assertion(Vars == [X]),
    assertion(Policies == [parallel]).

test(extract_policies_nested) :-
    Goal = is_parallel(is_shared(foo(X, Y))),
    annotated_goal(Goal, Stripped, Vars, Policies),
    assertion(Stripped == foo(X, Y)),
    assertion(sort(Vars, [X, Y]) == [X, Y]),
    assertion(sort(Policies, [parallel, shared]) == [parallel, shared]).

test(strip_all_wrappers_multiple) :-
    Goal = is_parallel(is_sequential(is_inlinable(bar(Z)))),
    strip_all_wrappers(Goal, Out),
    assertion(Out == bar(Z)).

:- end_tests(annotated_goal).
