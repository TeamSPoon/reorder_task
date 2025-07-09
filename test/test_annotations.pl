
:- begin_tests(enforced_annotations).
:- [reorder_task:prolog/reorder_task_static].

% Dummy annotated goals
is_parallel(a).
is_sequential((b, c)).
is_reorderable((d, e)).

% Stub implementation goals
a :- format("a\n").
b :- format("b\n").
c :- format("c\n").
d :- format("d\n").
e :- format("e\n").

test(strip_enforcement_parallel) :-
    strip_enforcement(is_parallel(foo), Stripped, Policy),
    assertion(Stripped == foo),
    assertion(Policy == parallel).

test(strip_nested_policy) :-
    strip_enforcement(is_parallel(is_sequential(foo)), Stripped, Policy),
    assertion(Stripped == foo),
    assertion(Policy == sequential).

test(extract_enforced_goals) :-
    extract_enforced_goals((t :- is_parallel(a), is_sequential(b)), Pairs),
    assertion(Pairs == [(parallel, a), (sequential, b)]).

:- end_tests(enforced_annotations).
