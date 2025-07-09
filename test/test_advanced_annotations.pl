
:- begin_tests(advanced_annotations).
:- [reorder_task:prolog/reorder_task_static].

% Sample annotated logic
example_task(t1 :- is_parallel(a), is_shared(b), is_critical_section(c), is_guarded_by(ready, d), is_delayable(e), is_ordered_after(e, c), is_pure(f)).

a :- format("✔ a (parallel)~n").
b :- format("✔ b (shared)~n").
c :- format("✔ c (critical section)~n").
d :- format("✔ d (guarded)~n").
e :- format("✔ e (delayable)~n").
f :- format("✔ f (pure)~n").
ready :- true.

test(annotation_extraction) :-
    example_task(Clause),
    extract_annotations(Clause, List),
    assertion(List == [
        (parallel, a),
        (shared, b),
        (critical_section, c),
        (guarded_by, d),
        (delayable, e),
        (ordered_after, is_ordered_after(e, c)),
        (pure, f)
    ]).

:- end_tests(advanced_annotations).
