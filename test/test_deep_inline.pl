
:- begin_tests(deep_inline).
:- [reorder_task:prolog/deep_inline].

a :- true.
b :- true.
c :- true.

test(nested_annotations_are_respected) :-
    Goal = is_parallel((a, is_uninlinable(b), is_inlinable(c))),
    deep_inline_body(Goal, [], Result),
    Result == is_parallel((true, b, true)).

:- end_tests(deep_inline).
