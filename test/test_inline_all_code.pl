
:- use_module(reorder_task_static).
:- use_module(library(plunit)).

:- begin_tests(inline_all_code).

foo(a) :- bar(a).
foo(a) :- baz(a).
bar(a).
baz(a).

test(inline_single_clause) :-
    inline_all_code(bar(a), Result, [max_depth(2)]),
    assertion(Result == true).

test(inline_multi_clause) :-
    inline_all_code(foo(a), Expanded, [max_depth(4)]),
    assertion(Expanded == (bar(a);baz(a))).

test(inline_with_except) :-
    inline_all_code(foo(a), Result, [max_depth(4), except_for([bar/1])]),
    assertion(Result == (bar(a);baz(a))).

:- end_tests(inline_all_code).
