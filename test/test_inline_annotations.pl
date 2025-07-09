
:- begin_tests(inline_annotations).
:- [reorder_task:prolog/reorder_task_static].

% Inlinable clause definitions
a :- format("a\n").
b :- format("b\n").

% Mixed clause with annotations
clause_to_inline((foo :- is_inlinable((a, b)))).
clause_to_inline((bar :- is_uninlinable((a, b)))).

test(inline_respects_is_uninlinable) :-
    clause_to_inline((bar :- is_uninlinable((a, b)))),
    inline_all_code([(bar :- is_uninlinable((a, b)))], [], [Inlined]),
    assertion(Inlined == (bar :- (a, b))).

test(inline_respects_is_inlinable) :-
    assert((a :- true)), assert((b :- true)),
    clause_to_inline((foo :- is_inlinable((a, b)))),
    inline_all_code([(foo :- is_inlinable((a, b)))], [], [Inlined]),
    Inlined = (foo :- Body),
    Body == (true, true).

:- end_tests(inline_annotations).
