
:- module(deep_inline, [
    deep_inline_body/3,
    wrap_by_policy/3
]).

:- meta_predicate deep_inline_body(0, +, -).

:- use_module(reorder_task_static, [
    strip_inline_annotation/3,
    strip_annotation/3
]).

deep_inline_body(Goal, _, Goal) :-
    strip_inline_annotation(Goal, _, uninlinable), !.

deep_inline_body(is_inlinable(Inner), Except, Out) :-
    !, deep_inline_body(Inner, Except, Out).

deep_inline_body(Wrapped, Except, WrappedOut) :-
    strip_annotation(Wrapped, Inner, Policy),
    memberchk(Policy, [parallel, sequential, reorderable, pure, shared, critical_section, delayable, guarded_by, ordered_after]),
    !,
    deep_inline_body(Inner, Except, InnerOut),
    wrap_by_policy(Policy, InnerOut, WrappedOut).

deep_inline_body((A, B), Except, (AOut, BOut)) :-
    !, deep_inline_body(A, Except, AOut),
       deep_inline_body(B, Except, BOut).

deep_inline_body((A ; B), Except, (AOut ; BOut)) :-
    !, deep_inline_body(A, Except, AOut),
       deep_inline_body(B, Except, BOut).

deep_inline_body(Goal, Except, Expanded) :-
    clause(Goal, Body),
    \+ memberchk(Goal, Except),
    !,
    deep_inline_body(Body, Except, Expanded).

deep_inline_body(Goal, _, Goal).

wrap_by_policy(parallel, G, is_parallel(G)).
wrap_by_policy(sequential, G, is_sequential(G)).
wrap_by_policy(reorderable, G, is_reorderable(G)).
wrap_by_policy(pure, G, is_pure(G)).
wrap_by_policy(shared, G, is_shared(G)).
wrap_by_policy(critical_section, G, is_critical_section(G)).
wrap_by_policy(delayable, G, is_delayable(G)).
wrap_by_policy(guarded_by, G, is_guarded_by(true, G)).
wrap_by_policy(ordered_after, G, G).
