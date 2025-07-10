
/** <module> reorder_task_static

Static analysis and task scheduling logic.
Groups tasks into ordered or parallelizable stages based on readiness.
*/


:- module(task_order_static, [
    reorder_task_groups/5,
    print_dependency_graph/2,
    group_parallel_stages/4,
    static_linear_order/4,
    goal_constraints/2
]).

:- use_module(library(lists)).
:- use_module(library(apply)).

% Count how many Vars are in the Bound list
count_bound_in_list([], _, 0).
count_bound_in_list([V|Vs], Bound, N) :-
    count_bound_in_list(Vs, Bound, N0),
    ( memberchk(V, Bound) -> N is N0 + 1 ; N = N0 ).

% === Constraint Analysis ===

goal_constraints(Goal, Vars) :-
    clause(Goal, Body),
    findall(V, constraint_var_in_body(Body, V), Raw),
    sort(Raw, Vars).

constraint_var_in_body((A,B), V) :- !, (constraint_var_in_body(A, V); constraint_var_in_body(B, V)).
constraint_var_in_body((A;B), V) :- !, (constraint_var_in_body(A, V); constraint_var_in_body(B, V)).
constraint_var_in_body((A->B), V) :- !, (constraint_var_in_body(A, V); constraint_var_in_body(B, V)).
constraint_var_in_body(\+ A, V) :- constraint_var_in_body(A, V).
constraint_var_in_body(freeze(V, _), V).
constraint_var_in_body(when(Cond, _), V) :-
    vars_in_when_cond(Cond, Vars),
    member(V, Vars).
constraint_var_in_body(_Other, _) :- fail.

vars_in_when_cond(nonvar(X), [X]).
vars_in_when_cond(ground(X), [X]).
vars_in_when_cond((C1,C2), Vars) :-
    vars_in_when_cond(C1, V1),
    vars_in_when_cond(C2, V2),
    append(V1, V2, Vars).
vars_in_when_cond((C1;C2), Vars) :-
    vars_in_when_cond(C1, V1),
    vars_in_when_cond(C2, V2),
    append(V1, V2, Vars).
vars_in_when_cond(_Other, []).

% === Static Readiness ===

required_threshold(Goal, Threshold, Vars) :-
    clause(Goal, Body),
    Body =.. [','|Parts],
    member(MustCall, Parts),
    MustCall =.. [must_have_bound, _, Vars, Threshold],
    !.

is_ready(Goal, BoundVars) :-
    ( required_threshold(Goal, Threshold, Vars),
      count_bound_in_list(Vars, BoundVars, Count),
      Count >= Threshold
    -> true
    ; goal_constraints(Goal, ConstraintVars),
      ConstraintVars \= [],
      count_bound_in_list(ConstraintVars, BoundVars, Count2),
      length(ConstraintVars, N),
      N > 0, Count2 == N
    ).

% === Sequential Reordering ===

reorder_task_groups([], _, AccGroupsRev, OrderedGroups, []) :-
    reverse(AccGroupsRev, OrderedGroups).

reorder_task_groups(Tasks, BoundVars, AccGroupsRev, OrderedGroups, Unplaced) :-
    partition_ready_goals(Tasks, BoundVars, Ready, NotReady),
    ( Ready == [] ->
        reverse(AccGroupsRev, OrderedGroups),
        Unplaced = NotReady
    ; true
    ),
    variables_in_goals(Ready, NewVars),
    append(BoundVars, NewVars, UpdatedBound),
    reorder_task_groups(NotReady, UpdatedBound, [Ready|AccGroupsRev], OrderedGroups, Unplaced).

partition_ready_goals([], _, [], []).

partition_ready_goals([G|Gs], BoundVars, [G|Ready], NotReady) :-
    is_ready(G, BoundVars), !,
    partition_ready_goals(Gs, BoundVars, Ready, NotReady).

partition_ready_goals([G|Gs], BoundVars, Ready, [G|NotReady]) :-
    partition_ready_goals(Gs, BoundVars, Ready, NotReady).

variables_in_goals(Goals, Vars) :-
    maplist(term_variables, Goals, Nested),
    append(Nested, All),
    sort(All, Vars).

% === Parallel Grouping ===

group_parallel_stages(Goals, PreBound, Stages, Leftovers):-
   group_parallel_stages_v2(Goals, PreBound, Stages, Leftovers), !.

group_parallel_stages_v2(Goals, _PreBound, Stages, []) :-
    stage_builder:build_execution_stages(Goals, StageReps),
    findall(Gs, member(stage(_, Gs), StageReps), Stages), !.

group_parallel_stages_v1(Goals, PreBound, Stages, Leftovers) :-
    expand_all_groupables(Goals, Groupables),
    maplist(arg(1), Groupables, ExpandedGoals),
    group_parallel_stages_(ExpandedGoals, PreBound, [], StagesRev, Leftovers),
    reverse(StagesRev, Stages).



group_parallel_stages_([], _, Acc, Acc, []).

group_parallel_stages_(Remaining, Bound, AccStages, FinalStages, Unplaced) :-
    partition(is_strict_sequential, Remaining, SeqGoals, OtherGoals),
    findall(G, (member(G, OtherGoals), is_ready(G, Bound)), ReadyGoals),
    ( ReadyGoals == [] ->
        append(SeqGoals, AccStages, FinalStages),
        Unplaced = OtherGoals
    ; select_independent_goals(ReadyGoals, Bound, ParallelGroup, UsedVars),
      subtract(OtherGoals, ParallelGroup, RestGoals),
      append(Bound, UsedVars, NewBound),
      append(SeqGoals, [ParallelGroup|AccStages], NewStages),
      group_parallel_stages_(RestGoals, NewBound, NewStages, FinalStages, Unplaced)
    ).

select_independent_goals(Goals, Bound, Selected, NewVars) :-
    select_independent_goals_(Goals, Bound, [], [], Selected, NewVars).

select_independent_goals_([], _, Acc, VarsAcc, Acc, VarsAcc).

select_independent_goals_([G|Gs], Bound, Acc, VarsAcc, Out, OutVars) :-
    required_threshold(G, _, Vars),
    \+ shares_var(Vars, VarsAcc),
    !,
    append(Vars, VarsAcc, VarsNext),
    select_independent_goals_(Gs, Bound, [G|Acc], VarsNext, Out, OutVars).

select_independent_goals_([_|Gs], Bound, Acc, VarsAcc, Out, OutVars) :-
    select_independent_goals_(Gs, Bound, Acc, VarsAcc, Out, OutVars).

shares_var(V1, V2) :-
    member(X, V1),
    memberchk(X, V2),
    !.

% === Cut/Side-Effect Safety ===

is_strict_sequential(Goal) :-
    goal_contains_cut(Goal), !.
is_strict_sequential(Goal) :-
    goal_is_side_effect(Goal), !.

goal_is_side_effect(Goal) :-
    functor(Goal, Name, Arity),
    side_effect_pred(Name/Arity), !.

goal_is_side_effect(Goal) :-
    predicate_property(Goal, side_effect), !.

side_effect_pred(write/1).
side_effect_pred(nl/0).
side_effect_pred(assert/1).
side_effect_pred(asserta/1).
side_effect_pred(assertz/1).
side_effect_pred(retract/1).
side_effect_pred(open/3).
side_effect_pred(close/1).
side_effect_pred(random/1).
side_effect_pred(gensym/2).

goal_contains_cut(Goal) :-
    clause(Goal, Body),
    contains_cut(Body).

contains_cut(!) :- !.
contains_cut((A,B)) :- (contains_cut(A); contains_cut(B)).
contains_cut((A;B)) :- (contains_cut(A); contains_cut(B)).
contains_cut((A->B)) :- (contains_cut(A); contains_cut(B)).
contains_cut(\+ A) :- contains_cut(A).
contains_cut(_Other) :- fail.

% === Utility: Linear Order ===

static_linear_order(TaskList, PreBindings, Ordered, Leftovers) :-
    group_parallel_stages(TaskList, PreBindings, ParallelStages, Leftovers),
    flatten(ParallelStages, Ordered).

% === Utility: Print Dependencies ===

print_dependency_graph(TaskList, PreBound) :-
    format("Dependency Graph:\n"),
    maplist(print_goal_dependencies(PreBound), TaskList),
    nl.

print_goal_dependencies(PreBound, Goal) :-
    term_variables(Goal, AllVars),
    (required_threshold(Goal, Threshold, Vars) -> true ; (Vars = [], Threshold = 0)),
    goal_constraints(Goal, ConstraintVars),
    include({PreBound}/[V]>>memberchk(V, PreBound), Vars, BoundFromPre),
    subtract(Vars, BoundFromPre, VarsFromOtherSteps),
    format("- ~w\n", [Goal]),
    format("  → Threshold: ~w from Vars: ~w\n", [Threshold, Vars]),
    format("  → Constraints: ~w\n", [ConstraintVars]),
    format("  → Pre-bound: ~w\n", [BoundFromPre]),
    format("  → Needs from others: ~w\n", [VarsFromOtherSteps]),
    nl.


%% collect_predicate_calls(+Module, -Calls)
%% Scans all visible predicate definitions and returns clause heads
collect_predicate_calls(Module, Calls) :-
    findall(Head, (
        current_predicate(Module:Name/Arity),
        functor(Template, Name, Arity),
        once(clause(Module:Template, _)),
        Head = Module:Template
    ), Calls).


%% inline_all_code(+Goal, -Expanded, +Options)
%% Replaces subcalls with their clause bodies, recursively
inline_all_code(Goal, Expanded, Options) :-
    option(max_depth(Max), Options, 3),
    option(except_for(Except), Options, []),
    inline_all_code_(Goal, Expanded, 0, Max, Except).

inline_all_code_(Goal, Goal, D, Max, _) :-
    D >= Max, !.

inline_all_code_(Goal, Goal, _, _, Except) :-
    functor(Goal, Name, Arity),
    memberchk(Name/Arity, Except), !.

inline_all_code_((A,B), (A2,B2), D, Max, Except) :- !,
    D1 is D + 1,
    inline_all_code_(A, A2, D1, Max, Except),
    inline_all_code_(B, B2, D1, Max, Except).

inline_all_code_((A;B), (A2;B2), D, Max, Except) :- !,
    D1 is D + 1,
    inline_all_code_(A, A2, D1, Max, Except),
    inline_all_code_(B, B2, D1, Max, Except).

inline_all_code_(Goal, Inlined, D, Max, Except) :-
    clause(Goal, Body),
    D1 is D + 1,
    inline_all_code_(Body, InlinedBody, D1, Max, Except),
    Inlined = InlinedBody.


inline_all_code_(Goal, TransformedGoal, D, Max, Except) :-
    D < Max,
    transform_unsafe_multithread(Goal, TransformedGoal, [multithread_safe(true), target_thread(shared_state)]),
    Goal \== TransformedGoal, !.


inline_all_code_(Goal, Goal, _, _, _).


inline_all_code_((A,B), (A2,B2), D, Max, Except) :- !,
    D1 is D + 1,
    inline_all_code_(A, A2, D1, Max, Except),
    inline_all_code_(B, B2, D1, Max, Except).

inline_all_code_((A;B), (A2;B2), D, Max, Except) :- !,
    D1 is D + 1,
    inline_all_code_(A, A2, D1, Max, Except),
    inline_all_code_(B, B2, D1, Max, Except).

inline_all_code_(Goal, Inlined, D, Max, Except) :-
    D < Max,
    functor(Goal, Name, Arity),
    \+ memberchk(Name/Arity, Except),
    findall(Body, clause(Goal, Body), Bodies),
    Bodies \= [],
    D1 is D + 1,
    maplist({D1,Max,Except}/[B,BI]>>inline_all_code_(B, BI, D1, Max, Except), Bodies, InlinedBodies),
    list_to_disjunction(InlinedBodies, Inlined).


inline_all_code_(Goal, TransformedGoal, D, Max, Except) :-
    D < Max,
    transform_unsafe_multithread(Goal, TransformedGoal, [multithread_safe(true), target_thread(shared_state)]),
    Goal \== TransformedGoal, !.


inline_all_code_(Goal, Goal, _, _, _).

%% list_to_disjunction(+List, -Disjunction)
list_to_disjunction([X], X) :- !.
list_to_disjunction([X|Xs], (X;Rest)) :-
    list_to_disjunction(Xs, Rest).


%% transform_unsafe_multithread(+GoalIn, -GoalOut, +Options)
%% Rewrites known unsafe builtins into call_in_thread/2 wrappers if rewrite option enabled
transform_unsafe_multithread(GoalIn, GoalOut, Options) :-
    option(multithread_safe(true), Options, false),
    functor(GoalIn, Name, Arity),
    unsafe_multithread_pred(Name/Arity),
    option(target_thread(ThreadID), Options, shared_state),
    GoalOut = call_in_thread(ThreadID, GoalIn), !.

transform_unsafe_multithread(Goal, Goal, _).

%% Set of known predicates that are not safe to call across threads directly
unsafe_multithread_pred(nb_setval/2).
unsafe_multithread_pred(nb_getval/2).
unsafe_multithread_pred(nb_current/2).
unsafe_multithread_pred(nb_delete/1).
unsafe_multithread_pred(b_setval/2).
unsafe_multithread_pred(b_getval/2).
unsafe_multithread_pred(b_delete/1).
unsafe_multithread_pred(recorded/3).
unsafe_multithread_pred(recorda/3).
unsafe_multithread_pred(recordz/3).
unsafe_multithread_pred(erase/1).


unsafe_multithread_pred(nb_linkval/2).
unsafe_multithread_pred(thread_self/1).
unsafe_multithread_pred(nb_once/1).
unsafe_multithread_pred(thread_setconcurrency/2).


%! expand_all_groupables(+Clauses:list, -Groupables:list) is det.
%
%  Transforms a list of `Clause` into a flat list of `groupable/1` clauses.
%  Safe disjunctions are expanded. Disjunctions with cuts are preserved.
%
%  == Example
%    ?- expand_all_groupables(
%         [(foo(X) :- a(X) ; b(X)), (bar(Y) :- baz(Y))],
%         Gs).
%    Gs = [groupable(foo(X) :- a(X)), groupable(foo(X) :- b(X)), groupable(bar(Y) :- baz(Y))].
%  ==
expand_all_groupables(Clauses, Groupables) :-
    findall(Groupable,
        ( member(Clause, Clauses),
          clause_to_groupable(Clause, Grouped),
          member(Groupable, Grouped)
        ),
        Groupables).



% Debug-enhanced version of clause_to_groupable
debug_clause_to_groupable(Clause, Groupables) :-
    clause_to_groupable(Clause, Groupables),
    format('~n🧩 Expanding: ~q~n', [Clause]),
    forall(member(G, Groupables),
        format('  → groupable: ~q~n', [G])).

% Debug-enhanced wrapper
debug_expand_all_groupables(Clauses, Groupables) :-
    format('📥 Input clauses: ~q~n', [Clauses]),
    findall(Groupable,
        ( member(Clause, Clauses),
          debug_clause_to_groupable(Clause, Grouped),
          member(Groupable, Grouped)
        ),
        Groupables),
    format('📤 Final groupables: ~q~n', [Groupables]).


% Predicate wrappers for user-declared execution control
:- meta_predicate enforce_parallel(0).
:- meta_predicate enforce_sequential(0).
:- meta_predicate enforce_reorderable(0).

% These are semantic markers and do nothing at runtime.
% They are stripped/interpreted during static scheduling and staging.
enforce_parallel(Goal) :- call(Goal).
enforce_sequential(Goal) :- call(Goal).
enforce_reorderable(Goal) :- call(Goal).

% Used to detect goal annotations during grouping/inlining
is_parallel_hint(enforce_parallel(_)).
is_sequential_hint(enforce_sequential(_)).
is_reorderable_hint(enforce_reorderable(_)).


%! strip_enforcement(+Wrapped, -Stripped, -Policy) is det.
%
%  Removes an enforce_* or is_* wrapper and returns its policy name (atom).
%  Policies: parallel, sequential, reorderable.
%
%  Nested wrappers return the innermost policy.
%
strip_enforcement(Goal, Stripped, Policy) :-
    ( Goal = is_parallel(Inner)     -> strip_enforcement(Inner, Stripped, parallel)
    ; Goal = is_sequential(Inner)   -> strip_enforcement(Inner, Stripped, sequential)
    ; Goal = is_reorderable(Inner)  -> strip_enforcement(Inner, Stripped, reorderable)
    ; Stripped = Goal, Policy = none
    ).

%! extract_enforced_goals(+Clause, -EnforcedList) is det.
%
%  Converts clause to list of (Policy, Goal) pairs.
extract_enforced_goals((Head :- Body), GoalsWithPolicy) :-
    body_to_list(Body, BodyList),
    maplist(strip_enforcement_pair, BodyList, GoalsWithPolicy0),
    include(valid_enforced_goal, GoalsWithPolicy0, GoalsWithPolicy).

strip_enforcement_pair(Goal, (Policy, Stripped)) :-
    strip_enforcement(Goal, Stripped, Policy).

valid_enforced_goal((Policy, _)) :- Policy \== none.

% Default fallback: outermost policy wins
resolve_nested_policy([P|_], P).


% Meta-predicate wrappers
:- meta_predicate is_pure(0).
:- meta_predicate is_shared(0).
:- meta_predicate is_guarded_by(0, 0).
:- meta_predicate is_ordered_after(+, +).
:- meta_predicate is_delayable(0).
:- meta_predicate is_critical_section(0).

% No-ops for now; they are intercepted during planning
is_pure(Goal) :- call(Goal).
is_shared(Goal) :- call(Goal).
is_guarded_by(Cond, Goal) :- call(Cond), call(Goal).
is_ordered_after(_, _) :- true.
is_delayable(Goal) :- call(Goal).
is_critical_section(Goal) :- call(Goal).

%! strip_annotation(+Wrapped, -Stripped, -Attr) is det.
%
%  General-purpose unwrapping of declarative annotations.
%  Attr can be: parallel, sequential, reorderable, pure, shared, guarded_by, ordered_after, delayable, critical_section
%
strip_annotation(Goal, Stripped, Attr) :-
    ( Goal = is_parallel(Inner)        -> strip_annotation(Inner, Stripped, parallel)
    ; Goal = is_sequential(Inner)      -> strip_annotation(Inner, Stripped, sequential)
    ; Goal = is_reorderable(Inner)     -> strip_annotation(Inner, Stripped, reorderable)
    ; Goal = is_pure(Inner)            -> strip_annotation(Inner, Stripped, pure)
    ; Goal = is_shared(Inner)          -> strip_annotation(Inner, Stripped, shared)
    ; Goal = is_delayable(Inner)       -> strip_annotation(Inner, Stripped, delayable)
    ; Goal = is_critical_section(Inner)-> strip_annotation(Inner, Stripped, critical_section)
    ; Goal = is_guarded_by(_, Inner)   -> strip_annotation(Inner, Stripped, guarded_by)
    ; Goal = is_ordered_after(_, _)    -> Stripped = Goal, Attr = ordered_after
    ; Stripped = Goal, Attr = none
    ).

strip_annotation_pair(Goal, (Attr, Stripped)) :-
    strip_annotation(Goal, Stripped, Attr).

% Converts body goals to annotated policy list
%! extract_annotations(+Clause, -ListOf(Pair)) is det.
extract_annotations((Head :- Body), AnnotatedGoals) :-
    body_to_list(Body, BodyList),
    maplist(strip_annotation_pair, BodyList, AnnotatedGoals0),
    include(valid_annotation, AnnotatedGoals0, AnnotatedGoals).

valid_annotation((Attr, _)) :- Attr \== none.


annotate_goals([], []).
annotate_goals([G|Gs], [(Pol,G2)|Rest]) :-
    annotated_goal:annotated_goal(G, G2, _, Ps),
    ( Ps = [Pol|_] -> true ; Pol = none ),
    annotate_goals(Gs, Rest).
    strip_annotation(G, G2, Policy),
    annotate_goals(Gs, Rest).

%! group_parallel_stages_policy(+Annotated:list, +PreBound:list, +Acc:list, -Stages:list, -Leftovers:list)
group_parallel_stages_policy([], _, Acc, Acc, []).
group_parallel_stages_policy([(sequential, G)|T], Bound, Acc, Final, Left) :-
    !, group_parallel_stages_policy(T, Bound, [[G]|Acc], Final, Left).
group_parallel_stages_policy([(critical_section, G)|T], Bound, Acc, Final, Left) :-
    !, group_parallel_stages_policy(T, Bound, [[G]|Acc], Final, Left).
group_parallel_stages_policy([(Policy, G)|T], Bound, [Stage|Rest], Final, Left) :-
    (var(Stage) -> Stage = [] ; true),
    group_parallel_stages_policy(T, Bound, [[G|Stage]|Rest], Final, Left).


% Meta-predicate wrappers for inlining
:- meta_predicate is_inlinable(0).
:- meta_predicate is_uninlinable(0).

is_inlinable(Goal) :- call(Goal).
is_uninlinable(Goal) :- call(Goal).

%! strip_inline_annotation(+Wrapped, -Stripped, -InlinePolicy) is det.
%
%  Separates inlinable/uninlinable declarations
strip_inline_annotation(Goal, Stripped, inlinable) :-
    Goal = is_inlinable(Inner), !,
    strip_inline_annotation(Inner, Stripped, _).
strip_inline_annotation(Goal, Stripped, uninlinable) :-
    Goal = is_uninlinable(Inner), !,
    strip_inline_annotation(Inner, Stripped, _).
strip_inline_annotation(Goal, Goal, none).


%! deep_inline_body(+Goal, +Except, -Expanded) is det.
%
%  Recursively inlines goals while respecting annotations like is_uninlinable/1,
%  is_inlinable/1, and other structural wrappers (e.g., is_parallel/1).
%  Expands compound bodies segment-by-segment, flattening where allowed.
%
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

%! wrap_by_policy(+Policy, +Goal, -Wrapped) is det.
wrap_by_policy(parallel, G, is_parallel(G)).
wrap_by_policy(sequential, G, is_sequential(G)).
wrap_by_policy(reorderable, G, is_reorderable(G)).
wrap_by_policy(pure, G, is_pure(G)).
wrap_by_policy(shared, G, is_shared(G)).
wrap_by_policy(critical_section, G, is_critical_section(G)).
wrap_by_policy(delayable, G, is_delayable(G)).
wrap_by_policy(guarded_by, G, is_guarded_by(true, G)).
wrap_by_policy(ordered_after, G, G).  % No structural wrapping

%! inline_all_code(+Clauses, +Except, -InlinedClauses)
inline_all_code([], _, []).
inline_all_code([Clause|Rest], Except, [NewClause|Out]) :-
    Clause = (Head :- Body),
    deep_inline:deep_inline_body(Body, Except, InlinedBody),
    NewClause = (Head :- InlinedBody),
    inline_all_code(Rest, Except, Out).
    Clause = (Head :- Body),
    deep_inline_body(Body, Except, InlinedBody),
    NewClause = (Head :- InlinedBody),
    inline_all_code(Rest, Except, Out).
