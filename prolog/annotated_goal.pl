
:- module(annotated_goal, [
    annotated_goal/4,
    extract_goal_policies/2,
    extract_goal_vars/2
]).

:- use_module(reorder_task_static, [
    strip_annotation/3,
    strip_inline_annotation/3
]).

%! annotated_goal(+Raw, -Stripped, -Vars:list, -Policies:list)
%
%  Extracts normalized goal, its variables, and declared policies
annotated_goal(Raw, Stripped, Vars, Policies) :-
    extract_goal_policies(Raw, Policies),
    strip_all_wrappers(Raw, Stripped),
    term_variables(Stripped, Vars).

%! extract_goal_policies(+Goal, -PolicyList)
%
%  Pulls out policies from annotation wrappers
extract_goal_policies(Goal, Policies) :-
    findall(P, get_policy(Goal, P), Ps),
    sort(Ps, Policies).

get_policy(Goal, Policy) :-
    strip_annotation(Goal, _, Policy),
    Policy \== none.
get_policy(Goal, Policy) :-
    strip_inline_annotation(Goal, _, Policy),
    Policy \== none.

%! strip_all_wrappers(+Wrapped, -Stripped) is det.
%
%  Removes all annotation layers recursively
strip_all_wrappers(Goal, Final) :-
    ( strip_annotation(Goal, Inner, _),
      Goal \= Inner -> strip_all_wrappers(Inner, Final)
    ; strip_inline_annotation(Goal, Inner2, _),
      Goal \= Inner2 -> strip_all_wrappers(Inner2, Final)
    ; Final = Goal
    ).

%! extract_goal_vars(+Goal, -Vars:list)
extract_goal_vars(Goal, Vars) :-
    strip_all_wrappers(Goal, Base),
    term_variables(Base, Vars).
