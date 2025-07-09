
:- module(goal_dag, [
    build_goal_dag/3
]).

:- use_module(annotated_goal, [
    annotated_goal/4
]).

%! build_goal_dag(+Goals, -Dag:list, -VarsUsed:list)
%
%  Builds a DAG: list of nodes (Goal, Vars, Policies, PredecessorIndices)
build_goal_dag(Goals, Dag, AllVars) :-
    maplist(annotated_goal_entry, Goals, RawNodes),
    assign_indices(RawNodes, Indexed),
    findall(V, member((_, V, _, _), Indexed), NestedVars),
    flatten(NestedVars, FlatVars),
    sort(FlatVars, AllVars),
    maplist(attach_dependencies(Indexed), Indexed, Dag).

annotated_goal_entry(Goal, (_, Vars, Policies, _)) :-
    annotated_goal:annotated_goal(Goal, _, Vars, Policies).

assign_indices(Entries, Indexed) :-
    findall((Goal, Vars, Policies, [], I),
        nth0(I, Entries, (Goal, Vars, Policies, [])),
        Indexed).

attach_dependencies(All, (Goal, Vars, Policies, _, I), (Goal, Vars, Policies, Deps)) :-
    findall(J,
        ( member((OtherGoal, OV, _, _, J), All),
          J \= I,
          shares_var(Vars, OV)
        ),
        Deps).

shares_var(V1, V2) :-
    member(X, V1),
    member(X, V2), !.
