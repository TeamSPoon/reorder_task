
:- module(stage_builder, [
    build_execution_stages/2
]).

:- use_module(goal_dag, [build_goal_dag/3]).

%! build_execution_stages(+RawGoals, -Stages:list(stage(Policy, [Goal,...])))
%
%  Converts a list of raw goals into staged execution groups based on shared variables and declared annotations.
build_execution_stages(Goals, Stages) :-
    build_goal_dag(Goals, Dag, _),
    stageify_dag(Dag, [], StagesRev),
    reverse(StagesRev, Stages).

%! stageify_dag(+Remaining:list, +AccStages, -FinalStages)
stageify_dag([], Stages, Stages).
stageify_dag(Dag, Acc, Stages) :-
    findall((Goal, Pols),
        ( member((Goal, _, Pols, Preds), Dag),
          Preds = [],
          \+ memberchk(stage(_, Goals), Acc),
          \+ member(Goal, Goals)
        ), RunnableGoals),

    % Split by policies and form new stage
    select_stage_subset(RunnableGoals, SelectedGoals, Policy),
    exclude_member(SelectedGoals, Dag, NewDag0),
    decrement_dependencies(NewDag0, SelectedGoals, NewDag),
    stageify_dag(NewDag, [stage(Policy, SelectedGoals)|Acc], Stages).

select_stage_subset(Goals, Selected, Policy) :-
    member((_, Policies), Goals),
    member(Policy, Policies), !,
    findall(G, member((G, Ps), Goals), SelectedGoals),
    include(has_policy(Policy), SelectedGoals, Selected),
    Selected \= [].

has_policy(Policy, (Goal, Policies)) :-
    memberchk(Policy, Policies).

exclude_member(Selected, Dag, NewDag) :-
    findall(Node,
        ( member(Node@(Goal,_,_,_), Dag),
          \+ memberchk((Goal,_), Selected)
        ),
        NewDag).

decrement_dependencies([], _, []).
decrement_dependencies([(G, V, P, Deps)|T], Completed, [(G, V, P, NewDeps)|Out]) :-
    findall(D,
        ( member(D, Deps),
          \+ memberchk(D, Completed)
        ),
        NewDeps),
    decrement_dependencies(T, Completed, Out).
