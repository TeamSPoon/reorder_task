
:- module(dag_export, [
    export_goal_dag_dot/2
]).

:- use_module(goal_dag, [build_goal_dag/3]).
:- use_module(library(lists)).

%! export_goal_dag_dot(+RawGoals, +File)
%
%  Exports the DAG of annotated goals as a DOT graph.
export_goal_dag_dot(Goals, File) :-
    build_goal_dag(Goals, Dag, _),
    open(File, write, Out),
    format(Out, "digraph G {~n", []),
    maplist(write_node(Out), Dag),
    write_edges(Out, Dag, 0),
    format(Out, "}~n", []),
    close(Out).


policy_color(parallel, "green").
policy_color(sequential, "blue").
policy_color(shared, "orange").
policy_color(critical_section, "red").
policy_color(pure, "gray").
policy_color(reorderable, "purple").
policy_color(delayable, "cyan").
policy_color(guarded_by, "brown").
policy_color(ordered_after, "black").
policy_color(none, "lightgray").

write_node(Out, (Goal, _, Policies, _)) :-
    term_string(Goal, GStr),
    ( Policies = [P|_] -> policy_color(P, Color) ; Color = "black" ),
    atomic_list_concat(Policies, ",", PolStr),
    format(Out, "  "~w" [label="~w\n~w", style=filled, fillcolor=~q];~n",
           [GStr, GStr, PolStr, Color]).

    term_string(Goal, GStr),
    atomic_list_concat(Policies, ",", PolStr),
    format(Out, "  "~w" [label="~w\n~w"];~n", [GStr, GStr, PolStr]).

write_edges(_, [], _).
write_edges(Out, [(Goal, _, _, Preds)|Rest], Index) :-
    term_string(Goal, GStr),
    forall(member(PredIdx, Preds),
        (nth0(PredIdx, Rest, (PGoal, _, _, _)),
         term_string(PGoal, PStr),
         format(Out, "  "~w" -> "~w";~n", [PStr, GStr]))),
    write_edges(Out, Rest, Index+1).
