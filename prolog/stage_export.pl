
:- module(stage_export, [
    export_stages_dot/2
]).

:- use_module(library(lists)).
:- use_module(stage_builder, [build_execution_stages/2]).

%! export_stages_dot(+Goals:list, +File:atom)
%
%  Writes a DOT file where each stage is grouped and labeled by policy
export_stages_dot(Goals, File) :-
    build_execution_stages(Goals, Stages),
    open(File, write, Out),
    format(Out, "digraph G {~n", []),
    format(Out, "  node [style=filled];~n", []),
    write_stage_nodes(Out, Stages, 0, _),
    format(Out, "}~n", []),
    close(Out).

write_stage_nodes(_, [], N, N).
write_stage_nodes(Out, [stage(Policy, Goals)|Rest], Index, Final) :-
    forall(member(G, Goals),
        ( term_string(G, GStr),
          format(Out, "  "~w" [fillcolor=lightgray, group=~w];~n", [GStr, Policy])
        )),
    write_stage_nodes(Out, Rest, Index+1, Final).
