
:- module(dag_export_shell, [
    export_goal_dag_png/2
]).

:- use_module(dag_export, [export_goal_dag_dot/2]).
:- use_module(library(process)).

%! export_goal_dag_png(+Goals:list, +BaseName:atom)
%
%  Exports DAG to BaseName.dot and BaseName.png using Graphviz
export_goal_dag_png(Goals, Base) :-
    atom_concat(Base, '.dot', DotFile),
    atom_concat(Base, '.png', PngFile),
    export_goal_dag_dot(Goals, DotFile),
    process_create(path(dot), ['-Tpng', DotFile, '-o', PngFile], []).
