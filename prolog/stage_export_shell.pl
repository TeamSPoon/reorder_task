
:- module(stage_export_shell, [
    export_stages_png/2
]).

:- use_module(stage_export, [export_stages_dot/2]).
:- use_module(library(process)).

%! export_stages_png(+Goals:list, +BaseName:atom)
%
%  Exports execution stages as a PNG using Graphviz
export_stages_png(Goals, Base) :-
    atom_concat(Base, '.dot', DotFile),
    atom_concat(Base, '.png', PngFile),
    export_stages_dot(Goals, DotFile),
    process_create(path(dot), ['-Tpng', DotFile, '-o', PngFile], []).
