
:- module(policy_executor, [
    run_stages/1
]).

:- use_module(library(thread)).
:- use_module(library(apply)).

%! run_stages(+Stages:list(stage(Policy, [Goal,...])))
%
%  Executes a list of stage(Policy, Goals) with appropriate strategy.
run_stages([]).
run_stages([stage(Policy, Goals)|Rest]) :-
    run_stage(Policy, Goals),
    run_stages(Rest).

%! run_stage(+Policy, +Goals)
run_stage(parallel, Goals) :-
    maplist(thread_create_detached, Goals).
run_stage(sequential, Goals) :-
    maplist(call, Goals).
run_stage(critical_section, Goals) :-
    with_mutex(global_lock, maplist(call, Goals)).
run_stage(shared, Goals) :-
    maplist(call, Goals).
run_stage(pure, Goals) :-
    maplist(call, Goals).
run_stage(delayable, Goals) :-
    maplist(call, Goals).
run_stage(reorderable, Goals) :-
    maplist(call, Goals).
run_stage(none, Goals) :-
    maplist(call, Goals).

thread_create_detached(Goal) :-
    thread_create(call(Goal), _, [detached(true)]).
