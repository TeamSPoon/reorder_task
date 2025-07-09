
:- module(trace_executor, [
    run_traced_stages/1,
    run_traced_stages_with_logging/1
]).

:- use_module(policy_executor, [run_stages/1]).
:- use_module(library(thread)).
:- dynamic trace_event/1.

%! run_traced_stages(+Stages)
run_traced_stages([]).
run_traced_stages([stage(Policy, Goals)|Rest]) :-
    trace_stage(Policy, Goals),
    run_traced_stages(Rest).

run_traced_stages_with_logging([]).
run_traced_stages_with_logging([stage(Policy, Goals)|Rest]) :-
    length(Goals, N),
    log_event(stage_start(Policy, N)),
    trace_stage(Policy, Goals),
    log_event(stage_done(Policy)),
    run_traced_stages_with_logging(Rest).

trace_stage(parallel, Goals) :-
    maplist(trace_threaded, Goals).
trace_stage(sequential, Goals) :-
    maplist(trace_goal(main), Goals).
trace_stage(critical_section, Goals) :-
    with_mutex(global_lock, maplist(trace_goal(locked), Goals)).
trace_stage(_, Goals) :-
    maplist(trace_goal(main), Goals).

trace_threaded(Goal) :-
    thread_create(trace_goal(thread), Goal, [detached(true)]).

trace_goal(Source, Goal) :-
    thread_self(Self),
    thread_property(Self, id(Id)),
    log_event(start(Source-Id, Goal)),
    catch(call(Goal), E, log_event(fail(Source-Id, Goal, E))),
    log_event(done(Source-Id, Goal)).


log_event(Event) :-
    assertz(trace_event(Event)),
    format("~w~n", [Event]).
%! dump_trace_log(+File)
dump_trace_log(File) :-
    findall(E, trace_event(E), Events),
    open(File, write, Stream),
    json_write(Stream, Events, [width(0)]),
    close(Stream).
