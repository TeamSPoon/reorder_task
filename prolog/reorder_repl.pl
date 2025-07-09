
:- module(reorder_repl, [plan/1]).

:- use_module(stage_builder).
:- use_module(trace_executor).
:- use_module(dag_export_shell).
:- use_module(stage_export_shell).

plan(Goals) :-
    format("~n🧠 Planning execution stages...~n"),
    stage_builder:build_execution_stages(Goals, Stages),
    print_stages(Stages),
    format("~n🚀 Running traced execution...~n"),
    trace_executor:run_traced_stages_with_logging(Stages),
    trace_executor:dump_trace_log('reorder_task/trace_log.json'),

    format("~n🎨 Exporting DAG and Stage Graphs...~n"),
    dag_export_shell:export_goal_dag_png(Goals, 'reorder_task/dag'),
    stage_export_shell:export_stages_png(Goals, 'reorder_task/stages'),

    format("~n✅ Done. Open trace_viewer.html to inspect results.~n").

print_stages([]).
print_stages([stage(Policy, Goals)|Rest]) :-
    format("Stage (~w):~n", [Policy]),
    forall(member(G, Goals), format("  - ~q~n", [G])),
    print_stages(Rest).


:- use_module(library(readutil)).

interactive_plan(Goals) :-
    format("~n🧠 Planning execution stages...~n"),
    stage_builder:build_execution_stages(Goals, Stages),
    print_stages_fancy(Stages),
    format("~n▶ Run stages now? [y/N]: "),
    read_line_to_string(user_input, Reply),
    ( Reply = "y" ->
        plan(Goals)
    ; format("❌ Aborted.~n")
    ).

print_stages_fancy([]).
print_stages_fancy([stage(Policy, Goals)|Rest]) :-
    ansi_format([fg(green)], "Stage (~w):~n", [Policy]),
    forall(member(G, Goals),
        ansi_format([fg(cyan)], "  • ~q~n", [G])),
    print_stages_fancy(Rest).


% Named plan presets
plan_name(basic) :-
    interactive_plan([is_parallel(a), is_sequential(b), c]).

plan_name(parallel_test) :-
    interactive_plan([is_parallel(foo(X)), is_parallel(bar(X)), is_sequential(baz)]).

plan_name(sync_example) :-
    interactive_plan([is_shared(read_db(ID)), is_critical_section(update_db(ID))]).

% Sample definitions to support the presets
foo(X) :- sleep(1), format("foo ~w~n", [X]).
bar(X) :- sleep(1), format("bar ~w~n", [X]).
baz :- sleep(1), format("baz~n").
read_db(ID) :- sleep(1), format("reading ~w~n", [ID]).
update_db(ID) :- sleep(2), format("updating ~w~n", [ID]).


list_plans :-
    format("Available named plans:~n"),
    format("  - basic~n"),
    format("  - parallel_test~n"),
    format("  - sync_example~n").


% load_plan_file(+File)
% Expects file to define: plan_goals(Goals).
load_plan_file(File) :-
    ensure_loaded(File),
    plan_goals(Goals),
    interactive_plan(Goals).


% save_plan_file(+Goals:list, +File:atom)
save_plan_file(Goals, File) :-
    open(File, write, Stream),
    format(Stream, '%% Auto-generated plan file~n', []),
    format(Stream, 'plan_goals(~q).~n', [Goals]),
    close(Stream),
    format("💾 Plan saved to ~w~n", [File]).


:- use_module(library(http/json)).
:- use_module(library(readutil)).

% load_json_plan_file(+File)
load_json_plan_file(File) :-
    open(File, read, In),
    json_read_dict(In, Dict),
    close(In),
    dict_to_goals(Dict.goals, Goals),
    interactive_plan(Goals).

dict_to_goals([], []).
dict_to_goals([G|T], [Wrapped|More]) :-
    parse_json_goal(G.policy, G.goal, Wrapped),
    dict_to_goals(T, More).

parse_json_goal("none", GStr, Goal) :- term_string(Goal, GStr).
parse_json_goal(Policy, GStr, Wrapped) :-
    term_string(Goal, GStr),
    atom_concat("is_", Policy, Functor),
    Wrapped =.. [Functor, Goal].


% save_plan_file_json(+Goals:list, +File:atom)
save_plan_file_json(Goals, File) :-
    maplist(goal_to_json_entry, Goals, Entries),
    JSON = _{goals: Entries},
    open(File, write, Stream),
    json_write_dict(Stream, JSON, [width(0)]),
    close(Stream),
    format("💾 JSON plan saved to ~w~n", [File]).

goal_to_json_entry(Goal, _{policy:Policy, goal:GStr}) :-
    ( Goal =.. [Wrap, Inner], atom_prefix(Wrap, is_), Inner \= true ->
        atom_string(Policy0, Wrap),
        atom_concat("is_", Policy, Policy0),
        term_string(Inner, GStr)
    ; Policy = "none",
      term_string(Goal, GStr)
    ).


list_saved_plans :-
    format("📄 Available .plan files:~n"),
    expand_file_name('*.plan', PlanFiles),
    forall(member(F, PlanFiles), format("  - ~w~n", [F])),

    format("📄 Available .json plans:~n"),
    expand_file_name('*.json', JsonFiles),
    forall(member(F, JsonFiles), format("  - ~w~n", [F])).
