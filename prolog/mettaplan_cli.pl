
:- initialization(main, main).
:- use_module(reorder_repl).

main :-
    current_prolog_flag(argv, Argv),
    handle_cli(Argv).

handle_cli(["list"]) :-
    list_saved_plans.

handle_cli(["run", PlanFile]) :-
    load_plan_file(PlanFile).

handle_cli(["trace", JSONFile]) :-
    load_json_plan_file(JSONFile).

handle_cli(["help"]) :-
    usage.

handle_cli(_) :-
    usage.

usage :-
    format("Usage: mettaplan <command> [file]~n~n"),
    format("Commands:~n"),
    format("  list               List available .plan and .json files~n"),
    format("  run <file.plan>    Load and run a .plan file~n"),
    format("  trace <file.json>  Load and run a .json plan~n"),
    format("  help               Show this help message~n").
