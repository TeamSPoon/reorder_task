
:- initialization(run_pipeline, main).

:- use_module(stage_builder).
:- use_module(trace_executor).
:- use_module(dag_export_shell).
:- use_module(stage_export_shell).

goals([
    is_parallel(a),
    is_sequential(b),
    c
]).

a :- format("a logic\n"), sleep(1).
b :- format("b logic\n"), sleep(2).
c :- format("c logic\n"), sleep(1).

run_pipeline :-
    goals(Goals),
    format("\n🧠 Building execution plan...\n"),
    stage_builder:build_execution_stages(Goals, Stages),

    format("\n🚀 Running with trace...\n"),
    trace_executor:run_traced_stages_with_logging(Stages),
    trace_executor:dump_trace_log('reorder_task/trace_log.json'),

    format("\n📦 Exporting DAG and stages...\n"),
    dag_export_shell:export_goal_dag_png(Goals, 'reorder_task/dag'),
    stage_export_shell:export_stages_png(Goals, 'reorder_task/stages'),

    format("\n✅ Done. Open trace_viewer.html to inspect results.\n").
