Prolog Core Modules:
- deep_inline.pl: Recursive inliner that exposes real control/IO semantics
- annotated_goal.pl: Extracts annotations like parallel/shared/sequential
- goal_dag.pl: Builds dependency-aware execution graph (DAG)
- stage_builder.pl: Groups DAG nodes into stage(Policy, [Goals])
- policy_executor.pl: Executes stage plans, honoring sync/threading
- trace_executor.pl: Logs [start|done|fail] events with timestamps
- dag_export.pl / stage_export.pl / *_shell.pl: Emit DOT/PNG graphs for visualization
- llm_annotator.pl: Heuristically suggests annotations based on predicate name
