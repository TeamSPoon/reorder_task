Prolog Core Modules:
- deep_inline.pl: Recursive inliner that exposes real control/IO semantics
- annotated_goal.pl: Extracts annotations like parallel/shared/sequential
- goal_dag.pl: Builds dependency-aware execution graph (DAG)
- stage_builder.pl: Groups DAG nodes into stage(Policy, [Goals])
- policy_executor.pl: Executes stage plans, honoring sync/threading
- trace_executor.pl: Logs [start|done|fail] events with timestamps
- dag_export.pl / stage_export.pl / *_shell.pl: Emit DOT/PNG graphs for visualization
- llm_annotator.pl: Heuristically suggests annotations based on predicate name

### Proposed Follow-up Tasks (May 2025)
- Clean up syntax in `reorder_sync_pool.pl` around `use_module/2`; extra
  bracket lines break compilation.
- Improve `test_trace_executor_timestamps.pl` by asserting timestamps are
  monotonic rather than just present.
