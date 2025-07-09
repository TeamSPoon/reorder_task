
# 🧾 CHANGELOG

## v0.1.0 - Initial Toolchain Release

- ✅ Added `deep_inline.pl` for goal body expansion
- ✅ Introduced `annotated_goal.pl` to extract concurrency annotations
- ✅ Created `goal_dag.pl` for variable-aware dependency graphs
- ✅ Built `stage_builder.pl` to construct execution stages
- ✅ Added `policy_executor.pl` and `trace_executor.pl` with timestamped trace logging
- ✅ CLI wrapper `mettaplan` and `mettaplan_cli.pl`
- ✅ `llm_annotator.pl` with name-based policy inference
- ✅ `run_metta_program/1` as symbolic entrypoint runner
- ✅ Full pipeline from `.metta` → `.pl` → DAG → executed stages
- ✅ `greedy_pipeline.sh` for GreedyChess example
- ✅ Added test coverage for trace executor, DAG planner, LLM annotator
- ✅ Visual export via DOT/PNG: DAG + stage graphs
