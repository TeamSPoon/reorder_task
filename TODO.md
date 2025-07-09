# ✅ TODOs for reorder_task

## 🧠 Inferred From Summaries and Structure

### Prolog (core modules)
- [ ] Implement `run_metta_program/1` or formally retire it in favor of `main_loop/0`
- [ ] Add `test/test_llm_annotator.pl` to test heuristic annotation logic
- [ ] Add more trace_executor tests (timestamp verification, edge failures)
- [ ] Ensure DAG traversal has a testable public entry (e.g., node stats or ordering)

### Python
- [ ] Add `requirements.txt` for OpenAI and future tooling
- [ ] Finalize `setup.py` entry_points → `metta2prolog` CLI command
- [ ] Add `python/__main__.py` for `python -m reorder_task` compatibility

### CLI / Root
- [ ] Add `CHANGELOG.md` to document phase milestones
- [ ] Add `VERSION` or tag strategy
- [ ] Make `run_pipeline.sh` emit human-readable summaries

### GreedyChess Example
- [ ] Add `play.sh` or simplified demo entry for users
- [ ] Add introspection to check if `main_loop/0` exists

