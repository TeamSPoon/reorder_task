# Task Executor and Sync System (Prolog + Python + MeTTa)

This system provides a multi-threaded, constraint-aware task execution and scheduling engine built with **SWI-Prolog**, extended by **Python** tools and **MeTTa** integration for flexible pipeline-based AI planning.

## ✅ Quick Start

1. **Activate the development environment**:
```bash
   ./from_venv.sh
````

Or, if you want to keep your shell:

```bash
source ./from_venv.sh
```

2. **Run the full pipeline** (example):

   ```bash
   ./run_pipeline.sh
   ```

3. **Load core modules in Prolog**:

   ```prolog
   ?- [prolog/reorder_executor, prolog/reorder_task_static, prolog/threaded_attvar].
   ```

4. **Run tests**:

   ```prolog
   ?- [prolog/run_tests], run_tests.
   ```

---

## 🔗 Key Files and Modules

### 🧠 Core Logic

* [`prolog/reorder_task_static.pl`](prolog/reorder_task_static.pl) – Task reordering
* [`prolog/reorder_executor.pl`](prolog/reorder_executor.pl) – Dynamic and parallel execution engine
* [`prolog/threaded_attvar.pl`](prolog/threaded_attvar.pl) – Attributed variables for cross-thread sync
* [`prolog/goal_dag.pl`](prolog/goal_dag.pl) – DAG-based task modeling
* [`prolog/run_pipeline.pl`](prolog/run_pipeline.pl) – Top-level composition runner

### 🧪 Tests

* [`test/`](test/) – Full test suite for executors, annotations, DAGs, and sync logic

### 🐍 Python Integration

* [`python/`](python/) – Python modules for metta ↔ prolog conversion and automation
* [`python/metta_to_prolog.py`](python/metta_to_prolog.py) – DSL converter
* [`python/requirements.txt`](python/requirements.txt) – Python dependencies

### 📄 Examples

* [`example/greedy_example/`](example/greedy_example/) – End-to-end GreedyChess planning demo

### 🧰 Dev Utilities

* [`from_venv.sh`](from_venv.sh) – Smart virtualenv setup (detects shell, installs deps, symlinks pack)
* [`Makefile`](Makefile) – Common targets for building, running, and testing
* [`run_pipeline.sh`](run_pipeline.sh) – Launch pipeline logic end-to-end

---

## 📘 Documentation

* [`WALKTHROUGH.md`](WALKTHROUGH.md) – Full usage walk-through and test plan
* [`README-ANNOTATOR.md`](README-ANNOTATOR.md) – How annotators and goal analysis work
* [`TODO.md`](TODO.md) – Development roadmap and backlog

---

## 🧠 Supported Execution Strategies

| Mode                           | Threads | Dynamic | Shared Vars | Best Use Case                       |
| ------------------------------ | ------- | ------- | ----------- | ----------------------------------- |
| `run_static_sequential/3`      | ❌       | ❌       | ❌           | Deterministic, step-by-step         |
| `run_static_parallel/3`        | ✅       | ❌       | ⚠           | Independent tasks, no var sharing   |
| `run_dynamic_ready_loop/3`     | ✅       | ✅       | ✅           | Tasks become ready at runtime       |
| `run_dynamic_parallel_ready/3` | ✅       | ✅       | ✅           | Parallel with full readiness + sync |

---

## 💡 Notes

* Requires **SWI-Prolog ≥ 8.2**
* Automatically symlinks the local folder as a SWI-Prolog pack
* Compatible with `.env` for configuration
* `.venv/` is created automatically by `from_venv.sh` if missing

---

For detailed developer usage, see [WALKTHROUGH.md](WALKTHROUGH.md).
See [AGENTS.md](AGENTS.md) for how agents (human or AI) coordinate work and document reasoning.

