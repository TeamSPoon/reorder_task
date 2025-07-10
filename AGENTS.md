# AGENTS.md

## 🤖 Instructions for AI Agents (Codex, GPT, etc.)

This file provides guidance for AI systems assisting with development or code generation in this repository. Please follow the conventions below when interacting with, editing, or extending the project.

---

## ✅ GENERAL PRINCIPLES

- This project combines **Prolog**, **Python**, and optionally **MeTTa** to implement a high-performance, multi-threaded task execution and planning system.
- All logical execution and scheduling rules are defined in **Prolog**.
- **Python** is used only for integration, tooling, or MeTTa conversions — not for task logic itself.
- The main virtual environment setup script is [`from_venv.sh`](from_venv.sh).
- Pipelines may include external MeTTa `.metta` files, processed via helper scripts or Prolog wrappers.


### 🛠️ System Setup for Prolog Support

Before attempting to run any code that depends on SWI-Prolog modules or multi-threaded execution:

Run the following manually to ensure all required Prolog packages are installed:

```bash
sudo apt update -y && sudo apt install -y swi-prolog-full swi-prolog-java swi-prolog-test
```

This enables support for:

* `engines`, `threads`, `atts`, `nb_rbtrees`, and other concurrency features
* Full `library(test)` and test runner support
* Java integration via `swi-prolog-java` if needed

> ⚠️ **Note**: This process can take several minutes and installs a large set of packages.
> For that reason, we intentionally **left it out of the default local VM or venv setup scripts** (like `from_venv.sh`), to avoid slowing down bootstrapping.

Agents (and users) should run this step **once manually** on any new machine or container where Prolog logic will be used.

Many tasks agents do will not require it though.

---

## ✅ DO

- ✅ Place all core logic modules inside `prolog/`
- ✅ Use `test/` for any new test cases
- ✅ Modify `python/` only for tooling, MeTTa integration, or data processing
- ✅ Assume `from_venv.sh` will be used for all local setup
- ✅ Use `example/` for demos or pipeline samples
- ✅ Keep logic predicates deterministic where possible (use `once/1` instead of `!`)

---

## 🚫 DO NOT

- ❌ Delete or rename `pack.pl`, `.venv/`, or `run_pipeline.sh`
- ❌ Add runtime logic inside `python/` — all execution control should remain in Prolog
- ❌ Introduce new Prolog modules without updating `run_tests.pl` or `mettaplan_cli.pl`
- ❌ Assume `prolog/` and `test/` share state — keep tests isolated

---

## 🔁 FILE ROLE GUIDE

| Folder/File            | Role |
|------------------------|------|
| `from_venv.sh`         | Sets up Python venv, installs deps, activates Prolog pack link |
| `prolog/`              | Core logic: executors, planners, stage building, synchronization |
| `python/`              | Tools: metta-to-prolog converters, listeners, integration glue |
| `test/`                | Prolog tests for all major logic modules |
| `example/greedy_example/` | Demo pipeline with MeTTa, listener, Prolog annotations |
| `run_pipeline.sh`      | Top-level orchestrated execution pipeline |
| `trace_viewer.html`    | Optional frontend for viewing execution trace |

---

## 🧪 HOW TO RUN TESTS

```prolog
?- [prolog/run_tests], run_tests.
````

For a single module test:

```prolog
?- [test/test_executor_inline], run_tests.
```

---

## ⚙️ SHELL AND ENVIRONMENT EXPECTATIONS

* Always run this project using `source ./from_venv.sh` or `./from_venv.sh [cmd]`.
* Python code expects packages to be installed via `python/requirements.txt`.
* `.env` is automatically loaded by `from_venv.sh` if it exists.

---

## 🧠 PROMPTING HINTS

* When writing new logic, prefer clear, composable predicates with small arity.
* Use `:- module(...).` headers in new Prolog files.
* Use `:- begin_tests(...)` and `:- end_tests(...)` to wrap all test files.
* Reuse `run_dynamic_parallel_ready/3` and `run_static_parallel/3` for testing scheduling modes.

Absolutely — here's an updated section you can append to the bottom of your `AGENTS.md` to guide agents (and humans) in using this file effectively across the repo:

---

### 🗂️ Using `AGENTS.md` Files Within Subdirectories

In addition to this root-level `AGENTS.md`, you may find (or create) **`AGENTS.md` files inside subdirectories** such as `prolog/`, `example/`, or `test/`.

These are encouraged and serve an important purpose:

---

### ✅ What Local `AGENTS.md` Files Are For

* 🧠 **Tracking what the agent just did** in that folder
* 💡 **Proposing tasks** or notes for other agents (or future you)
* 🔍 **Recording things the agent learned** about how this part of the code works
* ✍️ **Leaving prompts, intentions, or warnings** related to specific logic

---

### 📌 Best Practices

* When working in a subfolder, consider editing or adding a local `AGENTS.md` to:

  * Summarize what changes were made
  * Add context for follow-up logic or test generation
  * Document decisions (e.g., why a test was skipped or a refactor avoided)
* Include **brief summaries from `AGENTS.md`** in your **commit messages** when relevant. This makes it easier for others to trace the intention behind a change.

---

### 🧠 Example Use

Inside `prolog/AGENTS.md`:

```markdown
## April 2025 — Planning Agent

- Added `dag_export_shell.pl` to support CLI-friendly DAG output.
- Avoided modifying `goal_dag.pl` directly due to shared dependencies with `stage_builder.pl`.
- Future agents should extract the goal labeling logic into a separate module.
```

Inside `test/AGENTS.md`:

```markdown
## March 2025 — LLM Annotator Agent

- Created `test_llm_annotator.pl` to test annotation pipeline.
- Found out that `deep_inline.pl` expects `annotated_goal/2` to be already preprocessed.
- Need a test harness that mocks LLM output for more deterministic tests.
```

---

By maintaining structured `AGENTS.md` notes per directory, agents (and developers) can work together more effectively — even asynchronously or with different expertise.

Be verbose, be clear, and keep the next agent in mind.

---

Thank you, agent. Follow these guidelines to contribute safe, clean, and composable logic to this repository.

### Actual Use
- AGENTS.md: (This file) Ammendign beollow here:
- Need to keep README.md up to date as Project overview
- README-ANOTATATION.
- WALKTHROUGH.md: Usage tutorial for CLI and REPL modes
- README-ANNOTATOR.md needs updated
- We should get rid of Makefile

### Proposed Follow-up Tasks (May 2025)
- Fix typos in this section ("Ammendign beollow" and "README-ANOTATATION").
- Remove stray bracket lines in `prolog/reorder_sync_pool.pl` after the
  `use_module` directive.
- Clarify that the Makefile is deprecated in `README.md` so documentation
  matches project intentions.
- Extend `test/test_trace_executor_timestamps.pl` to verify timestamp order
  increases.

