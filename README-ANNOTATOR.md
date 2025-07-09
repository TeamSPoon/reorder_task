
# 🧠 MettaPlan: Declarative Task Scheduling & Execution

MettaPlan is a Prolog-based pipeline that lets you define, schedule, and execute tasks using declarative annotations like `is_parallel`, `is_sequential`, `is_shared`, etc. It supports tracing, visualization, and a full CLI toolchain.

---

## 🚀 Features

- ✅ Parallel & sequential task staging
- ✅ Annotation-driven scheduling
- ✅ Threaded execution with variable-aware sync
- ✅ Execution trace logging & HTML viewer
- ✅ DAG & stage graph exports (Graphviz)
- ✅ Named plans, `.plan` and `.json` support
- ✅ Interactive REPL and CLI

---

## 📦 Installation

```bash
make install        # Installs mettaplan CLI to /usr/local/bin
```

To remove:

```bash
make uninstall
```

---

## 🧪 Try It

```bash
mettaplan list
mettaplan run sample.plan
mettaplan trace sample.plan.json
```

Or use in Prolog:

```prolog
?- plan_name(basic).
?- interactive_plan([is_parallel(a), is_sequential(b), c]).
```

---

## 📄 File Types

- `*.plan` — Prolog format (`plan_goals([...])`)
- `*.json` — Machine-friendly format
- `trace_log.json` — Execution log
- `trace_viewer.html` — Interactive trace explorer
- `dag.png`, `stages.png` — Graphviz output

---

## 🧠 Learn More

- `run_pipeline.pl` — Full example runner
- `run_pipeline.sh` — Shell launch script
- `mettaplan` — CLI interface
- `reorder_repl.pl` — Interactive REPL core

---

## 📁 Contributing

Fork, file issues, or submit pull requests on GitHub.
