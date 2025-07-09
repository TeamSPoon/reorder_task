
# 🎬 MettaPlan Walkthrough

This guide walks you through using the MettaPlan scheduling tool via CLI and REPL.

---

## 🛠️ 1. Install

```bash
make install
```

This installs:
- `mettaplan` CLI → `/usr/local/bin/mettaplan`
- Prolog modules → `/usr/local/share/mettaplan`

---

## 📜 2. List Available Plans

```bash
mettaplan list
```

Outputs all `.plan` and `.json` files.

---

## 🚀 3. Run a Sample Plan

```bash
mettaplan run sample.plan
```

You will be shown:
- Annotated stages
- Prompt to execute
- Outputs and graphs

---

## 🧪 4. Trace a JSON Plan

```bash
mettaplan trace sample.plan.json
```

Results:
- `trace_log.json` — structured runtime log
- `trace_viewer.html` — open in browser

---

## 📊 5. View Graphs

Use any Graphviz viewer (or browser if exported as PNG):

- `dag.png` — Task dependency graph
- `stages.png` — Policy-based stage layout

---

## 🧠 6. Use from REPL

```prolog
?- plan_name(basic).
?- interactive_plan([is_parallel(a), b, is_sequential(c)]).
```

---

## 💾 7. Save and Load

```prolog
?- save_plan_file_json([...], 'my.plan.json').
?- load_json_plan_file('my.plan.json').
```

---

Enjoy your symbolic execution engine!
