
# Task Executor and Sync System (Prolog)

This package provides a multi-threaded, constraint-aware task execution and scheduling framework in SWI-Prolog, featuring parallel execution, priority dispatch, and synchronized variable sharing using attributed variables.

---

## 📦 Files Included

### 1. `reorder_task_static.pl`
**Purpose**: Static scheduling engine for Prolog task reordering.

**Main Predicates**:
- `reorder_task_groups(+Tasks, +PreBound, -Stages, -Unplaced)` – Group tasks based on readiness for sequential execution.
- `group_parallel_stages(+Tasks, +PreBound, -ParallelStages, -Unplaced)` – Group tasks into parallel-executable stages.
- `static_linear_order(+Tasks, +PreBound, -OrderedList, -Unplaced)` – Flattened sequence for single-threaded use.
- `print_dependency_graph(+Tasks, +PreBound)` – Human-readable analysis of variable dependencies.

---

### 2. `reorder_executor.pl`
**Purpose**: Runtime task execution system supporting sequential, dynamic, and parallel modes.

**Main Predicates**:
- `run_static_sequential(+Tasks, +PreTest, +FinalTest)` – Executes statically ordered tasks in one thread.
- `run_static_parallel(+Tasks, +PreTest, +FinalTest)` – Executes grouped tasks in parallel stages.
- `run_dynamic_ready_loop(+Tasks, +PreTest, +FinalTest)` – Dynamically picks and runs tasks as they become ready.
- `run_dynamic_parallel_ready(+Tasks, +PreTest, +FinalTest)` – Multi-threaded dynamic execution with retry, backoff, and trace.

Features:
- Shared variable state tracking (`bound_var/1`)
- Retry logic (`max_retries/1`)
- Priority-based dispatch (`goal_priority/2`)
- Deadlock detection with visual logging

---

### 3. `threaded_attvar.pl`
**Purpose**: Implements attributed variables for variable synchronization across threads or engines.

**Main Predicates**:
- `synchronized_with(+Var, +Command)` – Mark a variable as synchronized with an external command.
- `add_peer_sync(+Var, +Command)` – Add synchronization command to an existing sync-var.
- `copy_term_with_sync(+Term, -Copy, +ThisEngine, +ThatEngine, +Vars)` – Copies and sets up sync between variable copies.
- `sync_thread_create(+Goal, -ThreadID, +Options)` – Spawns a thread with shared synchronized variables.

Internals:
- Uses `put_attr/3` for attributed variable hooks
- Message passing with `send_to_engine/2` and `sync_dispatch/1`
- Shared registry backed by `nb_rbtrees`

---

### 4. `reorder_executor_tests.plt`
**Purpose**: Unit tests for all scheduling and executor modes.

**Test Cases**:
- Static sequential execution
- Static parallel execution
- Dynamic readiness loop
- Dynamic parallel thread execution

Use:
```prolog
?- [reorder_executor_tests], run_tests.
```

---

### 5. `test_threaded_attvar.pl`
**Purpose**: Demonstrates thread-safe variable sharing with `sync_thread_create/3`.

**Test Cases**:
- One-way variable update from main to thread
- Two-way shared variable communication

Use:
```prolog
?- [test_threaded_attvar], run_tests.
```

---

## ✅ Getting Started

Load the files:
```prolog
?- [reorder_task_static, reorder_executor, threaded_attvar].
```

Run a test task:
```prolog
?- get_sample_task(Vars, TaskList, PreTest, FinalTest),
   run_dynamic_parallel_ready(TaskList, PreTest, FinalTest).
```

Test synchronization:
```prolog
?- [test_threaded_attvar], run_tests.
```

---

## 💡 Notes

- Works best with SWI-Prolog ≥ 8.2 for full `engine/thread` and `atts` support.
- Uses thread-safe `nb_rbtrees` for fast var tracking.
- Can be extended to remote/multi-process with `engine_post/2`, sockets, or message brokers.



---

## 🔍 Static Analyzer for Variable Sharing

When synchronizing logic variables across **threads** or **engines**, unnecessary sharing can:

- Increase communication overhead
- Waste memory by duplicating bindings
- Cause unintended synchronization side effects

To address this, this system includes a **static sharing analyzer**:

```prolog
shared_variables(+ListOfTasks, -SharedVars)
```

### ✅ What It Does

- Scans the list of tasks (goals or compound terms)
- Collects all variables used in each task
- Detects variables that appear in **more than one distinct task**
- Returns **only** those shared variables

These are the **minimal set of variables that need to be synchronized** between threads.

---

### 🔧 Where It’s Used

When calling:

```prolog
copy_term_with_sync(Task, Copy, ThisEngine, RemoteEngine, auto).
```

The system automatically invokes:

```prolog
shared_variables([Task], SharedVars)
```

...and only those variables are marked with synchronization hooks.

---

### 🧠 Why It Matters

Without this step:

- All variables might get synchronized unnecessarily.
- Even thread-local variables would be included in global sync registries.

With this step:

- You maintain **efficient parallelism**.
- Only cross-thread communication is tracked and linked.
- Determinism is preserved where possible, concurrency is not overly restricted.

---

### 📊 Future Extensions

We plan to include:

- A map of variables per task for debugging: `task_variable_map/2`
- A visualization of shared-variable dependencies between threads
- Runtime tracing of variable binding flow across threads



## 🧠 Execution Strategy Comparison

| Mode                         | Threads | Scheduling Style | Uses Inlining? | Handles Sync Vars | Optimal Parallelism | Best Use Case                           |
|------------------------------|---------|------------------|----------------|-------------------|---------------------|------------------------------------------|
| `run_static_sequential/3`    | ❌      | Static (linear)  | ✅             | ❌                | ❌                  | Deterministic logic, step-by-step        |
| `run_static_parallel/3`      | ✅      | Static (staged)  | ✅             | ❌                | ⚠ Partial           | Disjoint clauses with no shared vars     |
| `run_dynamic_ready_loop/3`   | ✅      | Dynamic (polling)| ✅             | ✅                | ✅                  | Runtime-determined dependencies          |
| `run_sync_task_pool/3`       | ✅      | Static (manual)  | ✅             | ✅                | ⚠ Manual grouping    | Shared vars explicitly tracked           |
| `run_sync_task_pool_optimal/3`| ✅     | Static (analyzed)| ✅             | ✅                | ✅                  | Maximum safe parallelism w/ var analysis |
| `call_in_thread/2` + inline  | ✅      | Manual           | ✅             | ⚠ Partial         | ⚠ Dev usage only    | Testing and targeted runtime control     |

- ✅ = Fully supported
- ⚠ = Partially supported or needs care
- ❌ = Not supported
