
/** <module> reorder_executor

Execution engine for running tasks based on readiness and scheduling logic.
Supports static, dynamic, and parallel execution modes.
*/


:- module(task_executor, [
    run_static_parallel/3,
    run_static_sequential/3,
    run_dynamic_ready_loop/3
]).

:- use_module(reorder_task_static).

% === Static Parallel ===

run_static_parallel(TaskList, PreTest, FinalTest) :-
    retractall(called_step(_, _)),
    call(PreTest),
    term_variables(PreTest, PreVars),
    group_parallel_stages(TaskList, PreVars, Stages, Unplaced),
    ( Unplaced == [] ->
        run_parallel_stages(Stages),
        call(FinalTest)
    ; format("Unplaced goals due to unresolved dependencies:~n~w~n", [Unplaced]),
      fail
    ).

run_parallel_stages([]).
run_parallel_stages([Stage|Rest]) :-
    run_stage_concurrently(Stage),
    run_parallel_stages(Rest).

run_stage_concurrently([]).
run_stage_concurrently([Goal|Rest]) :-
    call(Goal),
    run_stage_concurrently(Rest).

% === Static Sequential ===

run_static_sequential(TaskList, PreTest, FinalTest) :-
    retractall(called_step(_, _)),
    call(PreTest),
    term_variables(PreTest, PreVars),
    static_linear_order(TaskList, PreVars, Ordered, Unplaced),
    ( Unplaced == [] ->
        run_goals(Ordered),
        call(FinalTest)
    ; format("Unplaced goals due to unresolved dependencies:~n~w~n", [Unplaced]),
      fail
    ).



% === Dynamic Ready Loop ===

run_dynamic_ready_loop(TaskList, PreTest, FinalTest) :-
    retractall(called_step(_, _)),
    call(PreTest),
    term_variables(PreTest, PreVars),
    run_ready_loop(TaskList, PreVars, Executed),
    ( Executed \= [] ->
        call(FinalTest)
    ; format("No goals executed. Check dependencies.~n"), fail
    ).

run_ready_loop([], _, []).
run_ready_loop(Remaining, Bound, ExecutedAll) :-
    select_ready_goals(Remaining, Bound, ReadyGoals, Rest),
    ( ReadyGoals == [] ->
        format("Unresolved tasks remain:~n~w~n", [Remaining]),
        ExecutedAll = []
    ; run_goals_collect_vars(ReadyGoals, NewBound),
      append(Bound, NewBound, UpdatedBound),
      run_ready_loop(Rest, UpdatedBound, ExecutedRest),
      append(ReadyGoals, ExecutedRest, ExecutedAll)
    ).

select_ready_goals([], _, [], []).

select_ready_goals([G|Gs], Bound, [G|Ready], Rest) :-
    is_ready(G, Bound), !,
    select_ready_goals(Gs, Bound, Ready, Rest).

select_ready_goals([G|Gs], Bound, Ready, [G|Rest]) :-
    select_ready_goals(Gs, Bound, Ready, Rest).

run_goals_collect_vars([], []).

run_goals_collect_vars([G|Gs], AllVars) :-
    call(G),
    term_variables(G, Vars),
    run_goals_collect_vars(Gs, VarsRest),
    append(Vars, VarsRest, AllVars).


% === Dynamic Parallel Ready Loop ===

:- use_module(library(thread)).

run_dynamic_parallel_ready(TaskList, PreTest, FinalTest) :-
    retractall(called_step(_, _)),
    call(PreTest),
    term_variables(PreTest, PreVars),
    message_queue_create(Queue),
    run_ready_workers(TaskList, PreVars, Queue, 4, WorkerThreads),
    thread_join_all(WorkerThreads),
    message_queue_destroy(Queue),
    call(FinalTest).

run_ready_workers(TaskList, PreVars, Queue, N, Threads) :-
    findall(ID,
        (between(1, N, _),
         thread_create(worker_loop(TaskList, Queue, PreVars), ID, [detached(false)])),
        Threads).

worker_loop(TaskList, Queue, InitBound) :-
    repeat,
    thread_get_message(Queue, ready(Goal), [timeout(0.01)]),
    !,
    ( call(Goal),
      term_variables(Goal, Vars),
      thread_send_message(Queue, new_bindings(Vars)),
      fail
    ; thread_get_message(Queue, stop), !
    ).

thread_join_all([]).
thread_join_all([ID|Rest]) :- thread_join(ID, _), thread_join_all(Rest).


% === Shared Binding Tracker, Debug Logging, and Controlled Shutdown ===

:- dynamic bound_var/1.
:- dynamic goal_done/1.

:- use_module(library(thread)).
:- use_module(library(debug)).

% Public interface
run_dynamic_parallel_ready(TaskList, PreTest, FinalTest) :-
    retractall(bound_var(_)),
    retractall(goal_done(_)),
    retractall(called_step(_, _)),
    call(PreTest),
    term_variables(PreTest, PreVars),
    maplist(assert_bound_var, PreVars),
    message_queue_create(Queue),
    run_ready_workers(TaskList, Queue, 4, WorkerThreads),
    monitor_goals(TaskList, Queue),
    stop_workers(Queue, WorkerThreads),
    message_queue_destroy(Queue),
    call(FinalTest).

assert_bound_var(V) :-
    ( var(V) -> true ; (bound_var(V) -> true ; assertz(bound_var(V))) ).

% Spawns worker threads
run_ready_workers(TaskList, Queue, N, Threads) :-
    findall(ID,
        (between(1, N, I),
         thread_create(worker_loop(TaskList, Queue, I), ID, [detached(false)])),
        Threads).

% Worker loop
worker_loop(TaskList, Queue, ID) :-
    format("[Worker ~w] Started~n", [ID]),
    repeat,
    thread_get_message(Queue, Msg),
    ( Msg = ready(Goal) ->
        format("[Worker ~w] Running: ~w~n", [ID, Goal]),
        (call(Goal) ->
            term_variables(Goal, Vars),
            maplist(assert_bound_var, Vars),
            assertz(goal_done(Goal)),
            thread_send_message(Queue, progress(Goal))
        ; format("[Worker ~w] FAILED: ~w~n", [ID, Goal])
        ),
        fail
    ; Msg = stop ->
        format("[Worker ~w] Shutting down~n", [ID]), !
    ; fail ).

% Goal monitoring and dispatch
monitor_goals(TaskList, Queue) :-
    monitor_goals_loop(TaskList, Queue, 0).

monitor_goals_loop(TaskList, Queue, Tick) :-
    sleep(0.05),
    include(not_done, TaskList, Remaining),
    include(shared_is_ready, Remaining, Ready),
    ( Ready == [] ->
        ( Remaining == [] ->
            format("[Monitor] All goals complete.~n"), true
        ; Tick > 100 ->
            format("[Monitor] TIMEOUT or DEADLOCK: Remaining: ~w~n", [Remaining]), fail
        ; monitor_goals_loop(TaskList, Queue, Tick + 1)
        )
    ; forall(member(G, Ready), (
          format("[Monitor] Dispatching: ~w~n", [G]),
          thread_send_message(Queue, ready(G))
      )),
      sleep(0.05),
      monitor_goals_loop(TaskList, Queue, Tick + 1)
    ).

not_done(G) :- \+ goal_done(G).

shared_is_ready(G) :-
    required_threshold(G, Threshold, Vars),
    count_bound_in_dynamic(Vars, Count),
    Count >= Threshold.

count_bound_in_dynamic([], 0).
count_bound_in_dynamic([V|Vs], N) :-
    count_bound_in_dynamic(Vs, N0),
    (bound_var(V) -> N is N0 + 1 ; N = N0).

% Controlled shutdown
stop_workers(Queue, Threads) :-
    forall(member(_, Threads), thread_send_message(Queue, stop)),
    thread_join_all(Threads).

thread_join_all([]).
thread_join_all([ID|Rest]) :- thread_join(ID, _), thread_join_all(Rest).


% === Enhancements: Trace, Priority, Retry ===

:- dynamic retry_count/2.
:- dynamic goal_priority/2.

max_retries(3).

% Example static priorities (can be customized)
goal_priority(G, P) :- functor(G, Name, _), priority_for(Name, P).
priority_for(step_abd_requires_2_bound, 1).
priority_for(step_was_ac_requires_1_bound, 2).
priority_for(step_a_requires_1_bound, 3).
priority_for(step_was_db_requires_1_bound, 4).
priority_for(_, 5).  % Default low priority

% Enhanced monitor with visual trace and priority
monitor_goals(TaskList, Queue) :-
    monitor_goals_loop(TaskList, Queue, 0).

monitor_goals_loop(TaskList, Queue, Tick) :-
    sleep(0.05),
    include(not_done, TaskList, Remaining),
    include(shared_is_ready, Remaining, ReadyRaw),
    sort_goals_by_priority(ReadyRaw, ReadySorted),
    ( ReadySorted == [] ->
        ( Remaining == [] ->
            format("[Monitor] ✅ All goals complete.~n"), true
        ; Tick > 100 ->
            format("[Monitor] ⛔ TIMEOUT or DEADLOCK: Remaining: ~w~n", [Remaining]), fail
        ; format("[Monitor] ⏳ Waiting... Tick ~d~n", [Tick]),
          monitor_goals_loop(TaskList, Queue, Tick + 1)
        )
    ; forall(member(G, ReadySorted), (
          format("[Monitor] 🟢 Dispatching: ~w~n", [G]),
          thread_send_message(Queue, ready(G))
      )),
      sleep(0.05),
      monitor_goals_loop(TaskList, Queue, Tick + 1)
    ).

sort_goals_by_priority(Goals, Sorted) :-
    map_list_to_pairs(get_goal_priority, Goals, Pairs),
    keysort(Pairs, SortedPairs),
    pairs_values(SortedPairs, Sorted).

get_goal_priority(G, P) :- goal_priority(G, P), !.
get_goal_priority(_, 1000).

% Retry/backoff logic on worker failure
worker_loop(TaskList, Queue, ID) :-
    format("[Worker ~w] 🧵 Started~n", [ID]),
    repeat,
    thread_get_message(Queue, Msg),
    ( Msg = ready(Goal) ->
        format("[Worker ~w] ▶️ Running: ~w~n", [ID, Goal]),
        ( call(Goal) ->
            format("[Worker ~w] ✅ Success: ~w~n", [ID, Goal]),
            term_variables(Goal, Vars),
            maplist(assert_bound_var, Vars),
            assertz(goal_done(Goal)),
            thread_send_message(Queue, progress(Goal)),
            retractall(retry_count(Goal, _))
        ; handle_retry(ID, Queue, Goal)
        ),
        fail
    ; Msg = stop ->
        format("[Worker ~w] 🛑 Stopping~n", [ID]), !
    ; fail ).

handle_retry(ID, Queue, Goal) :-
    ( retry_count(Goal, N) -> true ; N = 0 ),
    N1 is N + 1,
    max_retries(Max),
    ( N1 > Max ->
        format("[Worker ~w] ❌ Max retries exceeded for: ~w~n", [ID, Goal]),
        assertz(goal_done(Goal))  % Consider it dead
    ; format("[Worker ~w] 🔁 Retrying (~d): ~w~n", [ID, N1, Goal]),
      retractall(retry_count(Goal, _)),
      assertz(retry_count(Goal, N1)),
      thread_send_message(Queue, ready(Goal))
    ).


run_goals([]).
run_goals([G|Gs]) :-
    inline_all_code(G, G2, [max_depth(5), multithread_safe(true), target_thread(shared_state)]),
    call(G2),
    run_goals(Gs).

run_stage_concurrently([]).
run_stage_concurrently([Goal|Rest]) :-
    inline_all_code(Goal, G2, [max_depth(5), multithread_safe(true), target_thread(shared_state)]),
    call(G2),
    run_stage_concurrently(Rest).
