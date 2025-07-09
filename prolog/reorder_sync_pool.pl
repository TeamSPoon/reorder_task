/** <module> reorder_sync_pool

Runs tasks in a thread pool using synchronized variables.
Supports various modes: sequential, per-task threads, or N-threaded groupings.
*/


:- module(reorder_sync_pool,
    [run_sync_task_pool/3,
     run_sync_task_pool_optimal/3])

:- use_module(reorder_task_static, [group_parallel_stages/4]). [
    run_sync_task_pool/3
]).

:- use_module(threaded_attvar).

%% Run tasks in different sync/thread pool strategies
run_sync_task_pool(0, TaskList, []) :- !,
    task_list_when_ready(TaskList).

run_sync_task_pool(1, TaskList, [ThreadID]) :- !,
    spawn_sync_thread_group(TaskList, ThreadID).

run_sync_task_pool(inf, TaskList, Handles) :- !,
    maplist(sync_thread_create_wrapper, TaskList, Handles).

run_sync_task_pool(N, TaskList, Handles) :-
    var(N), length(TaskList, Len), N = Len, !,
    run_sync_task_pool(Len, TaskList, Handles).

run_sync_task_pool(N, TaskList, Handles) :- N > 1,
    length(EmptyPools, N),
    maplist(=([]), EmptyPools),
    distribute_tasks(TaskList, N, EmptyPools, ThreadedTasks),
    maplist(spawn_sync_thread_group, ThreadedTasks, Handles).

%% Distribute tasks across N buckets
distribute_tasks([], _, Acc, Acc).
distribute_tasks([T|Ts], N, [H|Tails], [HNew|Rest]) :-
    append(H, [T], HNew),
    append(Tails, [HNew], Rotated),
    distribute_tasks(Ts, N, Rotated, Rest).

%% Spawn a thread for each task
sync_thread_create_wrapper(Task, ThreadID) :-
    threaded_attvar:sync_thread_create(when_ready(Task), ThreadID, [detached(false)]).

%% Spawn a thread for a group of tasks
spawn_sync_thread_group(TaskGroup, ThreadID) :-
    threaded_attvar:sync_thread_create(task_list_when_ready(TaskGroup), ThreadID, []).

%% Wait for readiness and execute
task_list_when_ready(TaskList) :-
    forall(member(Call, TaskList), when_ready(Call)).

%% Hook to be defined externally (based on sync executor semantics)
:- multifile when_ready/1.


:- use_module(threaded_attvar, [shared_variables/2, copy_term_with_sync/5]).

%% :-
    copy_term_with_sync(task_list_when_ready(TaskGroup), Copy, _, _, SharedVars),
    threaded_attvar:sync_thread_create(Copy, ThreadID, []).



%% run_sync_task_pool_optimal(+N, +TaskList, -Handles)
%% First statically groups, then distributes tasks minimizing shared vars
run_sync_task_pool_optimal(N, TaskList, Handles) :-
    term_variables(TaskList, PreBound),
    group_parallel_stages(TaskList, PreBound, Stages, _),
    flatten(Stages, Ordered),
    length(EmptyPools, N),
    maplist(=([]), EmptyPools),
    distribute_tasks(Ordered, N, EmptyPools, ThreadedTasks),
    thread_shared_variable_plan(ThreadedTasks, SharedVars),
    maplist({SharedVars}/[Group,ID]>>spawn_group_with_shared(Group, SharedVars, ID), ThreadedTasks, Handles).
