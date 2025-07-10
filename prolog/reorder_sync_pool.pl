/** <module> reorder_sync_pool

Runs tasks in a thread pool using synchronized variables.
Supports various modes: sequential, per-task threads, or N-threaded groupings.
*/


:- module(reorder_sync_pool,
          [ run_sync_task_pool/3,
            run_sync_task_pool_optimal/3
          ]).

:- use_module(reorder_task_static, [group_parallel_stages/4]).

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



%% shared_variables(+ListOfTasks, -SharedVars)
%% Finds variables that appear in 2 or more distinct tasks
shared_variables(Tasks, Shared) :-
    maplist(term_variables, Tasks, VarsPerTask),
    append(VarsPerTask, All),
    sort(All, Unique),
    findall(V, (member(V, Unique), appears_in_multiple_tasks(V, VarsPerTask)), Shared).

appears_in_multiple_tasks(_, []) :- fail.
appears_in_multiple_tasks(V, [L|Ls]) :-
    ( memberchk(V, L) ->
        memberchk_rest(V, Ls)
    ; appears_in_multiple_tasks(V, Ls)
    ).

memberchk_rest(_, []) :- fail.
memberchk_rest(V, [L|Ls]) :-
    ( memberchk(V, L) -> true ; memberchk_rest(V, Ls) ).

%% Enhanced copy_term_with_sync that accepts 'auto' to infer shared vars
copy_term_with_sync(Term, Copy, ThisEngine, RemoteEngine, auto) :-
    shared_variables([Term], Shared),
    copy_term_with_sync(Term, Copy, ThisEngine, RemoteEngine, Shared).


%% call_in_thread(+ThreadID, +Goal)
%% Posts a goal to another thread for execution
call_in_thread(ThreadID, Goal) :-
    thread_send_message(ThreadID, run(Goal)).

%% Hook for redirected global set (e.g. b_setval)
safe_global_set(ThreadID, Key, Value) :-
    call_in_thread(ThreadID, threaded_attvar:do_setval(Key, Value)).

do_setval(Key, Value) :-
    nb_setval(Key, Value).


