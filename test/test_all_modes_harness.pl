
:- begin_tests(all_modes_harness).
:- [reorder_task:prolog/reorder_executor].
:- [reorder_task:prolog/reorder_sync_pool].
:- [reorder_task:prolog/reorder_task_static].
:- [reorder_task:prolog/threaded_attvar].

% Sample tasks
demo_tasks([
    (t1 :- a),
    (t2 :- b),
    (t3 :- c)
]).

% Stub goals with sleep and tracing
a :- 
    get_time(T1), format_time(atom(S1), '%T', T1), format("[~w] ⏱️ start a~n", [S1]),
    sleep(1),
    get_time(T2), format_time(atom(S2), '%T', T2), format("[~w] 🏁 end a~n", [S2]).

b :- 
    get_time(T1), format_time(atom(S1), '%T', T1), format("[~w] ⏱️ start b~n", [S1]),
    sleep(2),
    get_time(T2), format_time(atom(S2), '%T', T2), format("[~w] 🏁 end b~n", [S2]).

c :- 
    get_time(T1), format_time(atom(S1), '%T', T1), format("[~w] ⏱️ start c~n", [S1]),
    sleep(4),
    get_time(T2), format_time(atom(S2), '%T', T2), format("[~w] 🏁 end c~n", [S2]).

% Helper to wrap result capturing
run_mode(Name, Goal) :-
    format("~n=== 🔧 Running mode: ~w ===~n", [Name]),
    catch(Goal, E, format("❌ Error in ~w: ~w~n", [Name, E])).

test(run_all_modes) :-
    demo_tasks(Tasks),

    run_mode(static_seq,
        run_static_sequential(Tasks, [], _)),

    run_mode(static_parallel,
        run_static_parallel(Tasks, [], _)),

    run_mode(dynamic_ready_loop,
        run_dynamic_ready_loop(Tasks, [], _)),

    run_mode(sync_pool,
        run_sync_task_pool(Tasks, [], _)),

    run_mode(sync_pool_optimal,
        run_sync_task_pool_optimal(Tasks, [], _)),

    run_mode(call_in_thread_inline,
        (
            inline_all_code(Tasks, [], Inlined),
            forall(member((:- G), Inlined),
                   call_in_thread(G))
        )).

:- end_tests(all_modes_harness).


% Shared variable test scenario
demo_shared_tasks([
    (t1(X) :- gen(X)),
    (t2(X) :- double(X))
]).

gen(X) :-
    get_time(T1), format_time(atom(S1), '%T', T1), format("[~w] 🔄 gen start~n", [S1]),
    sleep(1),
    X = 42,
    get_time(T2), format_time(atom(S2), '%T', T2), format("[~w] ✅ gen done (X=~w)~n", [S2, X]).

double(X) :-
    get_time(T1), format_time(atom(S1), '%T', T1), format("[~w] 💤 double waiting on X~n", [S1]),
    sleep(2),
    Y is X * 2,
    get_time(T2), format_time(atom(S2), '%T', T2), format("[~w] 🔢 double done (2*X=~w)~n", [S2, Y]).

test(run_shared_variable_tasks) :-
    demo_shared_tasks(Tasks),
    run_sync_task_pool_optimal(Tasks, [], _).


% Disjunction task example that gets split
demo_disjunction_tasks([
    (t1 :- (do_a ; do_b))
]).

do_a :-
    get_time(T1), format_time(atom(S1), '%T', T1), format("[~w] ⚡ do_a start~n", [S1]),
    sleep(1),
    get_time(T2), format_time(atom(S2), '%T', T2), format("[~w] ✅ do_a done~n", [S2]).

do_b :-
    get_time(T1), format_time(atom(S1), '%T', T1), format("[~w] 🔁 do_b start~n", [S1]),
    sleep(2),
    get_time(T2), format_time(atom(S2), '%T', T2), format("[~w] ✅ do_b done~n", [S2]).

test(run_disjunction_task_expansion) :-
    demo_disjunction_tasks(Tasks),
    expand_all_groupables(Tasks, Groupables),
    maplist(arg(1), Groupables, Goals),
    run_static_parallel(Goals, [], _).
