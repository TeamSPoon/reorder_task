
/** <module> threaded_attvar

Implements attributed variables to enable synchronization between threads or engines.
Provides synchronization hooks for cross-thread communication.
*/

:- module(threaded_attvar, [
    copy_term_with_sync/5,
    send_to_engine/2,
    sync_dispatch/1,
    synchronized_with/2,
    init_sync_registry/0,
    add_peer_sync/2,
    find_or_create_sync_var/2
]).

:- use_module(library(gensym)).
:- use_module(library(nb_rbtrees)).
:- use_module(library(lists)).

:- multifile attribute_goals//1.

attribute_goals(Var) -->
    {
        get_attr(Var, sync_attr, sync(EngineCmds)),
        prolog_current_frame(Frame),
        prolog_frame_attribute(Frame, predicate_indicator, PI),
        (   PI = system:copy_term(_,_,_)
        ;   PI = system:thread_create(_,_,_)
        ;   PI = system:engine_create(_,_,_)
        ;   PI = system:engine_post(_,_)
        ;   true
        )
    },
    [sync_attr:synchronized_with(Var, EngineCmds)].

init_sync_registry :-
    nb_rb_empty(Tree),
    nb_linkval('$syn_vars', Tree).

:- thread_initialization(sync_attr:init_sync_registry).

attr_unify_hook(sync(Peers), Value) :-
    ( var(Value) ->
        put_attr(Value, sync(Peers))
    ; get_attr(Value, sync_attr, sync(OtherPeers)) ->
        ord_union(Peers, OtherPeers, Combined),
        put_attr(Value, sync(Combined))
    ; forall(member(EngineCmd, Peers),
             call(EngineCmd, Value))
    ).

synchronized_with(Var, EngineCmd) :-
    put_attr(Var, sync_attr, sync([EngineCmd])).

add_peer_sync(Var, EngineCmd) :-
    ( get_attr(Var, sync_attr, sync(Peers)) ->
        ord_add_element(Peers, EngineCmd, NewPeers),
        put_attr(Var, sync_attr, sync(NewPeers))
    ; put_attr(Var, sync_attr, sync([EngineCmd]))
    ).

find_or_create_sync_var(VarID, Cell) :-
    nb_getval('$syn_vars', Tree),
    ( nb_rb_get_node(Tree, VarID, Node) ->
        nb_rb_node_value(Node, Cell)
    ; Cell = var_cell(_),
      nb_rb_insert(Tree, VarID, Cell)
    ).


    copy_term(Term, Copy, Bindings),
    include(should_sync(Bindings), Bindings, EligibleBindings),
    maplist(sync_if_eligible(ThisEngine, RemoteEngine), EligibleBindings).


sync_if_eligible(ThisEngine, RemoteEngine, Eligible, L=R) :-
    ( memberchk(L, Eligible) ->
        gensym(var_, VarID),
        Cmd1 = sync_attr:send_to_engine(RemoteEngine, bind(VarID)),
        Cmd2 = sync_attr:send_to_engine(ThisEngine, bind(VarID)),
        add_peer_sync(L, Cmd1),
        add_peer_sync(R, Cmd2),
        register_sync_var(RemoteEngine, VarID, R),
        register_sync_var(ThisEngine, VarID, L)
    ; true ).

register_sync_var(_Engine, VarID, Var) :-
    Cell = var_cell(Var),
    nb_getval('$syn_vars', Tree),
    nb_rb_insert(Tree, VarID, Cell).

lookup_sync_var(VarID, Cell) :-
    nb_getval('$syn_vars', Tree),
    nb_rb_get_node(Tree, VarID, Node),
    nb_rb_node_value(Node, Cell).

send_to_engine(EngineID, Message) :-
    ( current_thread(EngineID, _) ->
        thread_signal(EngineID, sync_attr:sync_dispatch(Message))
    ; engine_post(EngineID, Message)
    ).

sync_dispatch(Msg) :-
    with_mutex(sync_dispatch_lock, sync_dispatch_serial(Msg)).

sync_dispatch_serial(bind(VarID, Value)) :-
    ( lookup_sync_var(VarID, var_cell(_)) ->
        nb_setarg(1, var_cell(_), Value)
    ; format("⚠️ Unknown VarID: ~w~n", [VarID])
    ).
sync_dispatch_serial(Other) :-
    format("⚠️ Unexpected message: ~q~n", [Other]).

make_engine_command(TargetEngineID, VarID, Command) :-
    Command = sync_attr:update_engine(TargetEngineID, VarID).

update_engine(TargetEngineID, VarID, Value) :-
    sync_attr:send_to_engine(TargetEngineID, bind(VarID, Value)).

sync_thread_create(Goal, ThreadID, Options) :-
    term_variables(Goal, Vars),
    engine_self(ThisEngine),
    thread_create(
        (
            init_sync_registry,
            engine_self(TargetEngine),
            forall(member(Var, Vars),
                   ( gensym(var_, VarID),
                     make_engine_command(ThisEngine, VarID, Cmd1),
                     make_engine_command(TargetEngine, VarID, Cmd2),
                     put_attr(Var, sync_attr, sync([Cmd1])),
                     find_or_create_sync_var(VarID, var_cell(Copy)),
                     put_attr(Copy, sync_attr, sync([Cmd2]))
                   )),
            call(Goal)
        ),
        ThreadID,
        Options
    ).


should_sync(_Bindings, L=R) :-
    var(L), var(R), L \== R.

sync_if_eligible(ThisEngine, RemoteEngine, L=R) :-
    gensym(var_, VarID),
    Cmd1 = threaded_attvar:send_to_engine(RemoteEngine, bind(VarID)),
    Cmd2 = threaded_attvar:send_to_engine(ThisEngine, bind(VarID)),
    threaded_attvar:add_peer_sync(L, Cmd1),
    threaded_attvar:add_peer_sync(R, Cmd2),
    threaded_attvar:register_syn_var(RemoteEngine, VarID, R),
    threaded_attvar:register_syn_var(ThisEngine, VarID, L).

%% copy_term_with_sync(+Term, -Copy, +ThisEngine, +RemoteEngine, +EligibleVars)
%% Copies a term and syncs only the variables listed in EligibleVars
copy_term_with_sync(Term, Copy, ThisEngine, RemoteEngine, EligibleVars) :-
    copy_term(Term, Copy, Bindings),
    include({EligibleVars}/[L=R]>>memberchk(L, EligibleVars), Bindings, EligibleBindings),
    maplist(sync_if_eligible(ThisEngine, RemoteEngine), EligibleBindings).

%% copy_term_with_sync(+Term, -Copy, +ThisEngine, +RemoteEngine)
%% Automatically detects which variables should be synced based on bindings
copy_term_with_sync(Term, Copy, ThisEngine, RemoteEngine) :-
    copy_term(Term, Copy, Bindings),
    include(should_sync, Bindings, EligibleBindings),
    maplist(sync_if_eligible(ThisEngine, RemoteEngine), EligibleBindings).

%% should_sync(+Binding)
%% Syncs variables that are distinct and unbound
should_sync(L=R) :- var(L), var(R), L \== R.

%% sync_if_eligible(+Engine1, +Engine2, +Binding)
sync_if_eligible(ThisEngine, RemoteEngine, L=R) :-
    gensym(var_, VarID),
    Cmd1 = threaded_attvar:send_to_engine(RemoteEngine, bind(VarID)),
    Cmd2 = threaded_attvar:send_to_engine(ThisEngine, bind(VarID)),
    threaded_attvar:add_peer_sync(L, Cmd1),
    threaded_attvar:add_peer_sync(R, Cmd2),
    threaded_attvar:register_syn_var(RemoteEngine, VarID, R),
    threaded_attvar:register_syn_var(ThisEngine, VarID, L).


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
