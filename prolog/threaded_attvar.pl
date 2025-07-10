
/** <module> threaded_attvar

Implements attributed variables to enable synchronization between threads or engines.
Provides synchronization hooks for cross-thread communication.
*/

:- module(threaded_attvar, [
    sync_thread_create/3,
    sync_thread_create/4
    % none of these need be public
    %send_to_engine/2,
    %sync_dispatch/1,
    %init_sync_registry/0,
    %add_peer_sync/2
    %find_or_create_sync_var/2
]).

:- use_module(library(gensym)).
:- use_module(library(nb_rbtrees)).
:- use_module(library(lists)).

:- multifile attribute_goals//1.


init_sync_registry :-
    rb_empty(Tree),
    nb_setval('$syn_vars', Tree).

:- thread_initialization(threaded_attvar:init_sync_registry).

attr_unify_hook(VarID, Value) :-
    ( var(Value) ->
       ( get_attr(Value, threaded_attvar, OtherVarID) ->
        OtherVarID==VarID
        ; (put_attr(Value, threaded_attvar, VarID),notify_peers(VarID,Value)))
     ; notify_peers(VarID,Value)).


/*
find_or_create_sync_var(VarID, Cell) :-
    nb_getval('$syn_vars', Tree),
    ( nb_rb_get_node(Tree, VarID, Node) ->
        nb_rb_node_value(Node, Cell)
    ; Cell = Cell,
      nb_rb_insert(Tree, VarID, Cell)
    ).
*/

register_sync_var(Engine, VarID, Var) :-
    assert(var_sync(VarID,Engine)),
    Cell = var_cell(Var),
    nb_getval('$syn_vars', Tree),
    nb_rb_insert(Tree, VarID, Cell).

lookup_sync_var(VarID, Cell) :-
    nb_getval('$syn_vars', Tree),
    nb_rb_get_node(Tree, VarID, Node),
    nb_rb_node_value(Node, Cell).

:- dynamic(var_sync/2).
notify_peers(VarID,Value):-
  forall(var_sync(VarID,EngineID),
     send_to_engine(EngineID, bind(VarID,Value))).

send_to_engine(EngineID, Message) :-
    ( current_thread(EngineID, _) ->
        thread_signal(EngineID, threaded_attvar:sync_dispatch(Message))
    ; engine_post(EngineID, Message)
    ).

sync_dispatch(Msg) :-
    with_mutex(sync_dispatch_lock, sync_dispatch_serial(Msg)).

sync_dispatch_serial(bind(VarID, Value)) :- !,
    ignore((lookup_sync_var(VarID, Cell), Cell = var_cell(Value), setarg(1, Cell, Value))).
sync_dispatch_serial(Other) :-
    format("⚠️ Unexpected message: ~q~n", [Other]).




sync_thread_create(Goal, ThreadID, Options) :-
    term_variables(Goal, Vars),
    sync_thread_create(Vars, Goal, ThreadID, Options).


sync_thread_create(Vars, Goal, ThreadID, Options):-
    % ensure the varible update this engine when changed in the child
    engine_self(ThisEngine),maplist(add_peer_sync(ThisEngine),Vars),
    thread_create(
        (
         engine_self(ThatEngine),maplist(add_peer_sync(ThatEngine),Vars),
         call(Goal)
        ),
        ThreadID,
        Options
    ).


add_peer_sync(ThisEngine,Var):- attvar(Var),get_attr(Var,threaded_attvar,VarID),!,(var_sync(VarID,ThisEngine)->true;(assert(var_sync(VarID,ThisEngine)),register_sync_var(ThisEngine,VarID, Var))).
add_peer_sync(ThisEngine,Var):- gensym(var_, VarID), register_sync_var(ThisEngine, VarID, Var),assert(var_sync(VarID,ThisEngine)),put_attr(Var,threaded_attvar,VarID).


























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

