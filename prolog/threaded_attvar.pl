/** <module> threaded_attvar

Implements attributed variables to enable synchronization between threads or engines.
Provides synchronization hooks for cross-thread communication.
*/

:- module(threaded_attvar, [
    sync_thread_create/3,
    sync_thread_create/4,
    on_bind/2
]).

:- use_module(library(gensym)).
:- use_module(library(nb_rbtrees)).
:- use_module(library(lists)).
:- use_module(library(logicmoo_common)).



% ------------------------------------------------------------------
% Sync Variable Registry (internal to each thread/engine)
% ------------------------------------------------------------------

%% init_sync_registry is det.
%
% Initializes the global registry used to store synchronized variables
init_sync_registry :-
    rb_empty(Tree),
    nb_setval('$syn_vars', Tree).

register_sync_var(VarID, RealVar) :-
    nb_getval('$syn_vars', Tree),
    nb_rb_insert(Tree, VarID, RealVar).


lookup_sync_var(VarID, RealVar) :-
    nb_getval('$syn_vars', Tree),
    nb_rb_get_node(Tree, VarID, Node),
    nb_rb_node_value(Node, RealVar).

% Initialization
:- thread_initialization(threaded_attvar:init_sync_registry).

% ------------------------------------------------------------------
% Reactive Bind Listeners
% ------------------------------------------------------------------
% our scheduling system can see how many are waiting
:- dynamic(waiting_on_bind/2).

:- dynamic(var_sync/2).


%% attr_unify_hook(+VarID, +Value)
%
% Handles unification logic when a variable with this attribute is unified.
attr_unify_hook(binding(_VarID, BindingPrev), Value) :- BindingPrev == Value, !. % means we already handled the new value
attr_unify_hook(Binding, Value) :-
    Binding = binding( VarID, BindingPrev),
    ( var(Value) ->
        ( get_attr(Value, threaded_attvar, binding(_OtherVarID, PrevOther)) ->
              PrevOther = BindingPrev

        ; (put_attr(Value, threaded_attvar, Binding),
             (Value \== BindingPrev -> notify_peers(VarID, Value) ; true))
        )

    ; (setarg(2, Binding, Value), notify_peers(VarID, Value))).


%% on_bind(+Var, :Goal) is nondet.
%
% Waits for a message that the given variable has been bound,
% and runs the Goal once the message arrives.
:- meta_predicate on_bind(?, 0).
on_bind(Var, Goal) :-
    ( \+ attvar(Var) ->
        % Fast path: variable already bound — run immediately
        call(Goal)
    ; get_attr(Var, threaded_attvar, binding(VarID, _)) ->
        thread_engine_self(Self),
        assert(waiting_on_bind(Self, VarID)),
        thread_get_message(bound(VarID)),
        retractall(waiting_on_bind(Self, VarID)),
        call(Goal)
    ;
        % Defensive fallback: variable was never tagged via put_attr/3
        throw(error(not_threaded_attvar(Var), on_bind/2))
    ).


%% notify_peers(+VarID, +Value)
%
% Notifies all engines watching a given VarID that it was bound.
notify_peers(VarID, Value) :-
    thread_engine_self(Self),
    forall( (var_sync(VarID, Engine), Engine\==Self),
             thread_signal(Engine, threaded_attvar:sync_dispatch(bind(VarID, Value)))).

%% sync_dispatch(+Message)
%
% Handles synchronized (with mutex) dispatch of messages.
sync_dispatch(Msg) :-
    with_mutex(sync_dispatch_lock, sync_dispatch_serial(Msg)).

sync_dispatch_serial(bind(VarID, Value)) :- !,
    ignore((
            lookup_sync_var(VarID, RealVar),
            get_attr(RealVar, threaded_attvar, Binding),
            arg(2, Binding, BindingPrev),
            (BindingPrev == Value -> true % nothing to do
             ; ( nb_setarg(2, Binding, Value),  % Update the attr itself so the next line (which will call attr_unify_hook/3) doesnt re-notify all the peers,
                                                % Since someone is already notifying everyone (that is why we even know right now)
                 RealVar = Value)))),           % yet, we want attr_unify_hook for other attributes within this thread to trigger

        % no need to update '$syn_vars' since we physically updated RealVar
        %nb_current('$syn_vars', Tree),
        %nb_rb_update(Tree, VarID, RealVar)

    thread_engine_self(Self), % send this along
    ( waiting_on_bind(Self, VarID) -> % if we have waiters
                thread_send_message(Self, bound(VarID)) ; true).


sync_dispatch_serial(Other) :-
    debug( threaded_attvar,  "⚠️ Unexpected message: ~q~n", [Other]).




% ------------------------------------------------------------------
% Thread Creation
% ------------------------------------------------------------------

%% sync_thread_create(+Vars, +Goal, -ThreadID, +Options)
%
% Spawns a new thread sharing variable bindings via attributed variables.
sync_thread_create(Vars, Goal, ThreadID, Options) :-
    thread_engine_self(Self),
    thread_at_exit(retractall(var_sync(_, Self))),
    maplist(add_peer_sync(Self), Vars),
    thread_create(
        (
            thread_engine_self(Engine),
            thread_at_exit(retractall(var_sync(_, Engine))),
            thread_at_exit(retractall(waiting_on_bind(Engine, _))),
            maplist(add_peer_sync(Engine), Vars),
            call(Goal)
        ),
        ThreadID,
        Options
    ).

%% sync_thread_create(+Goal, -ThreadID, +Options)
%
% Variant that infers variables from Goal.
sync_thread_create(Goal, ThreadID, Options) :-
    term_variables(Goal, Vars),
    sync_thread_create(Vars, Goal, ThreadID, Options).

%% thread_engine_self(-ID)
%
% Gets current thread or engine ID.
thread_engine_self(Self) :- engine_self(Self), !.
thread_engine_self(Self) :- thread_self(Self).

%% add_peer_sync(+Engine, +Var)
%
% Registers a variable as synchronized and binds an attribute to it.
add_peer_sync(Engine, Var) :-
    attvar(Var),
    get_attr(Var, threaded_attvar, binding(VarID, _)), !,
    ( var_sync(VarID, Engine) -> true
    ; assert(var_sync(VarID, Engine)),
      register_sync_var(VarID, Var)
    ).

add_peer_sync(Engine, Var) :-
    gensym(var_, VarID),
    register_sync_var(VarID, Var),
    assert(var_sync(VarID, Engine)),
    put_attr(Var, threaded_attvar, binding(VarID, _)).


