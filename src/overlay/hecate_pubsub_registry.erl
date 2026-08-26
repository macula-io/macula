%% @doc Per-identity registry for `hecate_pubsub_server' processes.
%%
%% Holds a `RealmTag => pid()' map and acts as the dispatch hub for
%% inbound SUBSCRIBE / UNSUBSCRIBE / EVENT frames. New realms are
%% materialised via `register/2': the registry spawn-links a
%% `hecate_pubsub_server' worker for the realm and stores its pid.
%% A linked worker that crashes delivers an `'EXIT'' message which
%% the registry traps + uses to clear the entry; a later
%% `register/2' yields a fresh server.
%%
%% == Sprint A invariant ==
%%
%% Realm tags are opaque 32-byte namespace keys. The registry does
%% NOT validate authenticity — multi-tenancy is structural (one
%% server per tag, no cross-realm leakage). Realm authority lives
%% outside the station per `PLAN_DEFERRED_WORK' §6.
%%
%% == Multi-identity (PLAN_MULTI_IDENTITY_RELAY §Phase 2) ==
%%
%% N identities run inside one BEAM. Each identity has its OWN
%% pubsub_registry, owning its OWN per-realm pubsub_server pool.
%% No cross-identity leakage — a realm tag X under identity A is
%% a different overlay than the same realm tag X under identity B.
%%
%% Phase 2 also folded the previous `hecate_pubsub_server_sup'
%% (`simple_one_for_one' pool) into the registry: the registry
%% spawn-links pubsub_servers itself. Equivalent semantics — they
%% are temporary, the registry's monitor was already doing the
%% bookkeeping that the supervisor would have — minus a module +
%% the pid-passing coordination that splitting them required under
%% per-identity supervision.
-module(hecate_pubsub_registry).
-behaviour(gen_server).

-compile({no_auto_import, [register/2]}).

-export([
    start_link/1,
    register/3,
    lookup/2,
    dispatch_frame/4,
    relay_publish/3,
    list_realms/1,
    stop/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export_type([opts/0, realm/0, identity/0]).

-type realm()    :: <<_:256>>.
-type identity() :: macula_identity:key_pair().

-type opts() :: #{
    %% Default identity used when a `register/3' caller does not
    %% pass one explicitly. Optional — passing identity per-call
    %% gives the same behaviour as the pre-Phase-2 API.
    identity     => identity(),
    %% Phase 6 (operational tooling): when supplied, the registry
    %% sets `logger:set_process_metadata(#{identity_id =&gt; Key})'
    %% on init so every log line from the registry process — and
    %% from the pubsub_servers it spawn-links — carries the
    %% identity for grep-friendly diagnostics on a multi-identity
    %% box.
    identity_key => term()
}.

-record(state, {
    default_identity     :: identity() | undefined,
    by_realm = #{}       :: #{realm() => pid()},
    by_pid   = #{}       :: #{pid() => realm()}
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link(opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Idempotently start a pubsub_server for `Realm' under
%% `RegistryPid', signing with `Identity'. If a live server already
%% exists, returns its pid; otherwise spawns a new one and records
%% the mapping. A stale entry pointing at a dead pid is replaced
%% transparently.
-spec register(pid(), realm(), identity()) ->
        {ok, pid()} | {error, term()}.
register(RegistryPid, <<_:256>> = Realm, Identity) ->
    gen_server:call(RegistryPid, {register, Realm, Identity}).

%% @doc Find the pubsub_server pid for `Realm' under `RegistryPid',
%% or report `not_found'.
-spec lookup(pid(), realm()) -> {ok, pid()} | {error, not_found}.
lookup(RegistryPid, <<_:256>> = Realm) ->
    gen_server:call(RegistryPid, {lookup, Realm}).

%% @doc Route a SUBSCRIBE / UNSUBSCRIBE / EVENT frame for `Realm' to
%% the matching pubsub_server. Returns the matched local subscribers
%% (empty list for SUBSCRIBE / UNSUBSCRIBE) or `{error, not_found}'
%% if no server is registered for `Realm' AND no `default_identity'
%% was configured at start-up.
%%
%% **Auto-registration**: when `default_identity' is set on the
%% registry (the production path under
%% `macula_station_identity_sup'), an unknown realm is materialised
%% on demand using that identity and the frame is dispatched against
%% the freshly-spawned server. Tests that omit `default_identity'
%% retain the strict `{error, not_found}' semantics.
-spec dispatch_frame(pid(), realm(), <<_:256>>, macula_frame:frame()) ->
        {ok, [<<_:256>>]} | {error, not_found}.
dispatch_frame(RegistryPid, <<_:256>> = Realm, From, Frame) ->
    gen_server:call(RegistryPid, {dispatch_frame, Realm, From, Frame}).

%% @doc Relay an inbound PUBLISH frame for `Realm' to the matching
%% pubsub_server. The server builds an EVENT frame and returns it
%% together with the local subscribers that should receive it. The
%% caller is responsible for sending `EventFrame' on each
%% subscriber's peering connection.
%%
%% Returns `{ok, EventFrame, [Subs]}' on success,
%% `{error, not_found}' when no server is registered for the realm
%% AND no `default_identity' was configured at start-up.
%%
%% **Auto-registration**: parallel to `dispatch_frame/4'. With
%% `default_identity' set (the production path under
%% `macula_station_identity_sup'), an unknown realm is materialised
%% on demand and the EVENT frame is built against a freshly-spawned
%% server with empty subscribers. This makes the EVENT available for
%% publisher-side bloom-fan forwarding to peer stations that have
%% the topic in their Bloom filter but no subscribe-on-peer chain
%% terminating at us. Tests that omit `default_identity' retain the
%% strict `{error, not_found}' semantics.
-spec relay_publish(pid(), realm(), macula_frame:frame()) ->
        {ok, macula_frame:frame(), [<<_:256>>]}
      | {error, not_found | realm_mismatch}.
relay_publish(RegistryPid, <<_:256>> = Realm, Frame) ->
    gen_server:call(RegistryPid, {relay_publish, Realm, Frame}).

%% @doc Snapshot the realm tags currently materialised under
%% `RegistryPid'. Used by status pages + tests.
-spec list_realms(pid()) -> [realm()].
list_realms(RegistryPid) ->
    gen_server:call(RegistryPid, list_realms).

%% @doc Stop the registry. Uses reason `shutdown' (not the default
%% `normal') so the registry's spawn-linked pubsub_servers receive
%% the exit signal and terminate alongside it. Without this, normal
%% exit does not propagate to non-trapping linked workers.
-spec stop(pid()) -> ok.
stop(RegistryPid) ->
    gen_server:stop(RegistryPid, shutdown, 5_000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% trap_exit: the registry spawn-links its pubsub_servers. A worker
%% crash arrives as `{'EXIT', Pid, Reason}' which `handle_info/2'
%% uses to clear the realm map.
init(Opts) ->
    process_flag(trap_exit, true),
    set_logger_identity(Opts),
    {ok, #state{default_identity = maps:get(identity, Opts, undefined)}}.

set_logger_identity(#{identity_key := Key}) ->
    logger:set_process_metadata(#{identity_id => Key});
set_logger_identity(_) ->
    ok.

handle_call({register, Realm, Identity}, _From, S) ->
    do_register_call(Realm, Identity, maps:find(Realm, S#state.by_realm), S);
handle_call({lookup, Realm}, _From, S) ->
    {reply, lookup_reply(maps:find(Realm, S#state.by_realm)), S};
handle_call({dispatch_frame, Realm, From, Frame}, _From, S) ->
    do_dispatch(Realm, From, Frame, maps:find(Realm, S#state.by_realm), S);
handle_call({relay_publish, Realm, Frame}, _From, S) ->
    do_relay_publish(Realm, Frame, maps:find(Realm, S#state.by_realm), S);
handle_call(list_realms, _From, S) ->
    {reply, maps:keys(S#state.by_realm), S};
handle_call(_Other, _From, S) ->
    {reply, {error, unknown_call}, S}.

handle_cast(_Msg, S) ->
    {noreply, S}.

handle_info({'EXIT', Pid, _Reason}, S) ->
    {noreply, drop_pid(Pid, S)};
handle_info(_Info, S) ->
    {noreply, S}.

terminate(_Reason, _State) ->
    %% Linked workers are taken down automatically by the runtime;
    %% no manual teardown required.
    ok.

%%====================================================================
%% Helpers
%%====================================================================

do_register_call(Realm, Identity, {ok, Pid}, S) ->
    handle_existing(Realm, Pid, Identity, is_process_alive(Pid), S);
do_register_call(Realm, Identity, error, S) ->
    do_spawn_server(Realm, Identity, S).

handle_existing(_Realm, Pid, _Identity, true, S) ->
    {reply, {ok, Pid}, S};
handle_existing(Realm, _Pid, Identity, false, S) ->
    %% The process is dead but the EXIT message has not been
    %% drained yet. Drop it now so the new server lands on a clean
    %% slate.
    do_spawn_server(Realm, Identity, drop_realm(Realm, S)).

do_spawn_server(Realm, Identity, S) ->
    on_server_started(Realm,
                      hecate_pubsub_server:start_link(
                          #{realm => Realm, identity => Identity}),
                      S).

on_server_started(Realm, {ok, Pid}, S) ->
    {reply, {ok, Pid},
     S#state{by_realm = (S#state.by_realm)#{Realm => Pid},
             by_pid   = (S#state.by_pid)#{Pid => Realm}}};
on_server_started(_Realm, {error, _} = E, S) ->
    {reply, E, S}.

lookup_reply({ok, Pid}) -> {ok, Pid};
lookup_reply(error)     -> {error, not_found}.

do_dispatch(_Realm, _From, _Frame, error,
            #state{default_identity = undefined} = S) ->
    {reply, {error, not_found}, S};
do_dispatch(Realm, From, Frame, error,
            #state{default_identity = Id} = S) ->
    %% Auto-register on first arrival so SUBSCRIBE / EVENT frames
    %% don't need a separate `register' round-trip from the caller.
    %% Only fires when a default_identity was configured at start-up
    %% — tests pass `#{}' to retain the strict not_found semantics.
    on_auto_registered(ensure_server(Realm, Id, S), Realm, From, Frame);
do_dispatch(Realm, From, Frame, {ok, Pid}, S) ->
    forward_frame(Realm, Pid, From, Frame, S).

on_auto_registered({ok, Pid, S}, Realm, From, Frame) ->
    forward_frame(Realm, Pid, From, Frame, S);
on_auto_registered({error, Reason, S}, _Realm, _From, _Frame) ->
    {reply, {error, Reason}, S}.

forward_frame(Realm, Pid, From, Frame, S) ->
    try hecate_pubsub_server:process_frame(Pid, From, Frame) of
        Subs -> {reply, {ok, Subs}, S}
    catch
        exit:{noproc, _} ->
            {reply, {error, not_found}, drop_realm(Realm, S)}
    end.

%%====================================================================
%% Internals — relay_publish
%%====================================================================

do_relay_publish(_Realm, _Frame, error,
                 #state{default_identity = undefined} = S) ->
    {reply, {error, not_found}, S};
do_relay_publish(Realm, Frame, error,
                 #state{default_identity = Id} = S) ->
    %% Auto-register so we can build the EVENT frame even when no
    %% local subscribers exist. The caller (pubsub_dispatcher) needs
    %% the frame to fan out to peer stations whose Bloom filter
    %% matches the topic — without auto-register, a station with no
    %% local interest for a realm would drop the publish on the floor
    %% even when downstream peers in the mesh are subscribed.
    on_auto_registered_publish(ensure_server(Realm, Id, S), Realm, Frame);
do_relay_publish(Realm, Frame, {ok, Pid}, S) ->
    forward_relay_publish(Realm, Pid, Frame, S).

on_auto_registered_publish({ok, Pid, S}, Realm, Frame) ->
    forward_relay_publish(Realm, Pid, Frame, S);
on_auto_registered_publish({error, Reason, S}, _Realm, _Frame) ->
    {reply, {error, Reason}, S}.

forward_relay_publish(Realm, Pid, Frame, S) ->
    try hecate_pubsub_server:relay_publish(Pid, Frame) of
        {EventFrame, Matched} when is_list(Matched) ->
            {reply, {ok, EventFrame, Matched}, S};
        {error, Reason} ->
            {reply, {error, Reason}, S}
    catch
        exit:{noproc, _} ->
            {reply, {error, not_found}, drop_realm(Realm, S)}
    end.

%%====================================================================
%% Internals — ensure_server (shared by register + auto-register)
%%====================================================================

ensure_server(Realm, Id, S) ->
    case hecate_pubsub_server:start_link(
            #{realm => Realm, identity => Id}) of
        {ok, Pid} ->
            {ok, Pid, S#state{
                by_realm = (S#state.by_realm)#{Realm => Pid},
                by_pid   = (S#state.by_pid)#{Pid => Realm}}};
        {error, R} ->
            {error, R, S}
    end.

drop_pid(Pid, S) ->
    case maps:find(Pid, S#state.by_pid) of
        {ok, Realm} -> drop_realm(Realm, S);
        error       -> S
    end.

drop_realm(Realm, S) ->
    Pid = maps:get(Realm, S#state.by_realm, undefined),
    S#state{
        by_realm = maps:remove(Realm, S#state.by_realm),
        by_pid   = drop_pid_entry(Pid, S#state.by_pid)
    }.

drop_pid_entry(undefined, ByPid) -> ByPid;
drop_pid_entry(Pid, ByPid)       -> maps:remove(Pid, ByPid).
