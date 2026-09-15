%% @doc Per-identity registry for `hecate_pubsub_server' processes.
%%
%% Holds a `RealmTag => pid()' map and acts as the dispatch hub for
%% inbound SUBSCRIBE / UNSUBSCRIBE / EVENT frames. The registry
%% spawn-links one `hecate_pubsub_server' worker per realm and stores
%% its pid. A linked worker that crashes delivers an `'EXIT'' message
%% which the registry traps + uses to clear the entry; a later
%% `register/3' or SUBSCRIBE yields a fresh server.
%%
%% == A realm's server lives while something holds it ==
%%
%% A realm gets a server in two ways. `register/3' materialises it for
%% the station's own use, such as a realm the station publishes on; such
%% a realm is pinned and stays until the registry stops. With a
%% `default_identity', a SUBSCRIBE for a realm without a server
%% materialises one; that realm lives while a subscription holds it, and
%% when an UNSUBSCRIBE or `purge_subscriber/2' takes its last one, its
%% server stops and its place frees. Nothing else starts a server: an
%% UNSUBSCRIBE or EVENT for a realm without one gets `{ok, []}', and
%% `relay_publish/3' builds its EVENT without one.
%%
%% At most `max_subscribed_realms' realms (1000 by default) are
%% materialised by SUBSCRIBE at once. A SUBSCRIBE that would take the
%% registry past that gets `{error, too_many_realms}' and starts no
%% server. Pinned realms do not count, so `register/3' is for realms the
%% station itself chooses, never for a realm a peer names.
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
    purge_subscriber/2,
    stop/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, format_status/1]).

-export_type([opts/0, realm/0, identity/0]).

-define(MAX_SUBSCRIBED_REALMS, 1000).

-type realm()    :: <<_:256>>.
-type identity() :: macula_node_keys:node_key().

-type opts() :: #{
    %% Default identity used when a `register/3' caller does not
    %% pass one explicitly. Optional — passing identity per-call
    %% gives the same behaviour as the pre-Phase-2 API. With it, a
    %% SUBSCRIBE for a realm without a server materialises one.
    identity     => identity(),
    %% Phase 6 (operational tooling): when supplied, the registry
    %% sets `logger:set_process_metadata(#{identity_id =&gt; Key})'
    %% on init so every log line from the registry process — and
    %% from the pubsub_servers it spawn-links — carries the
    %% identity for grep-friendly diagnostics on a multi-identity
    %% box.
    identity_key => term(),
    %% The most realms SUBSCRIBE frames may have materialised at once,
    %% 1000 by default. Realms `register/3' pinned do not count.
    max_subscribed_realms => pos_integer()
}.

-record(state, {
    default_identity      :: identity() | undefined,
    max_subscribed_realms :: pos_integer(),
    by_realm = #{}        :: #{realm() => pid()},
    by_pid   = #{}        :: #{pid() => realm()},
    %% Realms `register/3' materialised or took over: they stay without
    %% subscriptions and do not count towards the maximum. A pin outlives
    %% the realm's server, so a pinned realm may have no server here.
    pinned   = #{}        :: #{realm() => true}
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
%%
%% The realm is pinned: it stays when its last subscription leaves, and
%% it does not count towards `max_subscribed_realms'. It stays pinned when
%% its server stops, so the server the next `register/3' or SUBSCRIBE starts
%% for it is pinned too. Register only a
%% realm the station itself chooses, such as one it publishes on, never
%% a realm a peer names, or peers could grow the registry past its
%% maximum.
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
%% (empty list for SUBSCRIBE / UNSUBSCRIBE).
%%
%% For a realm without a server: when no `default_identity' was
%% configured at start-up, every frame gets `{error, not_found}'. With
%% one (the production path under `macula_station_identity_sup'), a
%% SUBSCRIBE materialises the realm and is dispatched against the fresh
%% server, or gets `{error, too_many_realms}' when SUBSCRIBE frames
%% already hold `max_subscribed_realms' realms; an UNSUBSCRIBE or EVENT
%% gets `{ok, []}' and starts nothing.
%%
%% An UNSUBSCRIBE that takes the last subscription of a realm a
%% SUBSCRIBE materialised stops that realm's server.
-spec dispatch_frame(pid(), realm(), <<_:256>>, macula_frame:frame()) ->
        {ok, [<<_:256>>]} | {error, not_found | too_many_realms}.
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
%% For a realm without a server and a `default_identity' set (the
%% production path under `macula_station_identity_sup'), the EVENT is
%% still built, signed with that identity, with no local subscribers.
%% This keeps the EVENT available for publisher-side bloom-fan
%% forwarding to peer stations that have the topic in their Bloom filter
%% but no subscribe-on-peer chain terminating at us, and starts no
%% process for a realm nobody here subscribes to. Tests that omit
%% `default_identity' retain the strict `{error, not_found}' semantics.
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

%% @doc Remove `Sub' (a subscriber pubkey) from every topic under
%% every realm currently materialised on this registry — i.e. from
%% every live `hecate_pubsub_server' it owns. A dead server pid
%% (raced against its own `EXIT' cleanup, see `handle_info/2') is
%% skipped rather than treated as an error; the registry's own
%% `by_realm'/`by_pid' bookkeeping self-heals on the pending `EXIT'.
%% A realm a SUBSCRIBE materialised whose last subscription the purge
%% took has its server stopped.
%%
%% Intended caller: the station's peer/daemon connection-lifecycle
%% path, once a `NodeId' is confirmed to have no remaining
%% connection. `Sub' has no notion of "which realm" it was
%% subscribed under, so this fans out to all of them rather than
%% requiring the caller to know.
-spec purge_subscriber(pid(), <<_:256>>) -> ok.
purge_subscriber(RegistryPid, <<_:256>> = Sub) ->
    gen_server:call(RegistryPid, {purge_subscriber, Sub}).

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
    {ok, #state{default_identity      = maps:get(identity, Opts, undefined),
                max_subscribed_realms = maps:get(max_subscribed_realms, Opts,
                                                 ?MAX_SUBSCRIBED_REALMS)}}.

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
handle_call({purge_subscriber, Sub}, _From, S) ->
    {reply, ok, lists:foldl(fun({Realm, Pid}, Acc) -> purge_one(Realm, Pid, Sub, Acc) end,
                            S, maps:to_list(S#state.by_realm))};
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

%% Status output and crash reports show this process's keys with their private halves redacted.
format_status(Status) ->
    macula_node_keys:redacted(Status).

%%====================================================================
%% Helpers
%%====================================================================

do_register_call(Realm, Identity, {ok, Pid}, S) ->
    handle_existing(Realm, Pid, Identity, is_process_alive(Pid), S);
do_register_call(Realm, Identity, error, S) ->
    do_spawn_server(Realm, Identity, S).

handle_existing(Realm, Pid, _Identity, true, S) ->
    {reply, {ok, Pid}, pin(Realm, S)};
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
     pin(Realm, S#state{by_realm = (S#state.by_realm)#{Realm => Pid},
                        by_pid   = (S#state.by_pid)#{Pid => Realm}})};
on_server_started(_Realm, {error, _} = E, S) ->
    {reply, E, S}.

pin(Realm, #state{pinned = Pinned} = S) ->
    S#state{pinned = Pinned#{Realm => true}}.

lookup_reply({ok, Pid}) -> {ok, Pid};
lookup_reply(error)     -> {error, not_found}.

do_dispatch(_Realm, _From, _Frame, error,
            #state{default_identity = undefined} = S) ->
    {reply, {error, not_found}, S};
do_dispatch(Realm, From, #{frame_type := subscribe} = Frame, error, S) ->
    %% Only a SUBSCRIBE materialises a realm, and only while SUBSCRIBE
    %% frames hold fewer realms than the maximum.
    materialise_for_subscribe(has_room_for_a_subscribed_realm(Realm, S), Realm, From, Frame, S);
do_dispatch(_Realm, _From, _Frame, error, S) ->
    %% An UNSUBSCRIBE or EVENT for a realm with no server has nothing to
    %% act on, and starts none.
    {reply, {ok, []}, S};
do_dispatch(Realm, From, Frame, {ok, Pid}, S) ->
    forward_frame(Realm, Pid, From, Frame, S).

materialise_for_subscribe(false, _Realm, _From, _Frame, S) ->
    {reply, {error, too_many_realms}, S};
materialise_for_subscribe(true, Realm, From, Frame, #state{default_identity = Id} = S) ->
    on_auto_registered(ensure_server(Realm, Id, S), Realm, From, Frame).

%% The realms SUBSCRIBE frames hold are the served realms that are not
%% pinned. A pin outlives the realm's server, so pinned realms are left out
%% by name, not by count, and a SUBSCRIBE for a pinned realm always has room:
%% a pinned realm never counts, with or without a server.
has_room_for_a_subscribed_realm(Realm, #state{by_realm = ByRealm, pinned = Pinned,
                                              max_subscribed_realms = Max}) ->
    is_map_key(Realm, Pinned) orelse
        map_size(maps:without(maps:keys(Pinned), ByRealm)) < Max.

on_auto_registered({ok, Pid, S}, Realm, From, Frame) ->
    forward_frame(Realm, Pid, From, Frame, S);
on_auto_registered({error, Reason, S}, _Realm, _From, _Frame) ->
    {reply, {error, Reason}, S}.

forward_frame(Realm, Pid, From, Frame, S) ->
    try hecate_pubsub_server:process_frame(Pid, From, Frame) of
        Subs -> {reply, {ok, Subs}, reap_if_empty(is_unsubscribe(Frame), Realm, Pid, S)}
    catch
        exit:{noproc, _} ->
            {reply, {error, not_found}, drop_realm(Realm, S)}
    end.

is_unsubscribe(#{frame_type := unsubscribe}) -> true;
is_unsubscribe(_Frame)                       -> false.

%%====================================================================
%% Internals — a realm lives while a subscription holds it
%%====================================================================

%% After an UNSUBSCRIBE or a purge that reached the realm's server: a
%% realm a SUBSCRIBE materialised whose server holds no subscription any
%% more has its server stopped, which frees its place. A pinned realm
%% stays.
reap_if_empty(false, _Realm, _Pid, S) ->
    S;
reap_if_empty(true, Realm, Pid, #state{pinned = Pinned} = S) ->
    reap(is_map_key(Realm, Pinned) orelse holds_subscriptions(Pid), Realm, Pid, S).

reap(true, _Realm, _Pid, S) ->
    S;
reap(false, Realm, Pid, S) ->
    ok = stop_server(Pid),
    drop_realm(Realm, S).

holds_subscriptions(Pid) ->
    try hecate_pubsub_server:topic_count(Pid) > 0
    catch exit:{noproc, _} -> false
    end.

%% The server is linked to this process, which traps exits: its `EXIT'
%% arrives after the realm is already dropped, and changes nothing.
stop_server(Pid) ->
    try hecate_pubsub_server:stop(Pid)
    catch exit:_Gone -> ok
    end.

%%====================================================================
%% Internals — relay_publish
%%====================================================================

do_relay_publish(_Realm, _Frame, error,
                 #state{default_identity = undefined} = S) ->
    {reply, {error, not_found}, S};
do_relay_publish(Realm, Frame, error,
                 #state{default_identity = Id} = S) ->
    %% No server for the realm: the EVENT is still built, signed by the
    %% station, so the caller (pubsub_dispatcher) can fan it out to peer
    %% stations whose Bloom filter matches the topic, but no process
    %% starts for a realm nobody here subscribes to, and there is no
    %% local subscriber to match.
    {reply, relay_without_server(Realm, Frame, Id), S};
do_relay_publish(Realm, Frame, {ok, Pid}, S) ->
    forward_relay_publish(Realm, Pid, Frame, S).

relay_without_server(Realm, #{frame_type := publish, realm := Realm} = Frame, Id) ->
    {ok, hecate_pubsub_server:relay_event(Frame, Id), []};
relay_without_server(_Realm, _Frame, _Id) ->
    {error, realm_mismatch}.

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
%% Internals — ensure_server (materialising a realm for a SUBSCRIBE)
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

%% A dropped realm's server entries go and its pin stays: a pin records the
%% station's own choice and lasts until the registry stops, whatever happens
%% to the realm's server.
drop_realm(Realm, S) ->
    Pid = maps:get(Realm, S#state.by_realm, undefined),
    S#state{
        by_realm = maps:remove(Realm, S#state.by_realm),
        by_pid   = drop_pid_entry(Pid, S#state.by_pid)
    }.

drop_pid_entry(undefined, ByPid) -> ByPid;
drop_pid_entry(Pid, ByPid)       -> maps:remove(Pid, ByPid).

%%====================================================================
%% Internals — purge_subscriber
%%====================================================================

purge_one(Realm, Pid, Sub, S) ->
    reap_if_empty(purged(Pid, Sub), Realm, Pid, S).

%% Whether the purge reached a live server, which may then hold no
%% subscription any more.
purged(Pid, Sub) ->
    try hecate_pubsub_server:purge_subscriber(Pid, Sub) of
        ok -> true
    catch exit:{noproc, _} -> false
    end.
