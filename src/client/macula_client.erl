%% @doc `macula_client' — the canonical pool client.
%%
%% Holds N peering links to N stations and routes ops with
%% replication, subscription replay, and inbound-event dedup. Apps
%% don't manage individual `macula_station_link' workers; they call
%% `macula_client' (or the `macula' facade, which re-exports the
%% public surface).
%%
%% Per `PLAN_V2_PARITY' Q2 §1: pool is the canonical client handle.
%% A single-station link is an internal worker only.
%%
%% == Lifecycle ==
%%
%% <pre>
%% {ok, Pool} = macula_client:connect(Seeds, Opts).
%% ok          = macula_client:publish(Pool, Realm, Topic, Payload, #{}).
%% {ok, Sub}   = macula_client:subscribe(Pool, Realm, Topic, self(), #{}).
%% receive {macula_event, Sub, Topic, Payload, Meta} -> ... end.
%% ok          = macula_client:unsubscribe(Pool, Sub).
%% ok          = macula_client:close(Pool).
%% </pre>
%%
%% == Replication ==
%%
%% `publish/5' fans the PUBLISH frame to `replication_factor' (default
%% 1) currently-spawned links. **Partial success counts as success**
%% per `PLAN_V2_PARITY' §5.1.1: the call returns `ok' as soon as one
%% link accepts the frame; the others are best-effort. When zero
%% links are spawned the call returns
%% `{error, {transient, no_healthy_station}}'.
%%
%% `subscribe/5' applies to every spawned link. The pool delivers a
%% deduped event stream to the consumer regardless of which link
%% relayed any given EVENT.
%%
%% == Dedup ==
%%
%% Inbound EVENT frames are keyed by `(Realm, Publisher, Seq)' in an
%% ETS table owned by the pool. The table is swept every
%% `dedup_sweep_ms' (default 30s) for entries older than
%% `dedup_window_ms' (default 60s).
%%
%% == Replay ==
%%
%% When a link's process dies the pool monitor fires; the pool
%% schedules a respawn after ?LINK_RESPAWN_DELAY_MS (1s). On respawn,
%% the pool re-issues every currently-tracked (Realm, Topic)
%% subscription against the new link via the internal
%% macula_client_replay helper.
-module(macula_client).
-behaviour(gen_server).

-export([connect/2, close/1, child_spec/3]).
%% Internal API — called by `macula_pubsub' (and future surfaces).
-export([publish/5, subscribe/5, unsubscribe/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([pool/0, opts/0, seed/0]).

-type pool() :: pid().
-type seed() :: binary() | string()
              | #{host := binary() | string(),
                  port := inet:port_number()}.

-type opts() :: #{
    %% Shared identity for every link in the pool. Auto-generated
    %% when absent. Stations see the pool as a single peer (one
    %% pubkey across N links).
    identity         => macula_identity:key_pair(),
    %% How many links accept a single PUBLISH frame. Default 1.
    replication_factor => pos_integer(),
    %% Forwarded to every `macula_station_link' opt map.
    capabilities     => non_neg_integer(),
    alpn             => [binary()],
    connect_timeout_ms => pos_integer(),
    %% Inbound-EVENT dedup window in milliseconds. Default 60_000.
    dedup_window_ms  => non_neg_integer(),
    %% How often the dedup table is swept. Default 30_000.
    dedup_sweep_ms   => pos_integer()
}.

-define(DEFAULT_REPLICATION, 1).
-define(DEFAULT_DEDUP_WINDOW_MS, 60_000).
-define(DEFAULT_DEDUP_SWEEP_MS, 30_000).
-define(LINK_RESPAWN_DELAY_MS, 1_000).

-record(link_state, {
    seed     :: seed(),
    pid      :: pid() | undefined,
    mon      :: reference() | undefined
}).

-record(sub_spec, {
    realm      :: <<_:256>>,
    topic      :: binary(),
    subscriber :: pid(),
    mon        :: reference()
}).

-record(state, {
    seeds         :: [seed()],
    identity      :: macula_identity:key_pair(),
    link_opts     :: map(),
    replication   :: pos_integer(),
    dedup_window  :: non_neg_integer(),
    dedup_sweep   :: pos_integer(),
    %% seed → link_state
    links = #{}   :: #{seed() => #link_state{}},
    %% pool-owned SubRef → sub_spec
    subs = #{}    :: #{reference() => #sub_spec{}},
    %% {realm, topic} → set of pool-owned SubRefs
    topic_index = #{} :: #{{<<_:256>>, binary()} => sets:set(reference())},
    dedup_tab     :: ets:tid()
}).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Spawn a pool with one link per seed. Returns immediately;
%% link handshakes complete asynchronously. Publish/subscribe block
%% until at least one link is connected (or fail with
%% `{error, {transient, no_healthy_station}}' on the publish path).
-spec connect([seed()], opts()) -> {ok, pool()} | {error, term()}.
connect(Seeds, Opts) when is_list(Seeds), is_map(Opts) ->
    gen_server:start_link(?MODULE, {Seeds, Opts}, []).

%% @doc Stop the pool. Every subscriber receives a final
%% `{macula_event_gone, SubRef, pool_closed}' message; every link
%% terminates with the pool.
-spec close(pool()) -> ok.
close(Pool) ->
    gen_server:stop(Pool, normal, 5_000).

%% @doc OTP child spec — drop the pool into a caller's supervision
%% tree. `Id' is the supervisor child id.
-spec child_spec(term(), [seed()], opts()) -> supervisor:child_spec().
child_spec(Id, Seeds, Opts) ->
    #{id       => Id,
      start    => {?MODULE, connect, [Seeds, Opts]},
      restart  => permanent,
      shutdown => 5_000,
      type     => worker,
      modules  => [?MODULE]}.

%% @doc Publish a frame to `replication_factor' currently-spawned
%% links. Partial success = success. Realm is per-call (32 bytes).
-spec publish(pool(), <<_:256>>, binary(), term(), map()) ->
    ok | {error, term()}.
publish(Pool, Realm, Topic, Payload, Opts)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Topic),
       is_map(Opts) ->
    Timeout = maps:get(timeout_ms, Opts, 5_000),
    gen_server:call(Pool, {publish, Realm, Topic, Payload, Opts},
                    Timeout + 500).

%% @doc Subscribe `Subscriber' to `(Realm, Topic)'. The pool
%% subscribes every currently-spawned link and dedupes inbound
%% events before fan-out. Returns `{ok, SubRef}'; `Subscriber'
%% receives `{macula_event, SubRef, Topic, Payload, Meta}' for each
%% delivered event and `{macula_event_gone, SubRef, Reason}' once
%% when the pool closes or the subscriber pid dies.
-spec subscribe(pool(), <<_:256>>, binary(), pid(), map()) ->
    {ok, reference()}.
subscribe(Pool, Realm, Topic, Subscriber, Opts)
  when is_pid(Pool),
       is_binary(Realm), byte_size(Realm) =:= 32,
       is_binary(Topic), is_pid(Subscriber),
       is_map(Opts) ->
    gen_server:call(Pool, {subscribe, Realm, Topic, Subscriber, Opts},
                    5_000).

%% @doc Drop a subscription. Idempotent — unknown `SubRef' is a
%% no-op. The wire-level link subscription persists for the pool's
%% lifetime (one wire sub per `(Realm, Topic)' multiplexed across
%% local consumers); Phase 4 will tighten this.
-spec unsubscribe(pool(), reference()) -> ok.
unsubscribe(Pool, SubRef) when is_pid(Pool), is_reference(SubRef) ->
    gen_server:call(Pool, {unsubscribe, SubRef}, 5_000).

%%====================================================================
%% gen_server
%%====================================================================

init({Seeds, Opts}) ->
    process_flag(trap_exit, true),
    Identity = maps:get(identity, Opts, macula_identity:generate()),
    LinkOpts = #{
        identity           => Identity,
        capabilities       => maps:get(capabilities, Opts, 0),
        alpn               => maps:get(alpn, Opts, [<<"macula">>]),
        connect_timeout_ms => maps:get(connect_timeout_ms, Opts, 30_000)
    },
    DedupWindow = maps:get(dedup_window_ms, Opts, ?DEFAULT_DEDUP_WINDOW_MS),
    DedupSweep  = maps:get(dedup_sweep_ms, Opts, ?DEFAULT_DEDUP_SWEEP_MS),
    Replication = maps:get(replication_factor, Opts, ?DEFAULT_REPLICATION),
    DedupTab    = macula_client_dedup:new(),
    State0 = #state{seeds = Seeds, identity = Identity,
                    link_opts = LinkOpts, replication = Replication,
                    dedup_window = DedupWindow, dedup_sweep = DedupSweep,
                    dedup_tab = DedupTab},
    State1 = lists:foldl(fun start_link_for_seed/2, State0, Seeds),
    erlang:send_after(DedupSweep, self(), dedup_sweep),
    {ok, State1}.

handle_call({publish, Realm, Topic, Payload, _Opts}, _From, S) ->
    Targets = spawned_link_pids(S),
    N = min(length(Targets), S#state.replication),
    Selected = lists:sublist(Targets, N),
    Results = [macula_station_link:publish(P, Realm, Topic, Payload)
               || P <- Selected],
    {reply, summarize_publish(Results, Targets), S};

handle_call({subscribe, Realm, Topic, Subscriber, _Opts}, _From, S) ->
    SubRef = make_ref(),
    Mon = erlang:monitor(process, Subscriber),
    Spec = #sub_spec{realm = Realm, topic = Topic,
                     subscriber = Subscriber, mon = Mon},
    Key = {Realm, Topic},
    AlreadyTracked = maps:is_key(Key, S#state.topic_index),
    NewS = register_sub(SubRef, Spec, S),
    issue_wire_subs(AlreadyTracked, Realm, Topic, NewS),
    {reply, {ok, SubRef}, NewS};

handle_call({unsubscribe, SubRef}, _From, S) ->
    {reply, ok, drop_sub(SubRef, S)};

handle_call(_Req, _From, S) ->
    {reply, {error, unknown_call}, S}.

handle_cast(_Msg, S) -> {noreply, S}.

handle_info({macula_event, _LinkSubRef, Topic, Payload, Meta}, S) ->
    Realm     = maps:get(realm, Meta, <<0:256>>),
    Publisher = maps:get(publisher, Meta),
    Seq       = maps:get(seq, Meta),
    on_inbound_event(macula_client_dedup:check(S#state.dedup_tab,
                                               Realm, Publisher, Seq),
                     Realm, Topic, Payload, Meta, S);

handle_info({macula_event_gone, _LinkSubRef, _Reason}, S) ->
    %% A link torn down its subscription end. Pool will respawn the
    %% link via the DOWN handler and replay subs. Don't propagate to
    %% local consumers — they see a continuous stream.
    {noreply, S};

handle_info({'DOWN', Mon, process, Pid, Reason}, S) ->
    on_down(Mon, Pid, Reason, S);

handle_info({respawn_link, Seed}, S) ->
    {noreply, on_respawn_link(Seed, S)};

handle_info(dedup_sweep, S) ->
    _ = macula_client_dedup:sweep(S#state.dedup_tab, S#state.dedup_window),
    erlang:send_after(S#state.dedup_sweep, self(), dedup_sweep),
    {noreply, S};

handle_info({'EXIT', _Pid, _Reason}, S) ->
    %% Links are linked to us via gen_server:start_link in
    %% start_link_for_seed (we trap_exit). The DOWN monitor fires
    %% alongside; that path handles cleanup. Drop the EXIT.
    {noreply, S};

handle_info(_Other, S) ->
    {noreply, S}.

terminate(_Reason, #state{subs = Subs}) ->
    %% Notify every subscriber that the pool is gone.
    maps:foreach(
      fun(SubRef, #sub_spec{subscriber = Pid, mon = Mon}) ->
          erlang:demonitor(Mon, [flush]),
          Pid ! {macula_event_gone, SubRef, pool_closed}
      end, Subs),
    ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.

%%====================================================================
%% Internals — link lifecycle
%%====================================================================

start_link_for_seed(Seed, S) ->
    LinkOpts = (S#state.link_opts)#{seed => Seed},
    after_link_start(macula_station_link:start_link(LinkOpts), Seed, S).

after_link_start({ok, Pid}, Seed, S) ->
    Mon = erlang:monitor(process, Pid),
    LinkState = #link_state{seed = Seed, pid = Pid, mon = Mon},
    S#state{links = (S#state.links)#{Seed => LinkState}};
after_link_start({error, Reason}, Seed, S) ->
    macula_diagnostics:event(<<"_macula.client.link_start_failed">>,
                             #{seed => Seed, reason => Reason}),
    erlang:send_after(?LINK_RESPAWN_DELAY_MS, self(), {respawn_link, Seed}),
    Empty = #link_state{seed = Seed, pid = undefined, mon = undefined},
    S#state{links = (S#state.links)#{Seed => Empty}}.

spawned_link_pids(#state{links = Links}) ->
    [P || #link_state{pid = P} <- maps:values(Links), is_pid(P)].

on_respawn_link(Seed, S) ->
    NewS = start_link_for_seed(Seed, S),
    replay_to_seed(maps:get(Seed, NewS#state.links, undefined), NewS).

replay_to_seed(#link_state{pid = Pid}, S) when is_pid(Pid) ->
    macula_client_replay:subs_to(Pid, S#state.topic_index),
    S;
replay_to_seed(_, S) ->
    S.

%%====================================================================
%% Internals — DOWN routing (link vs subscriber)
%%====================================================================

on_down(Mon, Pid, Reason, S) ->
    on_down_routed(find_link_by_mon(Mon, S), Mon, Pid, Reason, S).

on_down_routed({ok, Seed}, _Mon, Pid, Reason, S) ->
    macula_diagnostics:event(<<"_macula.client.link_down">>,
                             #{seed => Seed, pid => Pid, reason => Reason}),
    erlang:send_after(?LINK_RESPAWN_DELAY_MS, self(), {respawn_link, Seed}),
    {noreply, S#state{links = maps:remove(Seed, S#state.links)}};
on_down_routed(error, Mon, _Pid, _Reason, S) ->
    {noreply, on_subscriber_down(Mon, S)}.

find_link_by_mon(Mon, #state{links = Links}) ->
    case [Seed || {Seed, #link_state{mon = M}} <- maps:to_list(Links),
                  M =:= Mon] of
        [Seed | _] -> {ok, Seed};
        []         -> error
    end.

on_subscriber_down(Mon, #state{subs = Subs} = S) ->
    Found = [SubRef || {SubRef, #sub_spec{mon = M}}
                       <- maps:to_list(Subs), M =:= Mon],
    lists:foldl(fun drop_sub/2, S, Found).

%%====================================================================
%% Internals — subscription bookkeeping
%%====================================================================

register_sub(SubRef, #sub_spec{realm = R, topic = T} = Spec,
             #state{subs = Subs, topic_index = Idx} = S) ->
    Key = {R, T},
    Set = maps:get(Key, Idx, sets:new()),
    NewIdx  = Idx#{Key => sets:add_element(SubRef, Set)},
    NewSubs = Subs#{SubRef => Spec},
    S#state{subs = NewSubs, topic_index = NewIdx}.

drop_sub(SubRef, #state{subs = Subs} = S) ->
    drop_sub_take(maps:take(SubRef, Subs), SubRef, S).

drop_sub_take(error, _SubRef, S) ->
    S;
drop_sub_take({#sub_spec{realm = R, topic = T, mon = Mon}, NewSubs},
              SubRef, #state{topic_index = Idx} = S) ->
    erlang:demonitor(Mon, [flush]),
    Key = {R, T},
    NewSet = sets:del_element(SubRef, maps:get(Key, Idx, sets:new())),
    NewIdx = on_index_after_drop(sets:is_empty(NewSet), Key, NewSet, Idx),
    S#state{subs = NewSubs, topic_index = NewIdx}.

on_index_after_drop(true,  Key, _Set, Idx) -> maps:remove(Key, Idx);
on_index_after_drop(false, Key,  Set, Idx) -> Idx#{Key => Set}.

issue_wire_subs(true, _Realm, _Topic, _S) ->
    %% A sibling consumer already triggered the wire-level subscribe;
    %% the pool fans out to every local SubRef on inbound EVENT.
    ok;
issue_wire_subs(false, Realm, Topic, S) ->
    PoolPid = self(),
    [_ = macula_station_link:subscribe(P, Realm, Topic, PoolPid)
     || P <- spawned_link_pids(S)],
    ok.

%%====================================================================
%% Internals — inbound event fan-out
%%====================================================================

on_inbound_event(duplicate, _Realm, _Topic, _Payload, _Meta, S) ->
    {noreply, S};
on_inbound_event(new, Realm, Topic, Payload, Meta, S) ->
    fan_to_local(Realm, Topic, Payload, Meta, S),
    {noreply, S}.

fan_to_local(Realm, Topic, Payload, Meta,
             #state{topic_index = Idx, subs = Subs}) ->
    fan_to_set(maps:find({Realm, Topic}, Idx), Topic, Payload, Meta, Subs).

fan_to_set(error, _Topic, _Payload, _Meta, _Subs) ->
    ok;
fan_to_set({ok, Set}, Topic, Payload, Meta, Subs) ->
    sets:fold(fun(SubRef, _) ->
        deliver_one(SubRef, Topic, Payload, Meta, Subs)
    end, ok, Set).

deliver_one(SubRef, Topic, Payload, Meta, Subs) ->
    deliver_to(maps:find(SubRef, Subs), SubRef, Topic, Payload, Meta).

deliver_to(error, _SubRef, _Topic, _Payload, _Meta) ->
    ok;
deliver_to({ok, #sub_spec{subscriber = Pid}}, SubRef, Topic, Payload, Meta) ->
    Pid ! {macula_event, SubRef, Topic, Payload, Meta},
    ok.

%%====================================================================
%% Internals — publish summary
%%====================================================================

summarize_publish([], []) ->
    {error, {transient, no_healthy_station}};
summarize_publish([], _NotEmpty) ->
    %% Replication factor capped at 0 by config; treat as no-op ok.
    ok;
summarize_publish(Results, _Targets) ->
    on_publish_results(lists:any(fun(R) -> R =:= ok end, Results), Results).

on_publish_results(true,  _Results)        -> ok;
on_publish_results(false, [First | _])     -> First;
on_publish_results(false, [])              -> {error, no_publish_attempts}.
