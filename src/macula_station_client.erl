%% @doc Station-client — outbound RPC + pubsub over `macula_peering'.
%%
%% A `macula_station_client' is a `gen_server' that owns one
%% `macula_peering' connection to a single station endpoint. It
%% drives the CONNECT/HELLO handshake as the client side, then
%% exposes two surfaces over the same peering pipe:
%%
%% <ul>
%%   <li><strong>Request/response</strong> — `call/4' sends a CALL
%%       frame and matches inbound RESULT/ERROR frames against
%%       pending callers using the 16-byte CALL id. Convenience
%%       wrappers cover `_dht.put_record', `_dht.find_record', and
%%       `_dht.find_records_by_type'.</li>
%%   <li><strong>Streaming subscribe</strong> — `subscribe/3' sends
%%       a SUBSCRIBE frame and registers a delivery pid. Inbound
%%       EVENT frames matching the topic fan out to subscribers as
%%       `{macula_event, SubRef, Topic, Payload, Meta}'. On
%%       disconnect each subscriber receives a single
%%       `{macula_event_gone, SubRef, Reason}'.</li>
%% </ul>
%%
%% == Lifecycle ==
%%
%% <ol>
%%   <li>`start_link/1' — spawn worker, schedule connect.</li>
%%   <li>`connect_now/1' (cast) — build connect opts, call
%%       `macula_peering:connect/1', store the worker pid.</li>
%%   <li>Peering handshake completes → `{macula_peering, connected,
%%       Pid, PeerNodeId}' arrives → state moves to `connected'.</li>
%%   <li>`call/4' from caller → build CALL frame, sign happens inside
%%       peering, store `{from, deadline_timer}` keyed by CALL id, send
%%       frame via `macula_peering:send_frame/2'.</li>
%%   <li>RESULT or ERROR arrives as `{macula_peering, frame, Pid, Frame}'
%%       → look up `call_id', cancel timer, reply to caller.</li>
%%   <li>`{macula_peering, disconnected, Pid, Reason}' → fail all
%%       pending calls with `{error, {disconnected, Reason}}', notify
%%       all subscribers via `macula_event_gone', stop the client
%%       (caller is responsible for restart / reconnect).</li>
%% </ol>
%%
%% == Call reply taxonomy ==
%%
%% <table>
%%   <tr><th>Inbound frame</th><th>`call/4' returns</th></tr>
%%   <tr><td>RESULT(payload=`{error, Reason}')</td><td>`{ok, {error, Reason}}'</td></tr>
%%   <tr><td>RESULT(payload=Value)</td><td>`{ok, Value}'</td></tr>
%%   <tr><td>ERROR(code=C, name=N)</td><td>`{error, {call_error, C, N}}'</td></tr>
%%   <tr><td>(deadline elapses)</td><td>`{error, timeout}'</td></tr>
%%   <tr><td>(connection drops)</td><td>`{error, {disconnected, Reason}}'</td></tr>
%% </table>
%%
%% == Realm field ==
%%
%% Outbound CALL and SUBSCRIBE frames carry a 32-byte `realm' id.
%% Stations deployed today advertise an empty `realms' list
%% (realm-agnostic infrastructure) and do not enforce a realm match
%% on inbound frames — the dispatch path verifies the signature and
%% looks up the procedure / topic, nothing more. Callers therefore
%% pass any 32-byte value; this module defaults to all-zeros when no
%% realm is configured.
-module(macula_station_client).
-behaviour(gen_server).

-export([
    start_link/1,
    stop/1,
    call/4,
    put_record/2, put_record/3,
    find_record/2, find_record/3,
    find_records_by_type/2, find_records_by_type/3,
    subscribe/3,
    unsubscribe/2,
    is_connected/1,
    peer_node_id/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export_type([opts/0]).

-type url() :: binary() | string().

-type opts() :: #{
    %% Endpoint to dial. Either a URL (https://host:port) or a
    %% pre-parsed #{host, port} map.
    seed     := url() | #{host := binary() | string(),
                          port := inet:port_number()},
    %% Local Ed25519 keypair used to sign the CONNECT frame and any
    %% subsequent application frames. Auto-generated when absent.
    identity => macula_identity:key_pair(),
    %% 32-byte realm ID stamped on outbound CALL frames. Defaults to
    %% all-zeros for realm-agnostic clients.
    realm    => <<_:256>>,
    %% Capability bitfield announced in CONNECT (default 0).
    capabilities => non_neg_integer(),
    %% ALPN list passed through to QUIC (default [<<"macula">>]).
    alpn         => [binary()],
    %% Connect timeout in ms (default 30_000).
    connect_timeout_ms => non_neg_integer()
}.

-define(DEFAULT_REALM, <<0:256>>).
-define(DEFAULT_DEADLINE_MS, 5_000).
-define(CONNECT_RETRY_BACKOFF_MS, 1_000).

-record(state, {
    seed             :: #{host := binary() | string(),
                          port := inet:port_number()},
    identity         :: macula_identity:key_pair(),
    realm            :: <<_:256>>,
    capabilities     :: non_neg_integer(),
    alpn             :: [binary()],
    connect_timeout_ms :: non_neg_integer(),
    %% peering worker pid (`macula_peering_conn`). undefined while
    %% disconnected.
    peer_pid         :: pid() | undefined,
    %% peer's node id, set on `connected'.
    peer_node_id     :: macula_identity:pubkey() | undefined,
    %% map of CALL id (16 bytes) -> {From, TimerRef}.
    pending = #{}    :: #{<<_:128>> => {gen_server:from(), reference()}},
    %% Active topic subscriptions keyed by SubRef returned to the
    %% subscriber. The reverse `topic_index' lets inbound EVENT
    %% frames fan out to all SubRefs subscribed to a given topic
    %% without scanning the whole subscriptions map.
    subscriptions = #{} :: #{reference() => subscription()},
    topic_index   = #{} :: #{binary() => sets:set(reference())}
}).

-type subscription() :: {Topic     :: binary(),
                         Subscriber :: pid(),
                         Mon        :: reference()}.

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start a station-client connected to `seed'.
%% Returns once the gen_server is alive; the QUIC handshake completes
%% asynchronously. Use `is_connected/1' to poll readiness or just
%% issue `call/4' (which blocks the caller until ready or until its
%% timeout elapses).
-spec start_link(opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Issue a CALL frame and block until the station replies, the
%% deadline elapses, or the connection drops.
%%
%% `Procedure' is the V2 procedure name, e.g.
%% `<<"_dht.find_records_by_type">>'. `Payload' is any term that
%% `macula_frame:call/1' accepts (typically a map).
-spec call(pid(), binary(), term(), pos_integer()) ->
    {ok, term()} | {error, term()}.
call(Pid, Procedure, Payload, TimeoutMs)
  when is_pid(Pid), is_binary(Procedure),
       is_integer(TimeoutMs), TimeoutMs > 0 ->
    %% gen_server timeout = TimeoutMs + 500 to give the server time to
    %% report a clean `{error, timeout}' rather than the caller seeing
    %% a hard `exit({timeout, ...})'.
    GenTimeout = TimeoutMs + 500,
    try
        gen_server:call(Pid, {call, Procedure, Payload, TimeoutMs}, GenTimeout)
    catch
        exit:{timeout, _}      -> {error, timeout};
        exit:{noproc, _}       -> {error, noproc};
        exit:{normal, _}       -> {error, gone}
    end.

%% @doc Convenience wrapper for `_dht.put_record'. The record must be
%% a fully-signed `macula_record:record()' map (build via
%% `macula_record:envelope/3,4' + `macula_record:sign/2'). Returns
%% `ok' on success, `{error, Reason}' on RPC failure or unexpected
%% reply.
%%
%% Stations replicate the put across the K-nearest peers in their
%% Kademlia routing table, so a single `put_record/2' call against
%% any one connected station propagates to the rest of the DHT.
-spec put_record(pid(), map()) -> ok | {error, term()}.
put_record(Pid, Record) ->
    put_record(Pid, Record, ?DEFAULT_DEADLINE_MS).

-spec put_record(pid(), map(), pos_integer()) -> ok | {error, term()}.
put_record(Pid, Record, TimeoutMs) when is_pid(Pid), is_map(Record) ->
    classify_put(call(Pid, <<"_dht.put_record">>, Record, TimeoutMs)).

classify_put({ok, ok})       -> ok;
classify_put({ok, Other})    -> {error, {unexpected_reply, Other}};
classify_put({error, _} = E) -> E.

%% @doc Convenience wrapper for `_dht.find_record'. Looks up a record
%% by its `macula_record:storage_key/1' (32-byte BLAKE3 digest).
%% Returns `{error, not_found}' when no record exists at the key.
%% Callers SHOULD verify the returned record's signature with
%% `macula_record:verify/1' before trusting its payload.
-spec find_record(pid(), <<_:256>>) ->
    {ok, map()} | {error, not_found | term()}.
find_record(Pid, Key) ->
    find_record(Pid, Key, ?DEFAULT_DEADLINE_MS).

-spec find_record(pid(), <<_:256>>, pos_integer()) ->
    {ok, map()} | {error, not_found | term()}.
find_record(Pid, Key, TimeoutMs)
  when is_pid(Pid), is_binary(Key), byte_size(Key) =:= 32 ->
    classify_find(call(Pid, <<"_dht.find_record">>, #{key => Key}, TimeoutMs)).

classify_find({ok, #{type := _, payload := _, sig := _} = R}) -> {ok, R};
classify_find({ok, not_found})   -> {error, not_found};
classify_find({ok, Other})       -> {error, {unexpected_reply, Other}};
classify_find({error, _} = E)    -> E.

%% @doc Convenience wrapper for `_dht.find_records_by_type'. Returns
%% the decoded list of signed records (CBOR-decoded maps as produced
%% by `macula_record').
-spec find_records_by_type(pid(), 0..255) ->
    {ok, [map()]} | {error, term()}.
find_records_by_type(Pid, Type) ->
    find_records_by_type(Pid, Type, ?DEFAULT_DEADLINE_MS).

-spec find_records_by_type(pid(), 0..255, pos_integer()) ->
    {ok, [map()]} | {error, term()}.
find_records_by_type(Pid, Type, TimeoutMs)
  when is_integer(Type), Type >= 0, Type =< 255 ->
    classify_records(call(Pid, <<"_dht.find_records_by_type">>,
                          #{type => Type}, TimeoutMs)).

classify_records({ok, Records}) when is_list(Records) -> {ok, Records};
classify_records({ok, Other})                          -> {error, {unexpected_reply, Other}};
classify_records({error, _} = E)                       -> E.

%% @doc Subscribe to a peering pubsub topic. Sends a SUBSCRIBE frame
%% to the connected station and registers `Subscriber' as the
%% delivery pid for inbound EVENT frames matching `Topic'.
%%
%% Returns `{ok, SubRef}' once the SUBSCRIBE frame is sent. Stations
%% do not acknowledge SUBSCRIBE — the contract is best-effort,
%% mirroring the existing peering pubsub semantics.
%%
%% Subscriber receives one of:
%%
%% <ul>
%%   <li>`{macula_event, SubRef, Topic, Payload, Meta}' — every time
%%       an EVENT frame arrives for `Topic'. `Meta' is a map with
%%       `publisher', `seq', and `delivered_via' fields.</li>
%%   <li>`{macula_event_gone, SubRef, Reason}' — once, when the
%%       connection drops or the client stops. The subscription map
%%       is cleared on the same transition.</li>
%% </ul>
%%
%% The client monitors `Subscriber'; if it dies the subscription is
%% torn down (best-effort UNSUBSCRIBE on the wire).
-spec subscribe(pid(), binary(), pid()) ->
    {ok, reference()} | {error, not_connected}.
subscribe(Client, Topic, Subscriber)
  when is_pid(Client), is_binary(Topic), is_pid(Subscriber) ->
    gen_server:call(Client, {subscribe, Topic, Subscriber}, 5_000).

%% @doc Drop a subscription. Sends a best-effort UNSUBSCRIBE frame
%% to the station and clears local bookkeeping. Always returns `ok',
%% even when `SubRef' is unknown — unsubscribe is idempotent.
-spec unsubscribe(pid(), reference()) -> ok.
unsubscribe(Client, SubRef)
  when is_pid(Client), is_reference(SubRef) ->
    gen_server:call(Client, {unsubscribe, SubRef}, 5_000).

-spec is_connected(pid()) -> boolean().
is_connected(Pid) ->
    case gen_server:call(Pid, is_connected, 1_000) of
        true  -> true;
        false -> false
    end.

-spec peer_node_id(pid()) -> {ok, macula_identity:pubkey()} | {error, not_connected}.
peer_node_id(Pid) ->
    gen_server:call(Pid, peer_node_id, 1_000).

%%====================================================================
%% gen_server
%%====================================================================

init(Opts) ->
    Seed     = parse_seed(maps:get(seed, Opts)),
    Identity = maps:get(identity, Opts, macula_identity:generate()),
    Realm    = maps:get(realm, Opts, ?DEFAULT_REALM),
    Caps     = maps:get(capabilities, Opts, 0),
    Alpn     = maps:get(alpn, Opts, [<<"macula">>]),
    Tmo      = maps:get(connect_timeout_ms, Opts, 30_000),
    State    = #state{seed = Seed, identity = Identity, realm = Realm,
                      capabilities = Caps, alpn = Alpn,
                      connect_timeout_ms = Tmo},
    process_flag(trap_exit, true),
    self() ! attempt_connect,
    {ok, State}.

handle_call({call, _Proc, _Payload, _Tmo}, _From,
            #state{peer_pid = undefined} = S) ->
    {reply, {error, not_connected}, S};
handle_call({call, Proc, Payload, Tmo}, From,
            #state{peer_pid = Pid, identity = Id, realm = Realm,
                   pending = P} = S) ->
    CallId = crypto:strong_rand_bytes(16),
    Caller = macula_identity:public(Id),
    DeadlineMs = erlang:system_time(millisecond) + Tmo,
    Frame = macula_frame:call(#{
        call_id     => CallId,
        procedure   => Proc,
        realm       => Realm,
        payload     => Payload,
        deadline_ms => DeadlineMs,
        caller      => Caller
    }),
    ok = macula_peering:send_frame(Pid, Frame),
    TRef = erlang:send_after(Tmo, self(), {call_timeout, CallId}),
    {noreply, S#state{pending = P#{CallId => {From, TRef}}}};
handle_call(is_connected, _From, #state{peer_pid = undefined} = S) ->
    {reply, false, S};
handle_call(is_connected, _From, #state{peer_node_id = undefined} = S) ->
    {reply, false, S};
handle_call(is_connected, _From, S) ->
    {reply, true, S};
handle_call(peer_node_id, _From, #state{peer_node_id = undefined} = S) ->
    {reply, {error, not_connected}, S};
handle_call(peer_node_id, _From, #state{peer_node_id = Id} = S) ->
    {reply, {ok, Id}, S};

handle_call({subscribe, _Topic, _Sub}, _From,
            #state{peer_pid = undefined} = S) ->
    {reply, {error, not_connected}, S};
handle_call({subscribe, Topic, Subscriber}, _From,
            #state{peer_pid = Pid, identity = Id, realm = Realm,
                   subscriptions = Subs, topic_index = Idx} = S) ->
    SubRef = make_ref(),
    Mon    = erlang:monitor(process, Subscriber),
    SubKey = macula_identity:public(Id),
    Frame  = macula_frame:subscribe(#{topic      => Topic,
                                      realm      => Realm,
                                      subscriber => SubKey}),
    ok = macula_peering:send_frame(Pid, Frame),
    NewSubs = Subs#{SubRef => {Topic, Subscriber, Mon}},
    NewIdx  = add_topic_sub(Topic, SubRef, Idx),
    {reply, {ok, SubRef}, S#state{subscriptions = NewSubs,
                                  topic_index   = NewIdx}};

handle_call({unsubscribe, SubRef}, _From, S) ->
    {reply, ok, on_unsubscribe(SubRef, S)};

handle_call(_Req, _From, S) ->
    {reply, {error, unknown_call}, S}.

handle_cast(_Msg, S) -> {noreply, S}.

%%-------------------------------------------------------------------
%% Connect
%%-------------------------------------------------------------------

handle_info(attempt_connect, #state{seed = Seed, identity = Id, realm = Realm,
                                    capabilities = Caps, alpn = Alpn,
                                    connect_timeout_ms = Tmo} = S) ->
    Pub = macula_identity:public(Id),
    PeeringOpts = #{
        role            => client,
        target          => Seed#{alpn => Alpn, timeout_ms => Tmo},
        node_id         => Pub,
        identity        => Id,
        realms          => realms_for_connect(Realm),
        capabilities    => Caps,
        controlling_pid => self()
    },
    after_connect_request(macula_peering:connect(PeeringOpts), S);

handle_info({macula_peering, connected, Pid, PeerNodeId},
            #state{peer_pid = Pid} = S) ->
    {noreply, S#state{peer_node_id = PeerNodeId}};

handle_info({macula_peering, frame, Pid, Frame},
            #state{peer_pid = Pid} = S) ->
    {noreply, on_frame(Frame, S)};

handle_info({macula_peering, disconnected, Pid, Reason},
            #state{peer_pid = Pid} = S) ->
    NewS = fail_all_pending({disconnected, Reason}, S),
    %% Stop normally — the supervisor (or owning gen_server) decides
    %% whether to restart us.
    {stop, normal, NewS#state{peer_pid = undefined,
                              peer_node_id = undefined}};

handle_info({call_timeout, CallId}, #state{pending = P} = S) ->
    on_timeout(maps:take(CallId, P), S);

handle_info({'EXIT', Pid, Reason}, #state{peer_pid = Pid} = S) ->
    NewS = fail_all_pending({peering_exit, Reason}, S),
    {stop, normal, NewS#state{peer_pid = undefined,
                              peer_node_id = undefined}};

handle_info({'DOWN', Mon, process, _Pid, _Reason}, S) ->
    {noreply, on_subscriber_down(Mon, S)};

handle_info(_Other, S) ->
    {noreply, S}.

terminate(_Reason, #state{peer_pid = Pid}) when is_pid(Pid) ->
    catch macula_peering:close(Pid, client_stop),
    ok;
terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.

%%====================================================================
%% Internals
%%====================================================================

after_connect_request({ok, Pid}, S) ->
    link(Pid),
    {noreply, S#state{peer_pid = Pid}};
after_connect_request({error, Reason}, S) ->
    macula_diagnostics:event(<<"_macula.station_client.connect_failed">>, #{
        reason => Reason,
        seed   => S#state.seed
    }),
    erlang:send_after(?CONNECT_RETRY_BACKOFF_MS, self(), attempt_connect),
    {noreply, S}.

%% RESULT
on_frame(#{frame_type := result, call_id := CallId, payload := Payload},
         #state{pending = P} = S) ->
    deliver_pending(maps:take(CallId, P), {ok, Payload}, S);
%% ERROR
on_frame(#{frame_type := error, call_id := CallId} = Frame,
         #state{pending = P} = S) ->
    Code = maps:get(code, Frame, 0),
    Name = maps:get(name, Frame, undefined),
    deliver_pending(maps:take(CallId, P),
                    {error, {call_error, Code, Name}}, S);
%% EVENT — pubsub delivery. Fan out to every subscriber whose topic
%% matches. Stations may push EVENTs without a prior SUBSCRIBE on
%% this connection (e.g. wildcard / catalog channels); silently drop
%% those.
on_frame(#{frame_type := event, topic := Topic} = Frame, S) ->
    deliver_event(Topic, Frame, S);
%% HyParView / Plumtree / SWIM / content frames pass through here.
%% This client cares only about call/result/error and event; the
%% rest is for dedicated overlay modules.
on_frame(_Frame, S) ->
    S.

deliver_pending(error, _Reply, S) ->
    %% Unknown call_id (race with timeout, or duplicate reply).
    S;
deliver_pending({{From, TRef}, NewP}, Reply, S) ->
    _ = erlang:cancel_timer(TRef),
    gen_server:reply(From, Reply),
    S#state{pending = NewP}.

on_timeout(error, S) ->
    {noreply, S};
on_timeout({{From, _OldTRef}, NewP}, S) ->
    gen_server:reply(From, {error, timeout}),
    {noreply, S#state{pending = NewP}}.

fail_all_pending(Reason, #state{pending = P, subscriptions = Subs} = S) ->
    maps:foreach(fun(_CallId, {From, TRef}) ->
        _ = erlang:cancel_timer(TRef),
        gen_server:reply(From, {error, Reason})
    end, P),
    maps:foreach(fun(SubRef, {_Topic, Subscriber, Mon}) ->
        erlang:demonitor(Mon, [flush]),
        Subscriber ! {macula_event_gone, SubRef, Reason}
    end, Subs),
    S#state{pending = #{}, subscriptions = #{}, topic_index = #{}}.

%%-------------------------------------------------------------------
%% Subscription helpers
%%-------------------------------------------------------------------

add_topic_sub(Topic, SubRef, Idx) ->
    Set = maps:get(Topic, Idx, sets:new()),
    Idx#{Topic => sets:add_element(SubRef, Set)}.

del_topic_sub(Topic, SubRef, Idx) ->
    on_set_after_del(Topic, sets:del_element(SubRef, maps:get(Topic, Idx, sets:new())), Idx).

on_set_after_del(Topic, Set, Idx) ->
    on_empty_set(sets:is_empty(Set), Topic, Set, Idx).

on_empty_set(true,  Topic, _Set, Idx) -> maps:remove(Topic, Idx);
on_empty_set(false, Topic,  Set, Idx) -> Idx#{Topic => Set}.

%% Drop a single subscription. Best-effort UNSUBSCRIBE on the wire
%% (drops silently when disconnected — the station prunes stale
%% subscribers eventually). Idempotent: unknown SubRef is a no-op.
on_unsubscribe(SubRef, #state{subscriptions = Subs,
                              topic_index   = Idx,
                              peer_pid      = Pid,
                              identity      = Id,
                              realm         = Realm} = S) ->
    on_unsubscribe_take(maps:take(SubRef, Subs), SubRef, Idx, Pid, Id, Realm, S).

on_unsubscribe_take(error, _SubRef, _Idx, _Pid, _Id, _Realm, S) ->
    S;
on_unsubscribe_take({{Topic, _Subscriber, Mon}, NewSubs},
                    SubRef, Idx, Pid, Id, Realm, S) ->
    erlang:demonitor(Mon, [flush]),
    NewIdx = del_topic_sub(Topic, SubRef, Idx),
    send_unsubscribe(Pid, Topic, Realm, Id),
    S#state{subscriptions = NewSubs, topic_index = NewIdx}.

send_unsubscribe(undefined, _Topic, _Realm, _Id) ->
    ok;
send_unsubscribe(Pid, Topic, Realm, Id) ->
    SubKey = macula_identity:public(Id),
    Frame  = macula_frame:unsubscribe(#{topic      => Topic,
                                        realm      => Realm,
                                        subscriber => SubKey}),
    catch macula_peering:send_frame(Pid, Frame),
    ok.

%% Subscriber pid died — find its SubRef(s) by monitor ref, drop
%% them. A pid can only have one subscription via one monitor, but
%% scan defensively.
on_subscriber_down(Mon, #state{subscriptions = Subs} = S) ->
    Found = maps:fold(fun
        (SubRef, {_T, _P, M}, Acc) when M =:= Mon -> [SubRef | Acc];
        (_, _, Acc) -> Acc
    end, [], Subs),
    lists:foldl(fun on_unsubscribe/2, S, Found).

%% Fan an EVENT frame out to every subscriber for that topic.
deliver_event(Topic, Frame, #state{topic_index = Idx} = S) ->
    deliver_event_to(maps:find(Topic, Idx), Topic, Frame, S),
    S.

deliver_event_to(error, _Topic, _Frame, _S) ->
    ok;
deliver_event_to({ok, Set}, Topic, Frame, #state{subscriptions = Subs}) ->
    Payload = maps:get(payload, Frame),
    Meta = #{publisher     => maps:get(publisher, Frame),
             seq           => maps:get(seq, Frame),
             delivered_via => maps:get(delivered_via, Frame, direct)},
    sets:fold(fun(SubRef, _) ->
        deliver_event_one(SubRef, Topic, Payload, Meta, Subs)
    end, ok, Set).

deliver_event_one(SubRef, Topic, Payload, Meta, Subs) ->
    fan_event(maps:find(SubRef, Subs), SubRef, Topic, Payload, Meta).

fan_event(error, _SubRef, _Topic, _Payload, _Meta) ->
    ok;
fan_event({ok, {_T, Subscriber, _Mon}}, SubRef, Topic, Payload, Meta) ->
    Subscriber ! {macula_event, SubRef, Topic, Payload, Meta},
    ok.

%%-------------------------------------------------------------------
%% Helpers
%%-------------------------------------------------------------------

%% A realm-agnostic client (realm = all-zeros) advertises an empty
%% realms list in CONNECT — the station spec uses an empty list to
%% mean "no realm membership claimed".
realms_for_connect(<<0:256>>) -> [];
realms_for_connect(R) when is_binary(R), byte_size(R) =:= 32 -> [R].

parse_seed(#{host := _, port := _} = Map) ->
    Map;
parse_seed(Url) when is_binary(Url) ->
    parse_seed(binary_to_list(Url));
parse_seed(Url) when is_list(Url) ->
    case uri_string:parse(Url) of
        #{host := H, port := P} when is_integer(P) ->
            #{host => list_to_binary(H), port => P};
        #{host := H, scheme := "https"} ->
            #{host => list_to_binary(H), port => 4433};
        _ ->
            error({invalid_seed_url, Url})
    end.
