%%%-------------------------------------------------------------------
%%% @doc Macula Mesh Client — connects a node to the relay mesh via QUIC.
%%%
%%% Single gen_server managing ONE persistent QUIC connection to a relay
%%% with automatic failover across multiple relays. On disconnect, cycles
%%% to the next relay in the list with exponential backoff + jitter.
%%% Replays all subscriptions and procedure registrations on reconnect.
%%%
%%% This is the SDK client module. For relay-to-relay peering connections,
%%% see macula_relay_client in the macula-relay repository.
%%%
%%% Usage:
%%%
%%% ```
%%% {ok, Client} = macula_mesh_client:start_link(#{
%%%     relays => [<<"quic://boot.macula.io:443">>],
%%%     realm => <<"io.macula">>
%%% }).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_mesh_client).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1, stop/1]).
-export([subscribe/3, unsubscribe/2, publish/3]).
-export([advertise/3, unadvertise/2, call/4, call/5, async_call/7]).
-export([is_connected/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Test exports
-ifdef(TEST).
-export([backoff_ms/1, parse_url/1]).
-endif.

-define(RECONNECT_BASE_MS, 1000).
-define(RECONNECT_MAX_MS, 30000).
-define(RECONNECT_JITTER, 0.3).
-define(CALL_TIMEOUT, 5000).
-define(PING_INTERVAL_MS, 30000).
-define(PING_TOPIC, <<"_mesh.relay.ping">>).

-record(state, {
    relays :: [binary()],                                    %% all configured relay URLs
    relay_index :: non_neg_integer(),                         %% current relay (0-based)
    url :: binary(),                                         %% current relay URL
    host :: string(),
    port :: integer(),
    realm :: binary(),
    identity :: binary(),
    geo_identity :: map(),                                    %% geo metadata for CONNECT (city, country, lat, lng)
    site :: map() | undefined,                                %% site metadata for CONNECT (site_id, name, city, lat, lng, site_type)
    client_type = <<"node">> :: binary(),                      %% always "node" for mesh clients
    tls_verify :: verify_peer | none,                         %% TLS verification mode (none for relay peering)
    preferred_host :: string(),                               %% first relay hostname (for target_relay in CONNECT)
    previous_host :: string() | undefined,                    %% host before failover (for reroute detection)
    conn :: reference() | undefined,
    stream :: reference() | undefined,
    status :: connecting | connected | disconnected,
    recv_buffer :: binary(),
    reconnect_attempt :: non_neg_integer(),                   %% for exponential backoff
    %% Application state (survives reconnects)
    subscriptions :: #{reference() => {binary(), fun()}},     %% ref => {topic, callback}
    procedures :: #{binary() => fun()},                       %% procedure => handler
    pending_calls :: #{binary() => {pid(), reference()}},     %% call_id => {from, timer_ref}
    ping_sent_at :: integer() | undefined,                    %% erlang:monotonic_time(millisecond) when PING sent
    last_rtt_ms :: non_neg_integer() | undefined              %% most recent RTT measurement
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

-spec is_connected(pid()) -> boolean().
is_connected(Pid) ->
    gen_server:call(Pid, is_connected).

-spec subscribe(pid(), binary(), fun((map()) -> ok)) -> {ok, reference()}.
subscribe(Pid, Topic, Callback) ->
    gen_server:call(Pid, {subscribe, Topic, Callback}).

-spec unsubscribe(pid(), reference()) -> ok.
unsubscribe(Pid, Ref) ->
    gen_server:call(Pid, {unsubscribe, Ref}).

-spec publish(pid(), binary(), binary() | map()) -> ok.
publish(Pid, Topic, Payload) ->
    gen_server:cast(Pid, {publish, Topic, Payload}).

-spec advertise(pid(), binary(), fun((map()) -> {ok, term()} | {error, term()})) -> {ok, reference()}.
advertise(Pid, Procedure, Handler) ->
    gen_server:call(Pid, {advertise, Procedure, Handler}).

-spec unadvertise(pid(), binary()) -> ok.
unadvertise(Pid, Procedure) ->
    gen_server:call(Pid, {unadvertise, Procedure}).

-spec call(pid(), binary(), map(), timeout()) -> {ok, term()} | {error, term()}.
call(Pid, Procedure, Args, Timeout) ->
    call(Pid, Procedure, Args, Timeout, #{}).

-spec call(pid(), binary(), map(), timeout(), map()) -> {ok, term()} | {error, term()}.
call(Pid, Procedure, Args, Timeout, Opts) ->
    gen_server:call(Pid, {rpc_call, Procedure, Args, Timeout, Opts}, Timeout + 1000).

%% @doc Fire-and-forget RPC call. Sends CALL to the relay and delivers the reply
%% as {relay_reply, CorrelationId, Result, Trace} to CallbackPid.
%% Does NOT block the caller. Used for cross-relay RPC forwarding.
-spec async_call(pid(), binary(), map(), timeout(), map(), pid(), binary()) -> ok.
async_call(Pid, Procedure, Args, Timeout, Opts, CallbackPid, CorrelationId) ->
    gen_server:cast(Pid, {async_rpc_call, Procedure, Args, Timeout, Opts, CallbackPid, CorrelationId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    Relays = case maps:find(relays, Opts) of
        {ok, List} when is_list(List), length(List) > 0 -> List;
        _ -> [maps:get(url, Opts, <<"https://localhost:4433">>)]
    end,
    %% First relay is the preferred target (e.g., nearest virtual relay identity).
    %% Remaining entries are ordered fallbacks for failover — not load balancing.
    Index = 0,
    Url = hd(Relays),
    {Host, Port} = parse_url(Url),
    Realm = maps:get(realm, Opts, <<"io.macula">>),
    Identity = maps:get(identity, Opts, <<"anonymous">>),
    GeoIdentity = maps:get(geo_identity, Opts, #{}),
    Site = maps:get(site, Opts, undefined),
    %% Client type governs how the receiving relay classifies this
    %% connection. `<<"relay">>' → is_peer=true: SWIM/DHT/relay.ping
    %% protocol messages are accepted, publishes deliver to
    %% {relay_local,T} only (loop prevention). `<<"node">>' (default)
    %% is the normal application client profile.
    %%
    %% macula_relay_peering passes type=<<"relay">> for peer_clients.
    %% Previously this option was silently ignored (hardcoded to
    %% <<"node">>), so peer SWIM/DHT messages were dropped by the
    %% is_peer=true guards in the relay handler and SWIM never
    %% round-tripped across relays.
    ClientType = maps:get(type, Opts, <<"node">>),
    TlsVerify = maps:get(tls_verify, Opts, verify_peer),

    State = #state{
        relays = Relays,
        relay_index = Index,
        url = Url,
        host = Host,
        preferred_host = Host,
        port = Port,
        realm = Realm,
        identity = Identity,
        geo_identity = GeoIdentity,
        site = Site,
        client_type = ClientType,
        tls_verify = TlsVerify,
        conn = undefined,
        stream = undefined,
        status = connecting,
        recv_buffer = <<>>,
        reconnect_attempt = 0,
        subscriptions = #{},
        procedures = #{},
        pending_calls = #{}
    },

    self() ! connect,
    {ok, State}.

%%====================================================================
%% Status
%%====================================================================

handle_call(is_connected, _From, #state{status = connected, stream = S} = State)
  when S =/= undefined ->
    {reply, true, State};
handle_call(is_connected, _From, State) ->
    {reply, false, State};

%%====================================================================
%% Subscribe / Unsubscribe
%%====================================================================

handle_call({subscribe, Topic, Callback}, _From, State) ->
    Ref = make_ref(),
    Subs = maps:put(Ref, {Topic, Callback}, State#state.subscriptions),
    %% Send SUBSCRIBE if connected
    maybe_send(subscribe, #{<<"topics">> => [Topic], <<"qos">> => 0}, State),
    {reply, {ok, Ref}, State#state{subscriptions = Subs}};

handle_call({unsubscribe, Ref}, _From, State) ->
    case maps:get(Ref, State#state.subscriptions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        {Topic, _Callback} ->
            Subs = maps:remove(Ref, State#state.subscriptions),
            maybe_send(unsubscribe, #{<<"topics">> => [Topic]}, State),
            {reply, ok, State#state{subscriptions = Subs}}
    end;

%%====================================================================
%% Advertise / Unadvertise
%%====================================================================

handle_call({advertise, Procedure, Handler}, _From, State) ->
    Procs = maps:put(Procedure, Handler, State#state.procedures),
    maybe_send(register_procedure, #{<<"procedure">> => Procedure}, State),
    {reply, {ok, make_ref()}, State#state{procedures = Procs}};

handle_call({unadvertise, Procedure}, _From, State) ->
    Procs = maps:remove(Procedure, State#state.procedures),
    %% No protocol message for unadvertise yet — procedure removed on disconnect
    {reply, ok, State#state{procedures = Procs}};

%%====================================================================
%% RPC Call
%%====================================================================

handle_call({rpc_call, Procedure, Args, Timeout}, From, State) ->
    handle_call({rpc_call, Procedure, Args, Timeout, #{}}, From, State);
handle_call({rpc_call, Procedure, Args, Timeout, Opts}, From, State) ->
    CallId = base64:encode(crypto:strong_rand_bytes(12)),
    TimerRef = erlang:send_after(Timeout, self(), {call_timeout, CallId}),
    PendingCalls = maps:put(CallId, {From, TimerRef}, State#state.pending_calls),
    maybe_send(call, build_call_msg(CallId, Procedure, Args, Opts), State),
    {noreply, State#state{pending_calls = PendingCalls}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown}, State}.

%%====================================================================
%% Publish (async)
%%====================================================================

handle_cast({publish, Topic, Payload}, State) ->
    BinPayload = to_binary_payload(Payload),
    maybe_send(publish, #{
        <<"topic">> => Topic,
        <<"payload">> => BinPayload,
        <<"qos">> => 0,
        <<"retain">> => false,
        <<"message_id">> => crypto:strong_rand_bytes(16)
    }, State),
    {noreply, State};

%%====================================================================
%% Async RPC (non-blocking cross-relay forwarding)
%%====================================================================

handle_cast({async_rpc_call, _Procedure, _Args, _Timeout, _Opts, CallbackPid, CorrelationId},
            #state{stream = undefined} = State) ->
    ?LOG_WARNING("[relay_client] async_call dropped — no stream (corr=~s)",
                 [CorrelationId]),
    CallbackPid ! {relay_reply, CorrelationId,
                   #{<<"error">> => #{<<"code">> => <<"peer_disconnected">>,
                                      <<"message">> => <<"no stream to peer">>}},
                   undefined},
    {noreply, State};
handle_cast({async_rpc_call, Procedure, Args, Timeout, Opts, CallbackPid, CorrelationId}, State) ->
    CallId = base64:encode(crypto:strong_rand_bytes(12)),
    TimerRef = erlang:send_after(Timeout, self(), {call_timeout, CallId}),
    PendingCalls = maps:put(CallId, {async, CallbackPid, CorrelationId, TimerRef},
                            State#state.pending_calls),
    maybe_send(call, build_call_msg(CallId, Procedure, Args, Opts), State),
    {noreply, State#state{pending_calls = PendingCalls}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%====================================================================
%% Connection lifecycle
%%====================================================================

handle_info(connect, State) ->
    QuicOpts = [{alpn, ["macula"]} | build_tls_opts(State#state.tls_verify)],
    case macula_quic:connect(State#state.host, State#state.port, QuicOpts, 10000) of
        {ok, Conn} ->
            case macula_quic:open_stream(Conn) of
                {ok, Stream} ->
                    macula_quic:setopt(Stream, active, true),
                    ?LOG_INFO("[relay_client] Connected to ~s", [State#state.url]),
                    State2 = State#state{conn = Conn, stream = Stream, status = connected,
                                         recv_buffer = <<>>, reconnect_attempt = 0},
                    send_connect(State2),
                    replay_state(State2),
                    schedule_ping(),
                    {noreply, State2};
                {error, StreamErr} ->
                    ?LOG_WARNING("[relay_client] Stream open failed on ~s: ~p", [State#state.url, StreamErr]),
                    catch macula_quic:close(Conn),
                    {noreply, schedule_failover(State)}
            end;
        {error, Reason} ->
            ?LOG_WARNING("[relay_client] Connect to ~s failed: ~p", [State#state.url, Reason]),
            {noreply, schedule_failover(State)};
        {error, Type, Detail} ->
            ?LOG_WARNING("[relay_client] Connect to ~s failed: ~p ~p", [State#state.url, Type, Detail]),
            {noreply, schedule_failover(State)}
    end;

handle_info({call_timeout, CallId}, State) ->
    case maps:get(CallId, State#state.pending_calls, undefined) of
        undefined -> {noreply, State};
        {From, _TimerRef} ->
            ?LOG_WARNING("[mesh_client] RPC call ~s TIMED OUT (no reply from relay)", [CallId]),
            gen_server:reply(From, {error, timeout}),
            {noreply, State#state{pending_calls = maps:remove(CallId, State#state.pending_calls)}};
        {async, CallbackPid, CorrelationId, _TimerRef} ->
            ?LOG_WARNING("[mesh_client] Async RPC ~s TIMED OUT", [CallId]),
            CallbackPid ! {relay_reply, CorrelationId,
                          #{<<"error">> => #{<<"code">> => <<"timeout">>,
                                             <<"message">> => <<"Cross-relay RPC timed out">>}},
                          undefined},
            {noreply, State#state{pending_calls = maps:remove(CallId, State#state.pending_calls)}}
    end;

%%====================================================================
%% QUIC data received
%%====================================================================

handle_info({quic, Data, _Stream, _Flags}, State) when is_binary(Data) ->
    ?LOG_INFO("[relay_client] [trace] quic_data bytes=~p url=~s",
              [byte_size(Data), State#state.url]),
    Buffer = <<(State#state.recv_buffer)/binary, Data/binary>>,
    {NewBuffer, State2} = process_buffer(Buffer, State),
    {noreply, State2#state{recv_buffer = NewBuffer}};

%%====================================================================
%% QUIC lifecycle — reconnect on any closure
%%====================================================================

handle_info({quic, peer_send_shutdown, _Stream, _}, State) ->
    handle_disconnect(State);
handle_info({quic, peer_send_aborted, _Stream, _}, State) ->
    handle_disconnect(State);
handle_info({quic, send_shutdown_complete, _Stream, _}, State) ->
    handle_disconnect(State);
handle_info({quic, shutdown, _Ref, _Reason}, State) ->
    handle_disconnect(State);
handle_info({quic, closed, _Ref, _Reason}, State) ->
    handle_disconnect(State);
handle_info({quic, transport_shutdown, _Ref, _Reason}, State) ->
    handle_disconnect(State);
handle_info({error, transport_down, _Detail}, State) ->
    handle_disconnect(State);
handle_info({quic, streams_available, _Conn, Info}, State) ->
    ?LOG_INFO("[relay_client] [trace] streams_available info=~p url=~s",
              [Info, State#state.url]),
    {noreply, State};
handle_info({quic, peer_needs_streams, _Conn, Info}, State) ->
    ?LOG_INFO("[relay_client] [trace] peer_needs_streams info=~p url=~s",
              [Info, State#state.url]),
    {noreply, State};
handle_info({quic, new_stream, NewStream, _Flags}, State) ->
    ?LOG_WARNING("[relay_client] [trace] peer opened new stream ~p url=~s",
                 [NewStream, State#state.url]),
    {noreply, State};

%% No outstanding PING — record timestamp so dead-detection has a baseline.
handle_info(send_ping, #state{status = connected, ping_sent_at = undefined} = State) ->
    send_protocol_ping(State),
    schedule_ping(),
    {noreply, State#state{ping_sent_at = erlang:monotonic_time(millisecond)}};
%% A PING is outstanding — dispatch on how long it has been outstanding.
%% Preserve the ORIGINAL ping_sent_at so Elapsed grows each tick and the
%% dead-connection threshold (> 2× PING_INTERVAL_MS) actually fires.
handle_info(send_ping, #state{status = connected, ping_sent_at = SentAt} = State) ->
    Elapsed = erlang:monotonic_time(millisecond) - SentAt,
    handle_outstanding_ping(Elapsed > ?PING_INTERVAL_MS * 2, Elapsed, State);
handle_info(send_ping, State) ->
    %% Not connected — skip, next ping will fire after reconnect
    {noreply, State};
handle_info(quic_connection_dead, State) ->
    handle_disconnect(State);

handle_info(Info, State) ->
    ?LOG_DEBUG("[relay_client] Unhandled: ~p", [Info]),
    {noreply, State}.

%% Outstanding PING exceeded the dead-threshold — skip this tick's
%% PING send and declare the connection dead; reconnect will fire.
handle_outstanding_ping(true, Elapsed, State) ->
    ?LOG_WARNING("[relay_client] No PONG for ~pms — connection dead, reconnecting",
                 [Elapsed]),
    self() ! quic_connection_dead,
    schedule_ping(),
    {noreply, State};
%% Still within the window — re-send PING, but do NOT update
%% ping_sent_at (that would mask the growing gap).
handle_outstanding_ping(false, _Elapsed, State) ->
    send_protocol_ping(State),
    schedule_ping(),
    {noreply, State}.

terminate(_Reason, #state{conn = Conn, stream = Stream}) ->
    catch macula_quic:close(Stream),
    catch macula_quic:close(Conn),
    ok.

%%====================================================================
%% Mesh ping — RTT measurement + publish
%%====================================================================

schedule_ping() ->
    erlang:send_after(?PING_INTERVAL_MS, self(), send_ping).

send_protocol_ping(State) ->
    Ts = erlang:system_time(millisecond),
    ?LOG_INFO("[relay_client] [trace] PING send url=~s", [State#state.url]),
    maybe_send(ping, #{timestamp => Ts}, State).

publish_mesh_ping(RttMs, State) ->
    RelayHost = list_to_binary(State#state.host),
    NodeName = State#state.identity,
    Site = State#state.site,
    Lat = site_field(lat, Site),
    Lng = site_field(lng, Site),
    Payload = json:encode(#{
        relay => RelayHost,
        node => NodeName,
        rtt_ms => RttMs,
        lat => Lat,
        lng => Lng,
        ts => erlang:system_time(second)
    }),
    maybe_send(publish, #{
        <<"topic">> => ?PING_TOPIC,
        <<"payload">> => iolist_to_binary(Payload),
        <<"qos">> => 0,
        <<"retain">> => false,
        <<"message_id">> => base64:encode(crypto:strong_rand_bytes(8))
    }, State).

notify_discovery(RttMs, State) ->
    RelayHost = list_to_binary(State#state.host),
    try macula_relay_discovery ! {ping_rtt, RelayHost, RttMs}
    catch _:_ -> ok
    end.

site_field(Key, Site) when is_map(Site) -> maps:get(Key, Site, 0.0);
site_field(_, _) -> 0.0.

%%====================================================================
%% Internal: message processing
%%====================================================================

process_buffer(Buffer, State) when byte_size(Buffer) < 8 ->
    {Buffer, State};
process_buffer(<<_Version:8, TypeId:8, _Flags:8, _Reserved:8,
                 PayloadLen:32/big-unsigned, Rest/binary>> = Buffer, State)
  when byte_size(Rest) >= PayloadLen ->
    MsgBytes = binary:part(Buffer, 0, 8 + PayloadLen),
    Remaining = binary:part(Buffer, 8 + PayloadLen, byte_size(Buffer) - 8 - PayloadLen),
    %% Skip the 0x10 PUBLISH firehose — trace everything else.
    trace_frame_arrival(TypeId, PayloadLen, State#state.url),
    State2 = handle_message(macula_protocol_decoder:decode(MsgBytes), State),
    process_buffer(Remaining, State2);
process_buffer(Buffer, State) ->
    {Buffer, State}.

%% @private Frame arrival tracing. Filters out the publish firehose
%% so logs show handshake/ping/pong/rpc frames only while diagnosing
%% cross-relay PONG delivery.
trace_frame_arrival(16#10, _Size, _Url) -> ok;  %% PUBLISH — skipped
trace_frame_arrival(TypeId, Size, Url) ->
    ?LOG_INFO("[relay_client] [trace] frame type_id=~p len=~p url=~s",
              [TypeId, Size, Url]).

%% Incoming SWIM messages — sent directly by remote relay handler on the inbound
%% stream as a PUBLISH. Route to SWIM gen_server + extract piggybacked Bloom.
handle_message({ok, {publish, #{<<"topic">> := <<"_swim.", _/binary>> = Topic,
                                <<"payload">> := Payload}}}, State) ->
    Decoded = safe_decode_json(Payload),
    route_swim_message(Topic, Payload, Decoded),
    State;

%% Incoming PUBLISH from relay
handle_message({ok, {publish, Msg}}, State) ->
    Topic = maps:get(<<"topic">>, Msg, <<>>),
    Payload = maps:get(<<"payload">>, Msg, <<>>),
    Trace = maps:get(<<"_trace">>, Msg, undefined),
    invoke_matching_callbacks(Topic, Payload, Trace, State#state.subscriptions),
    State;

%% Incoming CALL from relay (we're the provider)
handle_message({ok, {call, Msg}}, State) ->
    Procedure = maps:get(<<"procedure">>, Msg, <<>>),
    CallId = maps:get(<<"call_id">>, Msg, <<>>),
    Args = maps:get(<<"args">>, Msg, #{}),
    Trace = maps:get(<<"_trace">>, Msg, undefined),
    dispatch_incoming_call(Procedure, CallId, Args, Trace, State),
    State;

%% Incoming REPLY from relay (we're the caller)
handle_message({ok, {reply, Msg}}, State) ->
    CallId = maps:get(<<"call_id">>, Msg, <<>>),
    case maps:get(CallId, State#state.pending_calls, undefined) of
        undefined ->
            ?LOG_DEBUG("[relay_client] Reply for unknown call_id ~s", [CallId]),
            State;
        {From, TimerRef} ->
            %% Synchronous call — unblock the caller
            erlang:cancel_timer(TimerRef),
            Trace = maps:get(<<"_trace">>, Msg, undefined),
            Reply = decode_reply(Msg, Trace),
            gen_server:reply(From, Reply),
            State#state{pending_calls = maps:remove(CallId, State#state.pending_calls)};
        {async, CallbackPid, CorrelationId, TimerRef} ->
            %% Async call — deliver reply to callback PID
            erlang:cancel_timer(TimerRef),
            Trace = maps:get(<<"_trace">>, Msg, undefined),
            Reply = decode_reply(Msg, Trace),
            CallbackPid ! {relay_reply, CorrelationId, unwrap_reply(Reply), Trace},
            State#state{pending_calls = maps:remove(CallId, State#state.pending_calls)}
    end;

%% PONG — measure RTT and publish mesh ping
handle_message({ok, {pong, _Msg}}, #state{ping_sent_at = undefined} = State) ->
    ?LOG_INFO("[relay_client] [trace] PONG recv (ping_sent_at=undef) url=~s",
              [State#state.url]),
    State;
handle_message({ok, {pong, _Msg}}, State) ->
    RttMs = erlang:monotonic_time(millisecond) - State#state.ping_sent_at,
    ?LOG_INFO("[relay_client] [trace] PONG recv rtt_ms=~p url=~s",
              [RttMs, State#state.url]),
    publish_mesh_ping(RttMs, State),
    notify_discovery(RttMs, State),
    State#state{ping_sent_at = undefined, last_rtt_ms = RttMs};

handle_message({ok, {Type, _Msg}}, State) ->
    ?LOG_INFO("[relay_client] [trace] ignoring frame type=~p url=~s",
              [Type, State#state.url]),
    State;

handle_message({error, Reason}, State) ->
    ?LOG_WARNING("[relay_client] Decode error: ~p", [Reason]),
    State.

%%====================================================================
%%====================================================================
%% RPC call/reply helpers — decomposed from handle_message
%%====================================================================

%% Dispatch an incoming CALL to the registered handler or reply not_found.
dispatch_incoming_call(Procedure, CallId, Args, Trace, State) ->
    case maps:get(Procedure, State#state.procedures, undefined) of
        undefined ->
            Reply = #{<<"call_id">> => CallId,
                      <<"error">> => #{<<"code">> => <<"not_found">>}},
            maybe_send(reply, maybe_add_trace(Reply, Trace), State);
        Handler ->
            Stream = State#state.stream,
            spawn(fun() ->
                execute_and_reply(Handler, Args, CallId, Trace, Stream)
            end)
    end.

execute_and_reply(Handler, Args, CallId, Trace, Stream) ->
    Result = invoke_handler(Handler, Args, CallId),
    ?LOG_DEBUG("[relay_client] handler result call_id=~s: ~p", [CallId, Result]),
    send_reply(Result, CallId, Trace, Stream).

%% @private Run the handler, capturing any crash for diagnostics.
invoke_handler(Handler, Args, CallId) ->
    try Handler(Args)
    catch C:E:S ->
        ?LOG_ERROR("[relay_client] handler crash call_id=~s ~p:~p ~p",
                   [CallId, C, E, S]),
        {error, E}
    end.

%% @private Encode and send the REPLY. Encoder/send failures are
%% logged at ERROR so missing replies don't vanish into spawned-process
%% exits. The enclosing try swallows crashes — let-it-crash is a false
%% economy here because the caller is a fire-and-forget spawn.
send_reply(Result, CallId, Trace, Stream) ->
    Reply = maybe_add_trace(format_call_result(Result, CallId), Trace),
    try
        Binary = macula_protocol_encoder:encode(reply, Reply),
        report_send(macula_quic:async_send(Stream, Binary), CallId)
    catch C:E:S ->
        ?LOG_ERROR("[relay_client] encode/send crash call_id=~s ~p:~p ~p",
                   [CallId, C, E, S])
    end.

report_send(ok, CallId) ->
    ?LOG_DEBUG("[relay_client] REPLY sent call_id=~s", [CallId]);
report_send({error, Reason}, CallId) ->
    ?LOG_ERROR("[relay_client] REPLY send failed call_id=~s: ~p",
               [CallId, Reason]).

format_call_result({ok, R}, CallId) ->
    #{<<"call_id">> => CallId, <<"result">> => R};
format_call_result({error, R}, CallId) ->
    #{<<"call_id">> => CallId,
      <<"error">> => #{<<"code">> => <<"handler_error">>,
                       <<"message">> => iolist_to_binary(io_lib:format("~p", [R]))}}.

%% Decode a REPLY message into the caller-facing result tuple.
decode_reply(Msg, Trace) ->
    Result = case maps:get(<<"result">>, Msg, undefined) of
        undefined -> {error, maps:get(<<"error">>, Msg, #{})};
        R -> {ok, R}
    end,
    maybe_attach_trace(Result, Trace).

maybe_attach_trace(Result, undefined) -> Result;
maybe_attach_trace({ok, R}, Trace) -> {ok, R, #{trace => Trace}};
maybe_attach_trace(Other, _Trace) -> Other.

%% @private Build a CALL message map with optional target and trace fields.
build_call_msg(CallId, Procedure, Args, Opts) ->
    Msg0 = #{<<"call_id">> => CallId, <<"procedure">> => Procedure, <<"args">> => Args},
    Msg1 = case maps:get(target, Opts, undefined) of
        undefined -> Msg0;
        Target -> Msg0#{<<"target">> => Target}
    end,
    case maps:get(<<"_trace">>, Opts, undefined) of
        undefined -> Msg1;
        Trace -> Msg1#{<<"_trace">> => Trace}
    end.

%% @private Unwrap decoded reply to raw result for async callbacks.
%% relay_reply expects the raw result map, not {ok, Result} tuples.
unwrap_reply({ok, Result}) -> Result;
unwrap_reply({ok, Result, _TraceInfo}) -> Result;
unwrap_reply({error, ErrorMap}) -> #{<<"error">> => ErrorMap};
unwrap_reply(Other) -> Other.

%% Internal helpers
%%====================================================================

send_connect(State) ->
    NodeId = crypto:strong_rand_bytes(32),
    %% Use identity as node_name when set (multi-tenant stubs).
    %% Falls back to Erlang node name, then random hex.
    NodeName = case State#state.identity of
        Id when is_binary(Id), byte_size(Id) > 0 -> Id;
        _ ->
            case net_kernel:nodename() of
                nonode@nohost -> binary:encode_hex(crypto:strong_rand_bytes(8));
                Name -> atom_to_binary(Name)
            end
    end,
    %% target_relay = the PREFERRED relay (first in list), not the actual connected host.
    %% When failover connects to a box URL, target_relay still identifies the intended
    %% virtual relay identity so the node registers under the correct city.
    TargetRelay = list_to_binary(State#state.preferred_host),
    Base = #{
        version => <<"1.0">>,
        node_id => NodeId,
        node_name => NodeName,
        realm_id => State#state.realm,
        capabilities => [<<"pubsub">>, <<"rpc">>],
        endpoint => State#state.url,
        type => State#state.client_type,
        target_relay => TargetRelay
    },
    Msg0 = case State#state.geo_identity of
        GeoId when is_map(GeoId), map_size(GeoId) > 0 -> Base#{identity => GeoId};
        _ ->
            %% macula_connection is in macula-relay; build identity from env
            case build_env_identity() of
                EnvId when map_size(EnvId) > 0 -> Base#{identity => EnvId};
                _ -> Base
            end
    end,
    %% Include site metadata if configured
    Msg1 = case State#state.site of
        SiteMap when is_map(SiteMap), map_size(SiteMap) > 0 -> Msg0#{site => SiteMap};
        _ -> Msg0
    end,
    %% Include previous_relay if this is a failover reconnect (different host)
    Msg = case State#state.previous_host of
        PrevHost when is_list(PrevHost), PrevHost =/= State#state.host ->
            Msg1#{previous_relay => list_to_binary(PrevHost)};
        _ -> Msg1
    end,
    maybe_send(connect, Msg, State).

%% Replay all subscriptions and procedure registrations after reconnect
replay_state(#state{subscriptions = Subs, procedures = Procs} = State) ->
    %% Batch all topics in one SUBSCRIBE
    Topics = lists:usort([T || {_Ref, {T, _Cb}} <- maps:to_list(Subs)]),
    case Topics of
        [] -> ok;
        _ ->
            ?LOG_INFO("[relay_client] Replaying ~p subscription(s)", [length(Topics)]),
            maybe_send(subscribe, #{<<"topics">> => Topics, <<"qos">> => 0}, State)
    end,
    %% Register all procedures
    maps:foreach(fun(Proc, _Handler) ->
        ?LOG_INFO("[relay_client] Replaying procedure: ~s", [Proc]),
        maybe_send(register_procedure, #{<<"procedure">> => Proc}, State)
    end, Procs).

maybe_send(Type, _Msg, #state{stream = undefined}) ->
    ?LOG_WARNING("[relay_client] Dropped ~p frame — no stream", [Type]);
maybe_send(Type, Msg, #state{stream = Stream}) ->
    Binary = macula_protocol_encoder:encode(Type, Msg),
    case macula_quic:async_send(Stream, Binary) of
        ok -> ok;
        {error, Reason} ->
            ?LOG_WARNING("[relay_client] Send ~p failed: ~p", [Type, Reason])
    end.

%% Ensure payload is a flat binary before passing to msgpack.
%% Callers may pass: map, iolist (from json:encode), or binary.
to_binary_payload(Payload) when is_binary(Payload) -> Payload;
to_binary_payload(Payload) when is_map(Payload)    -> iolist_to_binary(json:encode(Payload));
to_binary_payload(Payload) when is_list(Payload)   -> iolist_to_binary(Payload);
to_binary_payload(Payload)                         -> iolist_to_binary(json:encode(Payload)).

handle_disconnect(#state{status = disconnected} = State) ->
    {noreply, State};
handle_disconnect(State) ->
    ?LOG_WARNING("[relay_client] Disconnected from ~s", [State#state.url]),
    catch macula_quic:close(State#state.stream),
    catch macula_quic:close(State#state.conn),
    %% ping_sent_at MUST be cleared here — otherwise the next connection's
    %% dead-detection tick compares Now against the previous connection's
    %% unanswered PING, firing an immediate false-positive reconnect.
    State2 = State#state{conn = undefined, stream = undefined, status = disconnected,
                          recv_buffer = <<>>, ping_sent_at = undefined},
    {noreply, schedule_failover(State2)}.

%% Pick next relay and schedule reconnect with exponential backoff + jitter.
%% Uses geographic discovery if available, falls back to round-robin.
schedule_failover(#state{reconnect_attempt = Attempt} = State) ->
    {NextUrl, NextState} = select_failover_relay(State),
    {NextHost, NextPort} = parse_url(NextUrl),
    BackoffMs = backoff_ms(Attempt),
    erlang:send_after(BackoffMs, self(), connect),
    ?LOG_INFO("[relay_client] Failover to ~s in ~bms (attempt ~b)",
              [NextUrl, BackoffMs, Attempt + 1]),
    NextState#state{url = NextUrl, host = NextHost, port = NextPort,
                    status = disconnected, reconnect_attempt = Attempt + 1,
                    previous_host = State#state.host}.

%% Try geographic discovery first, fall back to round-robin.
select_failover_relay(#state{host = CurrentHost} = State) ->
    CurrentHostname = list_to_binary(CurrentHost),
    case try_discovery_failover(CurrentHostname) of
        {ok, Url} ->
            {Url, State};
        {error, _} ->
            round_robin_failover(State)
    end.

try_discovery_failover(CurrentHostname) ->
    try macula_relay_discovery:nearest_except(CurrentHostname)
    catch _:_ -> {error, discovery_unavailable}
    end.

round_robin_failover(#state{relays = Relays, relay_index = CurrentIdx} = State) ->
    NumRelays = length(Relays),
    NextIdx = case NumRelays > 1 of
        true -> (CurrentIdx + 1) rem NumRelays;
        false -> CurrentIdx
    end,
    NextUrl = lists:nth(NextIdx + 1, Relays),
    {NextUrl, State#state{relay_index = NextIdx}}.

backoff_ms(Attempt) ->
    Base = min(?RECONNECT_BASE_MS * (1 bsl min(Attempt, 10)), ?RECONNECT_MAX_MS),
    Jitter = round(Base * ?RECONNECT_JITTER),
    Base + rand:uniform(2 * Jitter + 1) - Jitter - 1.

parse_url(Url) when is_binary(Url) ->
    parse_url(binary_to_list(Url));
parse_url("https://" ++ Rest) ->
    parse_host_port(Rest);
parse_url(Rest) ->
    parse_host_port(Rest).

parse_host_port(HostPort) ->
    case string:split(HostPort, ":", trailing) of
        [Host, PortStr] -> {Host, list_to_integer(PortStr)};
        [Host] -> {Host, 4433}
    end.

invoke_matching_callbacks(Topic, Payload, Trace, Subscriptions) ->
    DecodedPayload = safe_decode_json(Payload),
    Base = #{topic => Topic, payload => DecodedPayload},
    Event = case Trace of
        undefined -> Base;
        _ -> Base#{'_trace' => Trace}
    end,
    maps:foreach(fun(_Ref, {SubTopic, Callback}) ->
        case topic_matches(SubTopic, Topic) of
            true ->
                spawn(fun() ->
                    try Callback(Event)
                    catch C:E -> ?LOG_WARNING("[relay_client] Callback error: ~p:~p", [C, E])
                    end
                end);
            false -> ok
        end
    end, Subscriptions).

topic_matches(Pattern, Topic) when Pattern =:= Topic -> true;
topic_matches(<<"**">>, _Topic) -> true;
topic_matches(Pattern, Topic) ->
    PatternParts = binary:split(Pattern, <<".">>, [global]),
    TopicParts = binary:split(Topic, <<".">>, [global]),
    parts_match(PatternParts, TopicParts).

parts_match([], []) -> true;
parts_match([<<"**">>], _) -> true;
parts_match([<<"*">> | PR], [_ | TR]) -> parts_match(PR, TR);
parts_match([P | PR], [T | TR]) when P =:= T -> parts_match(PR, TR);
parts_match(_, _) -> false.

safe_decode_json(Payload) when is_binary(Payload) ->
    try json:decode(Payload) catch _:_ -> Payload end;
safe_decode_json(Payload) -> Payload.

%% SWIM message routing — decomposed from handle_message to avoid nesting.
route_swim_message(Topic, RawPayload, Decoded) ->
    From = swim_from(Decoded),
    forward_to_swim(Topic, From, RawPayload),
    store_piggybacked_bloom(From, Decoded).

swim_from(#{<<"from">> := F}) when is_binary(F) -> F;
swim_from(_) -> <<>>.

forward_to_swim(<<"_swim.ack">>, From, Payload) ->
    swim_cast({ack_received, From, Payload});
forward_to_swim(<<"_swim.ping">>, From, Payload) ->
    swim_cast({ping_received, From, Payload});
forward_to_swim(_, _, _) -> ok.

swim_cast(Msg) ->
    case erlang:whereis(macula_relay_swim) of
        undefined -> ok;
        Pid -> gen_server:cast(Pid, Msg)
    end.

%% Mesh clients ignore bloom filters — those are relay-to-relay peering data.
store_piggybacked_bloom(_, _) -> ok.

maybe_add_trace(Msg, undefined) -> Msg;
maybe_add_trace(Msg, Trace) -> Msg#{<<"_trace">> => Trace}.

%% Build TLS options for QUIC client connection.
%% verify_peer: use global TLS mode (production or development).
%% none: force no-verify opts regardless of global mode.
build_tls_opts(none) ->
    [{verify, none}];
build_tls_opts(_) ->
    macula_tls:quic_client_opts().

%% @private Build node identity from environment variables.
%% Reads HECATE_GEO_* / MACULA_GEO_* env vars for geo metadata.
build_env_identity() ->
    Pairs = [
        {city,    "HECATE_GEO_CITY"},
        {country, "HECATE_GEO_COUNTRY"},
        {owner,   "HECATE_OWNER_NAME"},
        {site,    "HECATE_SITE_NAME"}
    ],
    Floats = [
        {lat, "HECATE_GEO_LAT"},
        {lng, "HECATE_GEO_LNG"}
    ],
    M1 = lists:foldl(fun({K, Env}, Acc) ->
        case os:getenv(Env) of
            false -> Acc;
            Val -> Acc#{K => list_to_binary(Val)}
        end
    end, #{}, Pairs),
    lists:foldl(fun({K, Env}, Acc) ->
        case os:getenv(Env) of
            false -> Acc;
            Val ->
                try Acc#{K => list_to_float(Val)}
                catch _:_ -> Acc end
        end
    end, M1, Floats).
