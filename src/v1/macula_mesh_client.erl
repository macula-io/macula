%%%-------------------------------------------------------------------
%%% @doc Macula Mesh Client — connects a node to the relay mesh via QUIC.
%%%
%%% Single gen_server managing ONE persistent QUIC connection to a relay
%%% with automatic failover across multiple relays. On disconnect, cycles
%%% to the next relay in the list with exponential backoff + jitter.
%%% Replays all subscriptions and procedure registrations on reconnect.
%%%
%%% This is the SDK client module. It speaks the node role only — it
%%% cannot identify as a relay, cannot subscribe to _swim/_dht/_relay.*,
%%% and has no loop-prevention concerns. For relay-to-relay peering
%%% connections, see macula_peer_client in the macula-relay repository.
%%%
%%% Usage:
%%%
%%% ```
%%% {ok, Client} = macula_mesh_client:start_link(#{
%%%     relays => [&lt;&lt;"quic://boot.macula.io:443"&gt;&gt;],
%%%     realm => &lt;&lt;"io.macula"&gt;&gt;
%%% }).
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(macula_mesh_client).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1, stop/1]).
-export([subscribe/3, unsubscribe/2, publish/3]).
-export([advertise/3, unadvertise/2, call/4, call/5]).
%% Streaming RPC (v1.5.1+ Phase 2)
-export([advertise_stream/3, advertise_stream/4,
         call_stream/4, call_stream/5,
         open_stream/4, open_stream/5,
         send_stream_frame/3]).
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
    tls_verify :: verify_peer | none,                         %% TLS verification mode (none for dev/localhost)
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
    last_rtt_ms :: non_neg_integer() | undefined,             %% most recent RTT measurement
    %% Streaming RPC (v1.5.1+ Phase 2)
    streams = #{} :: #{binary() => pid()},                    %% stream_id => macula_stream_v1 pid
    stream_procedures = #{} :: #{binary() => {atom(), fun()}} %% procedure => {Mode, Handler/2}
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
    validate_topic_or_die(Topic),
    gen_server:call(Pid, {subscribe, Topic, Callback}).

-spec unsubscribe(pid(), reference()) -> ok.
unsubscribe(Pid, Ref) ->
    gen_server:call(Pid, {unsubscribe, Ref}).

-spec publish(pid(), binary(), binary() | map()) -> ok.
publish(Pid, Topic, Payload) ->
    validate_topic_or_die(Topic),
    gen_server:cast(Pid, {publish, Topic, Payload}).

-spec advertise(pid(), binary(), fun((map()) -> {ok, term()} | {error, term()})) -> {ok, reference()}.
advertise(Pid, Procedure, Handler) ->
    validate_topic_or_die(Procedure),
    gen_server:call(Pid, {advertise, Procedure, Handler}).

-spec unadvertise(pid(), binary()) -> ok.
unadvertise(Pid, Procedure) ->
    validate_topic_or_die(Procedure),
    gen_server:call(Pid, {unadvertise, Procedure}).

-spec call(pid(), binary(), map(), timeout()) -> {ok, term()} | {error, term()}.
call(Pid, Procedure, Args, Timeout) ->
    call(Pid, Procedure, Args, Timeout, #{}).

-spec call(pid(), binary(), map(), timeout(), map()) -> {ok, term()} | {error, term()}.
call(Pid, Procedure, Args, Timeout, Opts) ->
    validate_topic_or_die(Procedure),
    gen_server:call(Pid, {rpc_call, Procedure, Args, Timeout, Opts}, Timeout + 1000).


%% Validate a topic or RPC procedure name against the canonical
%% macula_topic structure. Accepts canonical 5-segment topics and
%% _mesh.* system topics. Throws on anything else — invalid topics
%% are programming errors that must fail loudly, not silently land
%% on a dead route.
validate_topic_or_die(Topic) when is_binary(Topic) ->
    validate_dispatch(macula_topic:validate(Topic), Topic);
validate_topic_or_die(Topic) ->
    error({invalid_topic, Topic, not_a_binary}).

validate_dispatch(ok, _Topic) -> ok;
validate_dispatch({error, Reason}, Topic) ->
    error({invalid_topic, Topic, Reason}).

%%====================================================================
%% Streaming RPC API (v1.5.1+ Phase 2)
%%====================================================================

-type stream_mode() :: server_stream | client_stream | bidi.
-type stream_handler() :: fun((pid(), term()) -> any()).

%% @doc Advertise a streaming procedure (default mode: server_stream).
%% Registers locally in this client's stream_procedures map AND sends
%% a register_procedure frame upstream so the relay routes incoming
%% STREAM_OPEN frames for `Procedure' back to this client.
-spec advertise_stream(pid(), binary(), stream_handler()) ->
        ok | {error, term()}.
advertise_stream(Pid, Procedure, Handler) ->
    advertise_stream(Pid, Procedure, server_stream, Handler).

-spec advertise_stream(pid(), binary(), stream_mode(), stream_handler()) ->
        ok | {error, term()}.
advertise_stream(Pid, Procedure, Mode, Handler)
  when is_binary(Procedure), is_atom(Mode), is_function(Handler, 2) ->
    validate_topic_or_die(Procedure),
    gen_server:call(Pid, {advertise_stream, Procedure, Mode, Handler}).

%% @doc Open a server-stream call against a connected mesh client.
-spec call_stream(pid(), binary(), term(), map()) ->
        {ok, pid()} | {error, term()}.
call_stream(Pid, Procedure, Args, Opts) ->
    call_stream(Pid, Procedure, Args, Opts, 30000).

-spec call_stream(pid(), binary(), term(), map(), timeout()) ->
        {ok, pid()} | {error, term()}.
call_stream(Pid, Procedure, Args, Opts, Timeout) ->
    validate_topic_or_die(Procedure),
    gen_server:call(Pid,
        {open_stream, Procedure, Args, Opts, server_stream}, Timeout).

%% @doc Open a client-stream or bidi call (mode from opts; default bidi).
-spec open_stream(pid(), binary(), term(), map()) ->
        {ok, pid()} | {error, term()}.
open_stream(Pid, Procedure, Args, Opts) ->
    open_stream(Pid, Procedure, Args, Opts, 30000).

-spec open_stream(pid(), binary(), term(), map(), timeout()) ->
        {ok, pid()} | {error, term()}.
open_stream(Pid, Procedure, Args, Opts, Timeout) ->
    validate_topic_or_die(Procedure),
    Mode = maps:get(mode, Opts, bidi),
    gen_server:call(Pid,
        {open_stream, Procedure, Args, Opts, Mode}, Timeout).

%% @doc Send a STREAM_* frame out the QUIC stream (called by
%% macula_stream_v1 processes when their peer shape is {remote, ...}).
-spec send_stream_frame(pid(), atom(), map()) -> ok.
send_stream_frame(Pid, Type, Msg) ->
    gen_server:cast(Pid, {send_stream_frame, Type, Msg}).

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
        tls_verify = TlsVerify,
        conn = undefined,
        stream = undefined,
        status = connecting,
        recv_buffer = <<>>,
        reconnect_attempt = 0,
        subscriptions = #{},
        procedures = #{},
        pending_calls = #{},
        streams = #{},
        stream_procedures = #{}
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

%%====================================================================
%% Streaming RPC — advertise / open_stream
%%====================================================================

handle_call({advertise_stream, Procedure, Mode, Handler}, _From, State) ->
    Procs = maps:put(Procedure, {Mode, Handler},
                     State#state.stream_procedures),
    %% Route incoming STREAM_OPENs here: same registration path as
    %% advertise/3 — the relay looks up by procedure name, blind to
    %% whether the target is unary or streaming.
    maybe_send(register_procedure,
               #{<<"procedure">> => Procedure}, State),
    {reply, ok, State#state{stream_procedures = Procs}};

handle_call({open_stream, _Proc, _Args, _Opts, _Mode}, _From,
            #state{status = Status} = State) when Status =/= connected ->
    {reply, {error, not_connected}, State};
handle_call({open_stream, Procedure, Args, Opts, Mode}, {CallerPid, _Tag},
            State) ->
    StreamId = crypto:strong_rand_bytes(16),
    Owner = maps:get(owner, Opts, CallerPid),
    {ok, StreamPid} = macula_stream_v1:start_link(#{
        id => StreamId,
        role => client,
        mode => Mode,
        owner => Owner
    }),
    ok = macula_stream_v1:attach_remote(StreamPid, self(), StreamId),
    _ = erlang:monitor(process, StreamPid),
    send_stream_open(StreamId, Procedure, Mode, Args, State),
    Streams = maps:put(StreamId, StreamPid, State#state.streams),
    {reply, {ok, StreamPid}, State#state{streams = Streams}};

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

%% Streaming RPC: a `macula_stream_v1' with `{remote, self(), Sid}' peer
%% asks us to encode and send a STREAM_* frame.
handle_cast({send_stream_frame, Type, Msg}, State) ->
    maybe_send(Type, Msg, State),
    maybe_expire_stream_on_send(Type, Msg, State);

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
            {noreply, State#state{pending_calls = maps:remove(CallId, State#state.pending_calls)}}
    end;

%%====================================================================
%% QUIC data received
%%====================================================================

handle_info({quic, Data, _Stream, _Flags}, State) when is_binary(Data) ->
    ?LOG_DEBUG("[relay_client] quic_data bytes=~p url=~s",
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
    ?LOG_DEBUG("[relay_client] streams_available info=~p url=~s",
               [Info, State#state.url]),
    {noreply, State};
handle_info({quic, peer_needs_streams, _Conn, Info}, State) ->
    ?LOG_DEBUG("[relay_client] peer_needs_streams info=~p url=~s",
               [Info, State#state.url]),
    {noreply, State};
handle_info({quic, new_stream, NewStream, _Flags}, State) ->
    ?LOG_WARNING("[relay_client] peer opened new stream ~p url=~s",
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

%% Handler worker exit. The worker is the process spawned by
%% dispatch_incoming_call/5 to run an advertised procedure handler.
%%   - normal exit: the handler ran and send_reply completed, clean up.
%%   - abnormal exit: the worker died before sending a reply; the caller
%%     will time out. Log with Procedure + CallId so the failure has a
%%     diagnostic trail instead of vanishing silently.
handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    case erase({handler_worker, Ref}) of
        undefined ->
            %% Might be a tracked macula_stream_v1 pid — drop its entry.
            Streams2 = maps:filter(fun(_Sid, P) -> P =/= Pid end,
                                   State#state.streams),
            case map_size(Streams2) =:= map_size(State#state.streams) of
                true ->
                    ?LOG_DEBUG("[relay_client] DOWN from unknown monitor ~p: ~p",
                               [Ref, Reason]);
                false ->
                    ?LOG_DEBUG("[relay_client] stream pid ~p exited: ~p",
                               [Pid, Reason])
            end,
            {noreply, State#state{streams = Streams2}};
        {_Procedure, _CallId} when Reason =:= normal ->
            {noreply, State};
        {Procedure, CallId} ->
            ?LOG_WARNING("[relay_client] Handler worker for ~s "
                         "(call_id=~s) died abnormally: ~p — "
                         "reply dropped, caller will time out",
                         [Procedure, CallId, Reason]),
            {noreply, State}
    end;

handle_info(Info, State) ->
    ?LOG_INFO("[relay_client] UNHANDLED ~p url=~s",
              [Info, State#state.url]),
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
    ?LOG_DEBUG("[relay_client] PING send url=~s", [State#state.url]),
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
%% so DEBUG logs show handshake/ping/pong/rpc frames only.
trace_frame_arrival(16#10, _Size, _Url) -> ok;  %% PUBLISH — skipped
trace_frame_arrival(TypeId, Size, Url) ->
    ?LOG_DEBUG("[relay_client] frame type_id=~p len=~p url=~s",
               [TypeId, Size, Url]).

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
            State#state{pending_calls = maps:remove(CallId, State#state.pending_calls)}
    end;

%% PONG — measure RTT and publish mesh ping
handle_message({ok, {pong, _Msg}}, #state{ping_sent_at = undefined} = State) ->
    ?LOG_DEBUG("[relay_client] PONG recv (ping_sent_at=undef) url=~s",
               [State#state.url]),
    State;
handle_message({ok, {pong, _Msg}}, State) ->
    RttMs = erlang:monotonic_time(millisecond) - State#state.ping_sent_at,
    ?LOG_DEBUG("[relay_client] PONG recv rtt_ms=~p url=~s",
               [RttMs, State#state.url]),
    publish_mesh_ping(RttMs, State),
    notify_discovery(RttMs, State),
    State#state{ping_sent_at = undefined, last_rtt_ms = RttMs};

%% Streaming RPC: relay forwarded a STREAM_OPEN from another node. We
%% are the serving side — look up the handler, spin up a server-side
%% stream, bind it to this QUIC client, and spawn the handler.
handle_message({ok, {stream_open, Msg}}, State) ->
    handle_incoming_stream_open(Msg, State);
handle_message({ok, {stream_data, Msg}}, State) ->
    handle_incoming_stream_data(Msg, State);
handle_message({ok, {stream_end, Msg}}, State) ->
    handle_incoming_stream_end(Msg, State);
handle_message({ok, {stream_error, Msg}}, State) ->
    handle_incoming_stream_error(Msg, State);
handle_message({ok, {stream_reply, Msg}}, State) ->
    handle_incoming_stream_reply(Msg, State);

handle_message({ok, {Type, _Msg}}, State) ->
    ?LOG_DEBUG("[relay_client] ignoring frame type=~p url=~s",
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
%%
%% The worker is spawned (not inlined) deliberately: a slow handler
%% must not block mesh_client's receive loop, which also has to
%% process incoming REPLYs to any outstanding calls this client made.
%% Inlining would deadlock on any client that both advertises AND
%% calls into the mesh.
%%
%% The worker is monitored so an out-of-band exit (external kill,
%% OOM, cascade) that terminates the worker WITHOUT having sent the
%% reply is logged instead of silently dropped. Without the monitor
%% the caller just times out with no trail. The in-band try/catch
%% inside invoke_handler/3 still catches handler exceptions and
%% turns them into an {error, _} reply — the monitor covers what
%% try/catch cannot: non-exception process terminations.
dispatch_incoming_call(Procedure, CallId, Args, Trace, State) ->
    case maps:get(Procedure, State#state.procedures, undefined) of
        undefined ->
            Reply = #{<<"call_id">> => CallId,
                      <<"error">> => #{<<"code">> => <<"not_found">>}},
            maybe_send(reply, maybe_add_trace(Reply, Trace), State);
        Handler ->
            Stream = State#state.stream,
            {_Pid, Ref} = erlang:spawn_monitor(fun() ->
                execute_and_reply(Handler, Args, CallId, Trace, Stream)
            end),
            put({handler_worker, Ref}, {Procedure, CallId})
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

%% Internal helpers
%%====================================================================

%%====================================================================
%% Streaming RPC — frame handlers (v1.5.1+ Phase 2)
%%====================================================================

send_stream_open(StreamId, Procedure, Mode, Args, State) ->
    maybe_send(stream_open, #{
        stream_id => StreamId,
        procedure => Procedure,
        mode => Mode,
        args => args_payload(Args)
    }, State).

%% STREAM_OPEN args: pass binaries + maps through unchanged so the
%% protocol encoder can encode them as a single CBOR payload. Other
%% term shapes get pre-packed to a CBOR binary blob — recipients
%% then decode that blob themselves if they understand it.
args_payload(B) when is_binary(B) -> B;
args_payload(M) when is_map(M)    -> M;
args_payload(Other)               -> macula_cbor_nif:pack(Other).

stream_id_of(Msg) ->
    case maps:find(stream_id, Msg) of
        {ok, Sid} -> Sid;
        error -> maps:get(<<"stream_id">>, Msg, <<>>)
    end.

msg_field(Msg, Atom, Binary, Default) ->
    case maps:find(Atom, Msg) of
        {ok, V} -> V;
        error -> maps:get(Binary, Msg, Default)
    end.

%% A STREAM_OPEN from the relay means we are being asked to serve a
%% streaming call. Dispatch against our local `stream_procedures'
%% map. If the procedure is unknown, answer with STREAM_ERROR so the
%% caller doesn't hang on its await_reply / recv.
handle_incoming_stream_open(Msg, State) ->
    StreamId = stream_id_of(Msg),
    Procedure = msg_field(Msg, procedure, <<"procedure">>, <<>>),
    DeclaredMode = decode_mode(
                     msg_field(Msg, mode, <<"mode">>, bidi)),
    Args = msg_field(Msg, args, <<"args">>, #{}),
    case maps:get(Procedure, State#state.stream_procedures, undefined) of
        undefined ->
            ?LOG_WARNING("[relay_client] STREAM_OPEN for unknown procedure ~s",
                         [Procedure]),
            maybe_send(stream_error, #{
                stream_id => StreamId,
                code => <<"not_found">>,
                message => <<"procedure not advertised">>
            }, State),
            State;
        {AdvMode, Handler} ->
            Mode = pick_mode(AdvMode, DeclaredMode),
            start_incoming_stream(StreamId, Procedure, Mode, Handler,
                                  Args, State)
    end.

decode_mode(<<"server_stream">>) -> server_stream;
decode_mode(<<"client_stream">>) -> client_stream;
decode_mode(<<"bidi">>) -> bidi;
decode_mode(M) when is_atom(M) -> M;
decode_mode(_) -> bidi.

%% Advertised mode wins — the server declared the shape of the RPC.
pick_mode(Adv, _Req) -> Adv.

start_incoming_stream(StreamId, Procedure, Mode, Handler, Args, State) ->
    %% The server-side stream needs an owner; use a minimal host
    %% process so a crashing handler doesn't take down the mesh_client.
    Host = spawn(fun() -> receive stop -> ok end end),
    {ok, StreamPid} = macula_stream_v1:start_link(#{
        id => StreamId,
        role => server,
        mode => Mode,
        owner => Host
    }),
    ok = macula_stream_v1:attach_remote(StreamPid, self(), StreamId),
    _ = erlang:monitor(process, StreamPid),
    Streams = maps:put(StreamId, StreamPid, State#state.streams),
    _HandlerPid = spawn_stream_handler(Handler, StreamPid, Args, Procedure),
    ?LOG_INFO("[relay_client] STREAM_OPEN served: ~s mode=~p id=~p",
              [Procedure, Mode, StreamId]),
    State#state{streams = Streams}.

spawn_stream_handler(Handler, Stream, Args, Procedure) ->
    %% Matches macula_stream_local's in-process handler semantics:
    %% a crash becomes a STREAM_ERROR abort rather than a silent
    %% client-side timeout. try/catch is load-bearing here — the
    %% only way to surface distinct crash reasons back to the caller.
    spawn(fun() ->
        try Handler(Stream, Args)
        catch
            Class:Reason:Stack ->
                Code = atom_to_binary(Class, utf8),
                Msg = iolist_to_binary(io_lib:format(
                    "handler ~s crashed: ~p:~p~n~p",
                    [Procedure, Class, Reason, Stack])),
                _ = macula_stream_v1:abort(Stream, Code, Msg)
        end
    end).

handle_incoming_stream_data(Msg, State) ->
    StreamId = stream_id_of(Msg),
    Body = msg_field(Msg, body, <<"body">>, <<>>),
    Encoding = decode_encoding(
                 msg_field(Msg, encoding, <<"encoding">>, raw)),
    case maps:get(StreamId, State#state.streams, undefined) of
        undefined ->
            ?LOG_DEBUG("[relay_client] STREAM_DATA for unknown id", []),
            State;
        Pid ->
            macula_stream_v1:deliver_chunk(Pid, Encoding, Body),
            State
    end.

decode_encoding(<<"raw">>) -> raw;
decode_encoding(<<"msgpack">>) -> msgpack;
decode_encoding(E) when is_atom(E) -> E;
decode_encoding(_) -> raw.

handle_incoming_stream_end(Msg, State) ->
    StreamId = stream_id_of(Msg),
    Role = decode_end_role(msg_field(Msg, role, <<"role">>, both)),
    case maps:get(StreamId, State#state.streams, undefined) of
        undefined -> State;
        Pid ->
            macula_stream_v1:deliver_end(Pid, Role),
            maybe_forget_stream(Role, StreamId, State)
    end.

decode_end_role(<<"send">>) -> send;
decode_end_role(<<"both">>) -> both;
decode_end_role(R) when is_atom(R) -> R;
decode_end_role(_) -> both.

maybe_forget_stream(both, StreamId, State) ->
    State#state{streams = maps:remove(StreamId, State#state.streams)};
maybe_forget_stream(_Role, _StreamId, State) ->
    State.

handle_incoming_stream_error(Msg, State) ->
    StreamId = stream_id_of(Msg),
    Code = msg_field(Msg, code, <<"code">>, <<"error">>),
    Message = msg_field(Msg, message, <<"message">>, <<>>),
    case maps:get(StreamId, State#state.streams, undefined) of
        undefined -> State;
        Pid ->
            macula_stream_v1:deliver_error(Pid, Code, Message),
            State#state{streams = maps:remove(StreamId, State#state.streams)}
    end.

handle_incoming_stream_reply(Msg, State) ->
    StreamId = stream_id_of(Msg),
    Result = case maps:find(result, Msg) of
        {ok, R} -> {ok, R};
        error ->
            case maps:find(<<"result">>, Msg) of
                {ok, R} -> {ok, R};
                error ->
                    Err = msg_field(Msg, error, <<"error">>, #{}),
                    {error, Err}
            end
    end,
    case maps:get(StreamId, State#state.streams, undefined) of
        undefined -> State;
        Pid ->
            macula_stream_v1:deliver_reply(Pid, Result),
            State
    end.

%% Outbound STREAM_END/ERROR/REPLY mean our side is done with this
%% stream — drop the routing entry. STREAM_DATA does not.
maybe_expire_stream_on_send(stream_end, Msg, State) ->
    Role = decode_end_role(msg_field(Msg, role, <<"role">>, both)),
    {noreply, maybe_forget_stream(Role, stream_id_of(Msg), State)};
maybe_expire_stream_on_send(stream_error, Msg, State) ->
    Sid = stream_id_of(Msg),
    {noreply, State#state{streams = maps:remove(Sid, State#state.streams)}};
maybe_expire_stream_on_send(stream_reply, _Msg, State) ->
    {noreply, State};
maybe_expire_stream_on_send(_Type, _Msg, State) ->
    {noreply, State}.

%% Tear down all open streams with a `disconnected' error code. Caller
%% await_reply / recv waiters unblock immediately; handler processes
%% see their stream abort and exit cleanly.
teardown_streams(State) ->
    maps:foreach(fun(_Sid, Pid) ->
        catch macula_stream_v1:abort(Pid, <<"disconnected">>, <<"relay disconnect">>)
    end, State#state.streams),
    State#state{streams = #{}}.

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
        type => <<"node">>,
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
    %% Abort every open stream with a disconnected error. Without this
    %% every open recv/await_reply would hang until the next reconnect.
    State1 = teardown_streams(State),
    %% ping_sent_at MUST be cleared here — otherwise the next connection's
    %% dead-detection tick compares Now against the previous connection's
    %% unanswered PING, firing an immediate false-positive reconnect.
    State2 = State1#state{conn = undefined, stream = undefined, status = disconnected,
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

%% Deliver a matching PUBLISH to every local subscription, inline.
%%
%% Inline (rather than spawn-per-callback) is load-bearing for
%% dist-over-mesh. Every tunnel byte-frame arrives as a PUBLISH and
%% the bridge callback does `Self ! {tunnel_in, Payload}'. With a
%% spawn-per-callback, consecutive PUBLISHes spawn concurrent workers
%% that race to send into the bridge mailbox — the bridge then
%% receives frames out of order, writes them to gen_tcp out of order,
%% and the dist controller sees a scrambled byte stream. Inline
%% preserves per-connection FIFO end-to-end.
%%
%% Other benefits (applicable to any subscription callback):
%%   - avoids process explosion under high-traffic topics
%%   - lets a slow callback backpressure the mesh_client receive loop,
%%     which backpressures the QUIC stream — the correct shape for a
%%     reliable push pub/sub
%%   - exceptions are caught and logged with relay_client context so
%%     a misbehaving user callback cannot take down the client
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
                try Callback(Event)
                catch C:E -> ?LOG_WARNING("[relay_client] Callback error: ~p:~p", [C, E])
                end;
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
