%%%-------------------------------------------------------------------
%%% @doc Macula Relay Client — connects to relay servers via QUIC.
%%%
%%% Single gen_server managing ONE persistent QUIC connection with
%%% automatic failover across multiple relays. On disconnect, cycles
%%% to the next relay in the list with exponential backoff + jitter.
%%% Replays all subscriptions and procedure registrations on reconnect.
%%%
%%% Usage:
%%%
%%% ```
%%% Opts = #{relays => [Relay0, Relay1, Relay2]},
%%% {ok, Client} = macula_relay_client:start_link(Opts).
%%% '''
%%%
%%% Backward compatible with single relay via url key.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_relay_client).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1, stop/1]).
-export([subscribe/3, unsubscribe/2, publish/3]).
-export([advertise/3, unadvertise/2, call/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Test exports
-ifdef(TEST).
-export([backoff_ms/1, parse_url/1]).
-endif.

-define(RECONNECT_BASE_MS, 1000).
-define(RECONNECT_MAX_MS, 30000).
-define(RECONNECT_JITTER, 0.3).
-define(CALL_TIMEOUT, 5000).

-record(state, {
    relays :: [binary()],                                    %% all configured relay URLs
    relay_index :: non_neg_integer(),                         %% current relay (0-based)
    url :: binary(),                                         %% current relay URL
    host :: string(),
    port :: integer(),
    realm :: binary(),
    identity :: binary(),
    geo_identity :: map(),                                    %% geo metadata for CONNECT (city, country, lat, lng)
    client_type :: binary(),                                  %% <<"node">> or <<"relay">> (for CONNECT handshake)
    conn :: reference() | undefined,
    stream :: reference() | undefined,
    status :: connecting | connected | disconnected,
    recv_buffer :: binary(),
    reconnect_attempt :: non_neg_integer(),                   %% for exponential backoff
    %% Application state (survives reconnects)
    subscriptions :: #{reference() => {binary(), fun()}},     %% ref => {topic, callback}
    procedures :: #{binary() => fun()},                       %% procedure => handler
    pending_calls :: #{binary() => {pid(), reference()}}      %% call_id => {from, timer_ref}
}).

%%====================================================================
%% API
%%====================================================================

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

stop(Pid) ->
    gen_server:stop(Pid).

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
    gen_server:call(Pid, {rpc_call, Procedure, Args, Timeout}, Timeout + 1000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    Relays = case maps:find(relays, Opts) of
        {ok, List} when is_list(List), length(List) > 0 -> List;
        _ -> [maps:get(url, Opts, <<"https://localhost:4433">>)]
    end,
    %% Randomize initial relay to distribute load across relays
    Index = rand:uniform(length(Relays)) - 1,
    Url = lists:nth(Index + 1, Relays),
    {Host, Port} = parse_url(Url),
    Realm = maps:get(realm, Opts, <<"io.macula">>),
    Identity = maps:get(identity, Opts, <<"anonymous">>),
    GeoIdentity = maps:get(geo_identity, Opts, #{}),
    ClientType = maps:get(type, Opts, <<"node">>),

    State = #state{
        relays = Relays,
        relay_index = Index,
        url = Url,
        host = Host,
        port = Port,
        realm = Realm,
        identity = Identity,
        geo_identity = GeoIdentity,
        client_type = ClientType,
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
    CallId = base64:encode(crypto:strong_rand_bytes(12)),
    TimerRef = erlang:send_after(Timeout, self(), {call_timeout, CallId}),
    PendingCalls = maps:put(CallId, {From, TimerRef}, State#state.pending_calls),
    maybe_send(call, #{
        <<"call_id">> => CallId,
        <<"procedure">> => Procedure,
        <<"args">> => Args
    }, State),
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

handle_cast(_Msg, State) ->
    {noreply, State}.

%%====================================================================
%% Connection lifecycle
%%====================================================================

handle_info(connect, State) ->
    QuicOpts = [{alpn, ["macula"]} | macula_tls:quic_client_opts()],
    case macula_quic:connect(State#state.host, State#state.port, QuicOpts, 10000) of
        {ok, Conn} ->
            case macula_quic:open_stream(Conn) of
                {ok, Stream} ->
                    quicer:setopt(Stream, active, true),
                    ?LOG_INFO("[relay_client] Connected to ~s", [State#state.url]),
                    State2 = State#state{conn = Conn, stream = Stream, status = connected,
                                         recv_buffer = <<>>, reconnect_attempt = 0},
                    send_connect(State2),
                    replay_state(State2),
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
            gen_server:reply(From, {error, timeout}),
            {noreply, State#state{pending_calls = maps:remove(CallId, State#state.pending_calls)}}
    end;

%%====================================================================
%% QUIC data received
%%====================================================================

handle_info({quic, Data, _Stream, _Flags}, State) when is_binary(Data) ->
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
handle_info({quic, streams_available, _Conn, _Info}, State) ->
    {noreply, State};
handle_info({quic, peer_needs_streams, _Conn, _Info}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    ?LOG_DEBUG("[relay_client] Unhandled: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{conn = Conn, stream = Stream}) ->
    catch macula_quic:close(Stream),
    catch macula_quic:close(Conn),
    ok.

%%====================================================================
%% Internal: message processing
%%====================================================================

process_buffer(Buffer, State) when byte_size(Buffer) < 8 ->
    {Buffer, State};
process_buffer(<<_Version:8, _TypeId:8, _Flags:8, _Reserved:8,
                 PayloadLen:32/big-unsigned, Rest/binary>> = Buffer, State) ->
    case byte_size(Rest) >= PayloadLen of
        true ->
            MsgBytes = binary:part(Buffer, 0, 8 + PayloadLen),
            Remaining = binary:part(Buffer, 8 + PayloadLen, byte_size(Buffer) - 8 - PayloadLen),
            State2 = handle_message(macula_protocol_decoder:decode(MsgBytes), State),
            process_buffer(Remaining, State2);
        false ->
            {Buffer, State}
    end.

%% Incoming PUBLISH from relay
handle_message({ok, {publish, Msg}}, State) ->
    Topic = maps:get(<<"topic">>, Msg, <<>>),
    Payload = maps:get(<<"payload">>, Msg, <<>>),
    invoke_matching_callbacks(Topic, Payload, State#state.subscriptions),
    State;

%% Incoming CALL from relay (we're the provider)
handle_message({ok, {call, Msg}}, State) ->
    Procedure = maps:get(<<"procedure">>, Msg, <<>>),
    CallId = maps:get(<<"call_id">>, Msg, <<>>),
    Args = maps:get(<<"args">>, Msg, #{}),
    case maps:get(Procedure, State#state.procedures, undefined) of
        undefined ->
            maybe_send(reply, #{<<"call_id">> => CallId,
                                <<"error">> => #{<<"code">> => <<"not_found">>}}, State);
        Handler ->
            %% Execute handler in separate process to avoid blocking
            Stream = State#state.stream,
            spawn(fun() ->
                Result = try Handler(Args)
                         catch _:Err -> {error, Err}
                         end,
                ReplyMsg = case Result of
                    {ok, R} -> #{<<"call_id">> => CallId, <<"result">> => R};
                    {error, R} -> #{<<"call_id">> => CallId,
                                    <<"error">> => #{<<"code">> => <<"handler_error">>,
                                                     <<"message">> => iolist_to_binary(
                                                         io_lib:format("~p", [R]))}}
                end,
                Binary = macula_protocol_encoder:encode(reply, ReplyMsg),
                macula_quic:async_send(Stream, Binary)
            end)
    end,
    State;

%% Incoming REPLY from relay (we're the caller)
handle_message({ok, {reply, Msg}}, State) ->
    CallId = maps:get(<<"call_id">>, Msg, <<>>),
    case maps:get(CallId, State#state.pending_calls, undefined) of
        undefined ->
            ?LOG_DEBUG("[relay_client] Reply for unknown call_id"),
            State;
        {From, TimerRef} ->
            erlang:cancel_timer(TimerRef),
            Result = case maps:get(<<"result">>, Msg, undefined) of
                undefined ->
                    Error = maps:get(<<"error">>, Msg, #{}),
                    {error, Error};
                R ->
                    {ok, R}
            end,
            gen_server:reply(From, Result),
            State#state{pending_calls = maps:remove(CallId, State#state.pending_calls)}
    end;

%% PONG — connection alive
handle_message({ok, {pong, _Msg}}, State) ->
    State;

handle_message({ok, {Type, _Msg}}, State) ->
    ?LOG_DEBUG("[relay_client] Ignoring message type: ~p", [Type]),
    State;

handle_message({error, Reason}, State) ->
    ?LOG_WARNING("[relay_client] Decode error: ~p", [Reason]),
    State.

%%====================================================================
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
    %% target_relay = the hostname the client connected to (for multi-tenant relay routing)
    TargetRelay = list_to_binary(State#state.host),
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
    Msg = case State#state.geo_identity of
        GeoId when is_map(GeoId), map_size(GeoId) > 0 -> Base#{identity => GeoId};
        _ ->
            case macula_connection:build_node_identity(#{}) of
                EnvId when map_size(EnvId) > 0 -> Base#{identity => EnvId};
                _ -> Base
            end
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

maybe_send(_Type, _Msg, #state{stream = undefined}) -> ok;
maybe_send(Type, Msg, #state{stream = Stream}) ->
    Binary = macula_protocol_encoder:encode(Type, Msg),
    macula_quic:async_send(Stream, Binary).

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
    State2 = State#state{conn = undefined, stream = undefined, status = disconnected,
                          recv_buffer = <<>>},
    {noreply, schedule_failover(State2)}.

%% Pick next relay (round-robin) and schedule reconnect with exponential backoff + jitter.
schedule_failover(#state{relays = Relays, relay_index = CurrentIdx,
                          reconnect_attempt = Attempt} = State) ->
    NumRelays = length(Relays),
    %% Try a different relay if available
    NextIdx = case NumRelays > 1 of
        true -> (CurrentIdx + 1) rem NumRelays;
        false -> CurrentIdx
    end,
    NextUrl = lists:nth(NextIdx + 1, Relays),
    {NextHost, NextPort} = parse_url(NextUrl),
    %% Exponential backoff with jitter (±30%)
    BackoffMs = backoff_ms(Attempt),
    erlang:send_after(BackoffMs, self(), connect),
    ?LOG_INFO("[relay_client] Failover to ~s in ~bms (attempt ~b)",
              [NextUrl, BackoffMs, Attempt + 1]),
    State#state{relay_index = NextIdx, url = NextUrl, host = NextHost,
                port = NextPort, status = disconnected,
                reconnect_attempt = Attempt + 1}.

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

invoke_matching_callbacks(Topic, Payload, Subscriptions) ->
    DecodedPayload = safe_decode_json(Payload),
    maps:foreach(fun(_Ref, {SubTopic, Callback}) ->
        case topic_matches(SubTopic, Topic) of
            true ->
                spawn(fun() ->
                    try Callback(#{topic => Topic, payload => DecodedPayload})
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
