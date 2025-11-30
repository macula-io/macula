%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Connection - QUIC Transport Layer (v0.7.0+).
%%%
%%% This module manages the low-level QUIC connection lifecycle and
%%% message transport for mesh participants.
%%%
%%% Responsibilities:
%%% - Establish and maintain QUIC connection
%%% - Send messages via QUIC stream
%%% - Receive and route incoming messages to handlers
%%% - Handle connection errors and reconnection
%%% - Message encoding/decoding and buffering
%%%
%%% Renamed from macula_connection in v0.7.0 for clarity:
%%% - macula_connection = QUIC transport (this module - low-level)
%%% - macula_peer = mesh participant (high-level API)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_connection).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").
-include("macula_config.hrl").

%% API
-export([start_link/2, send_message/3, send_message_async/3, get_status/1, default_config/0]).

%% API for testing
-export([decode_messages/2]).

%% Internal exports
-export([start_keepalive_timer/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(CONNECT_RETRY_DELAY, 5000).

-record(state, {
    url :: binary(),
    opts :: map(),
    node_id :: binary(),
    realm :: binary(),
    peer_id :: integer(),  %% Unique peer system identifier for gproc lookups
    connection :: pid() | undefined,
    stream :: pid() | undefined,
    status = connecting :: connecting | connected | disconnected | error,
    recv_buffer = <<>> :: binary(),
    keepalive_timer :: reference() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(Url, Opts) ->
    gen_server:start_link(?MODULE, {Url, Opts}, []).

-spec default_config() -> map().
default_config() ->
    #{
        keepalive_enabled => true,
        keepalive_interval => 30000  %% 30 seconds
    }.

-spec send_message(pid(), atom(), map()) -> ok | {error, term()}.
send_message(Pid, Type, Msg) ->
    gen_server:call(Pid, {send_message, Type, Msg}, 5000).

%% @doc Send message asynchronously (fire-and-forget).
%% Use for operations where blocking is unacceptable and failures can be tolerated.
%% The message will be sent if connected, silently dropped if not.
-spec send_message_async(pid(), atom(), map()) -> ok.
send_message_async(Pid, Type, Msg) ->
    gen_server:cast(Pid, {send_message_async, Type, Msg}).

-spec get_status(pid()) -> connecting | connected | disconnected | error.
get_status(Pid) ->
    gen_server:call(Pid, get_status, 5000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Url, Opts}) ->
    ?LOG_INFO("[Connection] Starting for ~s", [Url]),

    %% Parse URL to extract host and port
    {Host, Port} = parse_url(Url),

    %% Get realm (required)
    Realm = get_realm_from_opts(Opts),

    %% Generate or get node ID
    NodeId = maps:get(node_id, Opts, generate_node_id()),

    %% Get peer_id from opts (set by macula_peer_system)
    PeerId = maps:get(peer_id, Opts, erlang:unique_integer([monotonic, positive])),

    State = #state{
        url = Url,
        opts = Opts#{host => Host, port => Port},
        node_id = NodeId,
        realm = Realm,
        peer_id = PeerId,
        connection = undefined,
        stream = undefined,
        status = connecting,
        recv_buffer = <<>>,
        keepalive_timer = undefined
    },

    %% Initiate connection asynchronously
    self() ! connect,

    {ok, State}.

handle_call({send_message, Type, Msg}, _From, #state{status = connected, stream = Stream} = State) ->
    case send_message_raw(Type, Msg, Stream) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({send_message, _Type, _Msg}, _From, State) ->
    {reply, {error, not_connected}, State};

handle_call(get_status, _From, State) ->
    {reply, State#state.status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle async send message - fire and forget.
%% Sends if connected, logs warning and drops if not.
handle_cast({send_message_async, Type, Msg}, #state{status = connected, stream = Stream} = State) ->
    case send_message_raw(Type, Msg, Stream) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            ?LOG_WARNING("Async send failed for type ~p: ~p", [Type, Reason]),
            {noreply, State}
    end;

handle_cast({send_message_async, Type, _Msg}, State) ->
    ?LOG_DEBUG("Async send dropped (not connected): type=~p, status=~p", [Type, State#state.status]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(connect, State) ->
    ?LOG_DEBUG("[Connection] Attempting connect to ~s", [State#state.url]),
    case do_connect(State) of
        {ok, State2} ->
            ?LOG_INFO("[Connection] Successfully connected to ~s", [State#state.url]),
            {noreply, State2};
        {error, Reason} ->
            ?LOG_ERROR("[Connection] Failed to connect to ~s: ~p, retrying in ~p ms",
                      [State#state.url, Reason, ?CONNECT_RETRY_DELAY]),
            erlang:send_after(?CONNECT_RETRY_DELAY, self(), connect),
            {noreply, State#state{status = error}}
    end;

handle_info({quic, Data, Stream, _Props}, State) when is_binary(Data) ->
    %% Received data from QUIC stream
    MainStream = State#state.stream,
    ?LOG_WARNING("[Connection] QUIC data received: ~p bytes, from_stream=~p, main_stream=~p, match=~p",
                 [byte_size(Data), Stream, MainStream, Stream =:= MainStream]),
    handle_stream_data(Stream =:= MainStream, Data, Stream, State);

%% Handle QUIC control messages (non-binary Data)
handle_info({quic, ControlMsg, Stream, _Props}, State) when is_atom(ControlMsg) ->
    %% QUIC control message - check if it indicates closure
    handle_quic_control_message(ControlMsg, Stream, State);

%% Handle keep-alive tick - send PING message
handle_info(keepalive_tick, #state{status = connected, stream = Stream} = State) ->
    %% Send PING message
    PingMsg = #{timestamp => erlang:system_time(millisecond)},
    case send_message_raw(ping, PingMsg, Stream) of
        ok ->
            ?LOG_DEBUG("Keep-alive PING sent"),
            %% Restart timer for next keep-alive
            StateWithTimer = start_keepalive_timer(State),
            {noreply, StateWithTimer};
        {error, Reason} ->
            ?LOG_WARNING("Failed to send keep-alive PING: ~p", [Reason]),
            %% Connection might be dead - let it retry
            {noreply, State}
    end;

%% Ignore keep-alive tick if not connected
handle_info(keepalive_tick, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    %% Ignore unexpected messages
    ?LOG_DEBUG("Unhandled handle_info message: ~p", [_Info]),
    {noreply, State}.

terminate(_Reason, #state{stream = Stream, connection = Conn}) ->
    ?LOG_INFO("Connection manager terminating"),
    %% Clean up QUIC resources
    catch macula_quic:close(Stream),
    catch macula_quic:close(Conn),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Dispatch stream data based on validity (pattern matching on boolean).
handle_stream_data(true, Data, _Stream, State) ->
    ?LOG_WARNING("[Connection] handle_stream_data(true) ENTRY: ~p bytes", [byte_size(Data)]),
    handle_received_data(Data, State);
handle_stream_data(false, _Data, Stream, State) ->
    ?LOG_WARNING("Received data from unknown stream: ~p", [Stream]),
    {noreply, State}.

%% @doc Handle QUIC control messages - trigger reconnect on connection/stream closure.
-spec handle_quic_control_message(atom(), pid(), #state{}) -> {noreply, #state{}}.

%% Stream closed - trigger reconnection
handle_quic_control_message(peer_send_shutdown, _Stream, State) ->
    ?LOG_WARNING("QUIC stream closed by peer (peer_send_shutdown), reconnecting..."),
    trigger_reconnect(State);

handle_quic_control_message(peer_send_aborted, _Stream, State) ->
    ?LOG_WARNING("QUIC stream aborted by peer, reconnecting..."),
    trigger_reconnect(State);

handle_quic_control_message(send_shutdown_complete, _Stream, State) ->
    ?LOG_WARNING("QUIC send shutdown complete, reconnecting..."),
    trigger_reconnect(State);

handle_quic_control_message(shutdown, _Stream, State) ->
    ?LOG_WARNING("QUIC shutdown, reconnecting..."),
    trigger_reconnect(State);

handle_quic_control_message(closed, _Stream, State) ->
    ?LOG_WARNING("QUIC connection closed, reconnecting..."),
    trigger_reconnect(State);

%% Other control messages - log and ignore
handle_quic_control_message(ControlMsg, _Stream, State) ->
    ?LOG_DEBUG("Ignoring QUIC control message: ~p", [ControlMsg]),
    {noreply, State}.

%% @doc Trigger reconnection by cleaning up and scheduling reconnect.
-spec trigger_reconnect(#state{}) -> {noreply, #state{}}.
trigger_reconnect(#state{stream = Stream, connection = Conn, keepalive_timer = Timer} = State) ->
    %% Cancel keep-alive timer
    case Timer of
        undefined -> ok;
        _ -> erlang:cancel_timer(Timer)
    end,

    %% Clean up old connection resources
    catch macula_quic:close(Stream),
    catch macula_quic:close(Conn),

    %% Schedule reconnect after delay
    erlang:send_after(?CONNECT_RETRY_DELAY, self(), connect),

    %% Update state to disconnected
    NewState = State#state{
        connection = undefined,
        stream = undefined,
        status = disconnected,
        recv_buffer = <<>>,
        keepalive_timer = undefined
    },
    {noreply, NewState}.

%% @doc Establish QUIC connection and perform handshake.
-spec do_connect(#state{}) -> {ok, #state{}} | {error, term()}.
do_connect(State) ->
    #{host := Host, port := Port} = State#state.opts,
    QuicOpts = build_quic_opts(Host),

    case attempt_quic_connection(Host, Port, QuicOpts) of
        {ok, Conn, Stream} ->
            complete_connection_setup(Conn, Stream, Host, Port, State);
        {error, _Reason} = Error ->
            Error
    end.

%% @doc Build QUIC connection options with TLS configuration.
%% Uses macula_tls module for centralized TLS settings (v0.11.0+).
%% Hostname is used for TLS hostname verification in production mode.
-spec build_quic_opts(Host :: string() | binary()) -> list().
build_quic_opts(Host) ->
    %% Get TLS options with hostname verification from centralized module
    TlsOpts = macula_tls:quic_client_opts_with_hostname(Host),

    %% Merge with QUIC-specific options
    BaseOpts = [
        {alpn, ["macula"]},
        {idle_timeout_ms, 60000},
        {keep_alive_interval_ms, 20000},
        {handshake_idle_timeout_ms, 30000}
    ],
    merge_opts(BaseOpts, TlsOpts).

%% @doc Merge two option lists, with second list taking precedence.
-spec merge_opts(list(), list()) -> list().
merge_opts(BaseOpts, OverrideOpts) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            lists:keystore(Key, 1, Acc, {Key, Value})
        end,
        BaseOpts,
        OverrideOpts
    ).

%% @doc Attempt QUIC connection and stream setup
-spec attempt_quic_connection(string(), integer(), list()) ->
    {ok, pid(), pid()} | {error, term()}.
attempt_quic_connection(Host, Port, QuicOpts) ->
    case safe_quic_connect(Host, Port, QuicOpts) of
        {ok, Conn} ->
            setup_bidirectional_stream(Conn);
        {error, _Reason} = Error ->
            Error;
        {error, _Type, _Details} = Error ->
            {error, {connection_failed, Error}};
        Other ->
            {error, {connection_failed, Other}}
    end.

%% @doc Safe QUIC connect with error handling.
%% NIF boundary - quicer can throw exceptions, convert to tagged tuples.
-spec safe_quic_connect(string(), integer(), list()) ->
    {ok, pid()} | {error, term()}.
safe_quic_connect(Host, Port, QuicOpts) ->
    try
        macula_quic:connect(Host, Port, QuicOpts, ?DEFAULT_TIMEOUT)
    catch
        _:Error ->
            {error, {connection_failed, Error}}
    end.

%% @doc Open and configure bidirectional stream
-spec setup_bidirectional_stream(pid()) ->
    {ok, pid(), pid()} | {error, term()}.
setup_bidirectional_stream(Conn) ->
    case macula_quic:open_stream(Conn) of
        {ok, Stream} ->
            configure_stream_active_mode(Conn, Stream);
        {error, Reason} ->
            macula_quic:close(Conn),
            {error, {stream_open_failed, Reason}}
    end.

%% @doc Set stream to active mode for receiving messages
-spec configure_stream_active_mode(pid(), pid()) ->
    {ok, pid(), pid()} | {error, term()}.
configure_stream_active_mode(Conn, Stream) ->
    case quicer:setopt(Stream, active, true) of
        ok ->
            ?LOG_DEBUG("Stream set to active mode"),
            {ok, Conn, Stream};
        {error, SetOptErr} ->
            ?LOG_WARNING("Failed to set stream active: ~p", [SetOptErr]),
            macula_quic:close(Stream),
            macula_quic:close(Conn),
            {error, {stream_setopt_failed, SetOptErr}}
    end.

%% @doc Complete connection setup with handshake and DHT registration
-spec complete_connection_setup(pid(), pid(), string(), integer(), #state{}) ->
    {ok, #state{}} | {error, term()}.
complete_connection_setup(Conn, Stream, Host, Port, State) ->
    ConnectMsg = build_connect_message(State),

    case send_message_raw(connect, ConnectMsg, Stream) of
        ok ->
            ?LOG_INFO("Connected to Macula mesh: ~s:~p", [Host, Port]),
            register_server_in_dht(State),

            %% Start keep-alive timer if enabled
            ConnectedState = State#state{
                connection = Conn,
                stream = Stream,
                status = connected
            },
            StateWithKeepalive = start_keepalive_timer(ConnectedState),

            {ok, StateWithKeepalive};
        {error, Reason} ->
            macula_quic:close(Stream),
            macula_quic:close(Conn),
            {error, {handshake_failed, Reason}}
    end.

%% @doc Build CONNECT protocol message
-spec build_connect_message(#state{}) -> map().
build_connect_message(State) ->
    LocalEndpoint = get_advertise_endpoint(),
    #{
        version => <<"1.0">>,
        node_id => State#state.node_id,
        realm_id => State#state.realm,
        capabilities => [pubsub, rpc],
        endpoint => LocalEndpoint
    }.

%% @doc Get endpoint to advertise for peer-to-peer connections
-spec get_advertise_endpoint() -> binary().
get_advertise_endpoint() ->
    case application:get_env(macula, advertise_endpoint) of
        {ok, Endpoint} when is_binary(Endpoint) ->
            Endpoint;
        _ ->
            construct_default_endpoint()
    end.

%% @doc Construct default endpoint from MACULA_HOSTNAME environment variable.
%% Also uses MACULA_QUIC_PORT if set, otherwise defaults to 9443.
-spec construct_default_endpoint() -> binary().
construct_default_endpoint() ->
    NodeHost = get_hostname_from_env(),
    Port = list_to_binary(os:getenv("MACULA_QUIC_PORT", "9443")),
    <<"https://", NodeHost/binary, ":", Port/binary>>.

%% @doc Get hostname from environment variables.
%% Checks MACULA_HOSTNAME first (Docker), then NODE_HOST, then HOSTNAME, fallback to localhost.
-spec get_hostname_from_env() -> binary().
get_hostname_from_env() ->
    get_hostname_from_env([
        "MACULA_HOSTNAME",  %% Docker compose sets this
        "NODE_HOST",        %% Legacy/alternative
        "HOSTNAME"          %% Standard shell variable
    ]).

%% @doc Try environment variables in order, return first non-false value.
-spec get_hostname_from_env([string()]) -> binary().
get_hostname_from_env([]) ->
    <<"localhost">>;
get_hostname_from_env([EnvVar | Rest]) ->
    case os:getenv(EnvVar) of
        false -> get_hostname_from_env(Rest);
        Value -> list_to_binary(Value)
    end.

%% @doc Register connected server in DHT routing table (best effort).
%% If DHT is not running, logs warning but continues.
%% Unexpected errors (bad URL, crypto failure) will crash - this is intentional.
-spec register_server_in_dht(#state{}) -> ok.
register_server_in_dht(State) ->
    ServerAddress = parse_server_endpoint(State#state.url),
    ServerNodeInfo = #{
        node_id => crypto:strong_rand_bytes(32),
        address => ServerAddress
    },

    case macula_routing_server:add_node(macula_routing_server, ServerNodeInfo) of
        ok ->
            ?LOG_DEBUG("Added server node to DHT routing table with address: ~p", [ServerAddress]),
            ok;
        {error, Reason} ->
            ?LOG_WARNING("DHT registration failed (expected if DHT not running): ~p", [Reason]),
            ok
    end.

%% @doc Send a protocol message through a stream (raw).
%% Crashes if message is invalid - this indicates a bug in the caller.
-spec send_message_raw(atom(), map(), pid()) -> ok | {error, term()}.
send_message_raw(Type, Msg, Stream) ->
    ?LOG_DEBUG("Type=~p, Msg=~p", [Type, Msg]),
    Binary = macula_protocol_encoder:encode(Type, Msg),
    macula_quic:async_send(Stream, Binary).

%% @doc Handle received data from the stream.
-spec handle_received_data(binary(), #state{}) -> {noreply, #state{}}.
handle_received_data(Data, State) ->
    %% Append to receive buffer
    Buffer = <<(State#state.recv_buffer)/binary, Data/binary>>,
    ?LOG_WARNING("[Connection] handle_received_data ENTRY: data=~p bytes, buffer=~p bytes", [byte_size(Data), byte_size(Buffer)]),

    %% Try to decode messages
    {Messages, RemainingBuffer} = decode_messages(Buffer, []),
    ?LOG_WARNING("[Connection] decode_messages returned: ~p messages, remaining=~p bytes", [length(Messages), byte_size(RemainingBuffer)]),

    %% Process each message
    State2 = lists:foldl(fun process_message/2, State, Messages),

    {noreply, State2#state{recv_buffer = RemainingBuffer}}.

%% @doc Decode all complete messages from buffer.
-spec decode_messages(binary(), list()) -> {list(), binary()}.
decode_messages(Buffer, Acc) when byte_size(Buffer) < 8 ->
    %% Not enough for header
    {lists:reverse(Acc), Buffer};
decode_messages(<<_Version:8, _TypeId:8, _Flags:8, _Reserved:8,
                  PayloadLen:32/big-unsigned, Rest/binary>> = Buffer, Acc) ->
    case byte_size(Rest) of
        ActualLen when ActualLen >= PayloadLen ->
            %% We have a complete message
            ?LOG_WARNING("[Connection] DECODING message: buffer=~p bytes, payload_len=~p", [byte_size(Buffer), PayloadLen]),
            case macula_protocol_decoder:decode(Buffer) of
                {ok, {Type, Msg}} ->
                    %% Skip this message and continue
                    ?LOG_WARNING("[Connection] DECODED OK: type=~p", [Type]),
                    <<_:8/binary, _Payload:PayloadLen/binary, Remaining/binary>> = Buffer,
                    decode_messages(Remaining, [{Type, Msg} | Acc]);
                {error, Reason} ->
                    %% Decode error, skip this message - LOG THE ERROR
                    <<_:8, TypeIdByte:8, _:6/binary, _Payload:PayloadLen/binary, Remaining/binary>> = Buffer,
                    ?LOG_WARNING("[Connection] DECODE ERROR: type_id=~p (0x~.16B), reason=~p, payload_len=~p",
                                 [TypeIdByte, TypeIdByte, Reason, PayloadLen]),
                    decode_messages(Remaining, Acc)
            end;
        _ ->
            %% Incomplete message, wait for more data
            {lists:reverse(Acc), Buffer}
    end.

%% @doc Process a received message - route to appropriate handler.
-spec process_message({atom(), map()}, #state{}) -> #state{}.

%% Route PUBLISH messages to pub/sub handler
process_message({publish, Msg}, State) ->
    ?LOG_INFO("Connection manager routing message type: ~p", [publish]),
    %% Look up the pubsub handler PID via gproc (using peer_id for uniqueness)
    case gproc:lookup_local_name({pubsub_handler, State#state.realm, State#state.peer_id}) of
        undefined ->
            ?LOG_WARNING("PubSub handler not found for realm ~s, peer_id ~p", [State#state.realm, State#state.peer_id]),
            State;
        PubSubPid ->
            macula_pubsub_handler:handle_incoming_publish(PubSubPid, Msg),
            ?LOG_DEBUG("Routed PUBLISH to pubsub_handler"),
            State
    end;

%% Route REPLY messages to RPC handler
process_message({reply, Msg}, State) ->
    ?LOG_INFO("Connection manager routing message type: ~p", [reply]),
    %% Look up the RPC handler by node_id (not peer_id) so replies arriving on ANY
    %% connection belonging to this node route to the correct handler
    case gproc:lookup_local_name({rpc_handler, State#state.realm, State#state.node_id}) of
        undefined ->
            ?LOG_WARNING("RPC handler not found for realm ~s, node_id ~s when routing REPLY",
                        [State#state.realm, binary:encode_hex(State#state.node_id)]),
            State;
        RpcPid ->
            macula_rpc_handler:handle_incoming_reply(RpcPid, Msg),
            ?LOG_DEBUG("Routed REPLY to rpc_handler"),
            State
    end;

%% Handle CONNECTED acknowledgment
process_message({connected, Msg}, State) ->
    ?LOG_INFO("Received CONNECTED acknowledgment from server: ~p", [Msg]),
    State;

%% Handle PING message - respond with PONG
process_message({ping, Msg}, #state{stream = Stream} = State) ->
    ?LOG_DEBUG("Received PING, responding with PONG"),
    PongMsg = #{timestamp => maps:get(timestamp, Msg, 0)},
    _ = send_message_raw(pong, PongMsg, Stream),
    State;

%% Handle PONG message - keep-alive acknowledgment
process_message({pong, _Msg}, State) ->
    ?LOG_DEBUG("Received PONG - connection alive"),
    State;

%% Handle FIND_VALUE_REPLY message - route to RPC handler for DHT query results
process_message({find_value_reply, Msg}, State) ->
    ?LOG_INFO("Connection manager routing message type: ~p", [find_value_reply]),
    %% Look up the RPC handler by node_id (not peer_id) so replies arriving on ANY
    %% connection belonging to this node route to the correct handler
    case gproc:lookup_local_name({rpc_handler, State#state.realm, State#state.node_id}) of
        undefined ->
            ?LOG_WARNING("RPC handler not found for realm ~s, node_id ~s",
                        [State#state.realm, binary:encode_hex(State#state.node_id)]),
            State;
        RpcPid ->
            macula_rpc_handler:handle_find_value_reply(RpcPid, Msg),
            ?LOG_DEBUG("Routed FIND_VALUE_REPLY to rpc_handler"),
            State
    end;

%% Handle RPC_REQUEST message - execute local handler and send reply back via stream.
%% This is used for relay scenarios: bootstrap forwards RPC_REQUEST through client stream,
%% and we handle it locally and reply through the same stream.
%%
%% IMPORTANT: We search ALL RPC handlers in this realm for the procedure, not just
%% the one associated with this connection's peer_id. This is because:
%% - The procedure handler (e.g., ping.handler) is registered by macula_ping_pong
%% - macula_ping_pong uses the RPC handler from macula_peers_sup (outbound connection)
%% - But the RPC_REQUEST arrives via the gateway's inbound client stream (different peer_id)
%% - So we need to search all RPC handlers to find the procedure
process_message({rpc_request, Msg}, #state{stream = Stream} = State) ->
    ?LOG_WARNING("[Connection] PROCESSING RPC_REQUEST locally (relay scenario)"),
    RequestId = maps:get(<<"request_id">>, Msg, undefined),
    Procedure = maps:get(<<"procedure">>, Msg, undefined),
    Args = maps:get(<<"args">>, Msg, <<>>),
    FromNode = maps:get(<<"from_node">>, Msg, undefined),

    ?LOG_WARNING("[Connection] RPC_REQUEST details: procedure=~s, request_id=~p, from_node=~p",
                [Procedure, RequestId, FromNode]),

    %% Search ALL RPC handlers in this realm for the procedure
    %% This handles relay scenarios where handler is in a different peer system
    Result = find_and_execute_handler(State#state.realm, Procedure, Args),

    ?LOG_WARNING("[Connection] RPC_REQUEST processing result: ~p", [Result]),

    %% Build and send reply back through the same stream
    %% Include to_node (original requester) so bootstrap can forward the reply
    ReplyMsg = case Result of
        {ok, ResultValue} ->
            EncodedResult = try macula_utils:encode_json(ResultValue) catch _:_ -> ResultValue end,
            #{
                request_id => RequestId,
                result => EncodedResult,
                from_node => State#state.node_id,
                to_node => FromNode,  %% Original requester - bootstrap uses this to route reply
                timestamp => erlang:system_time(millisecond)
            };
        {error, ErrorReason} ->
            #{
                request_id => RequestId,
                error => ErrorReason,
                from_node => State#state.node_id,
                to_node => FromNode,  %% Original requester - bootstrap uses this to route reply
                timestamp => erlang:system_time(millisecond)
            }
    end,

    ?LOG_WARNING("[Connection] Sending RPC_REPLY back through stream: ~p", [ReplyMsg]),
    EncodedReply = macula_protocol_encoder:encode(rpc_reply, ReplyMsg),
    ?LOG_WARNING("[Connection] EncodedReply size: ~p bytes", [byte_size(EncodedReply)]),
    case macula_quic:send(Stream, EncodedReply) of
        ok ->
            ?LOG_WARNING("[Connection] Successfully sent RPC_REPLY");
        {error, SendError} ->
            ?LOG_WARNING("[Connection] Failed to send RPC_REPLY: ~p", [SendError])
    end,
    State;

%% Handle RPC_REPLY message - route to RPC handler for async callback invocation.
%% This handles replies that come back through the client connection stream.
%% IMPORTANT: Use handle_async_reply (not handle_incoming_reply) because
%% async RPC uses request_id field, while sync RPC uses call_id field.
%% CRITICAL: Look up by node_id (not peer_id) so replies arriving on ANY connection
%% belonging to this node route to the correct handler. This fixes the relay scenario
%% where request goes out via outbound connection but reply arrives via inbound stream.
process_message({rpc_reply, Msg}, State) ->
    RequestId = maps:get(<<"request_id">>, Msg, maps:get(request_id, Msg, undefined)),
    ?LOG_WARNING("[Connection] RECEIVED RPC_REPLY: request_id=~p, node_id=~s",
                [RequestId, binary:encode_hex(State#state.node_id)]),
    case gproc:lookup_local_name({rpc_handler, State#state.realm, State#state.node_id}) of
        undefined ->
            ?LOG_WARNING("[Connection] RPC_REPLY: RPC handler not found, realm=~p, node_id=~s",
                        [State#state.realm, binary:encode_hex(State#state.node_id)]),
            State;
        RpcPid ->
            ?LOG_WARNING("[Connection] RPC_REPLY: routing to RPC handler pid=~p", [RpcPid]),
            macula_rpc_handler:handle_async_reply(RpcPid, Msg),
            ?LOG_WARNING("[Connection] RPC_REPLY: delivered to RPC handler"),
            State
    end;

%% Handle unknown message types
process_message({Type, _Msg}, State) ->
    ?LOG_INFO("Connection manager routing message type: ~p", [Type]),
    ?LOG_WARNING("Unknown message type: ~p", [Type]),
    State.

%% @doc Parse URL to extract host and port.
-spec parse_url(binary()) -> {string(), integer()}.
parse_url(Url) when is_binary(Url) ->
    parse_url(binary_to_list(Url));
parse_url("https://" ++ Rest) ->
    parse_host_port(Rest, 443);
parse_url("http://" ++ Rest) ->
    parse_host_port(Rest, 80);
parse_url(Url) ->
    error({invalid_url, Url}).

%% @doc Parse host and port from URL remainder, using default port if not specified.
-spec parse_host_port(string(), integer()) -> {string(), integer()}.
parse_host_port(HostPort, DefaultPort) ->
    case string:split(HostPort, ":") of
        [Host, PortStr] -> {Host, list_to_integer(PortStr)};
        [Host] -> {Host, DefaultPort}
    end.

%% @doc Parse server endpoint to extract address for DHT.
-spec parse_server_endpoint(binary()) -> binary().
parse_server_endpoint(Url) ->
    %% Extract host from URL for DHT address
    case parse_url(Url) of
        {Host, Port} ->
            list_to_binary(Host ++ ":" ++ integer_to_list(Port));
        _ ->
            Url
    end.

%% @doc Generate a random node ID.
-spec generate_node_id() -> binary().
generate_node_id() ->
    macula_utils:generate_node_id().

%% @doc Get realm from options and normalize.
-spec get_realm_from_opts(map()) -> binary().
get_realm_from_opts(Opts) ->
    normalize_realm(maps:get(realm, Opts, undefined)).

%% @doc Normalize realm to binary.
-spec normalize_realm(undefined | binary() | list() | atom()) -> binary().
normalize_realm(undefined) ->
    error({missing_required_option, realm});
normalize_realm(Realm) when is_binary(Realm) ->
    Realm;
normalize_realm(Realm) when is_list(Realm) ->
    list_to_binary(Realm);
normalize_realm(Realm) when is_atom(Realm) ->
    atom_to_binary(Realm).

%% @doc Start keep-alive timer if enabled in options.
-spec start_keepalive_timer(#state{}) -> #state{}.
start_keepalive_timer(#state{opts = Opts, keepalive_timer = OldTimer} = State) ->
    %% Cancel existing timer if present
    case OldTimer of
        undefined -> ok;
        _ -> erlang:cancel_timer(OldTimer)
    end,

    %% Check if keep-alive is enabled
    Enabled = maps:get(keepalive_enabled, Opts, true),
    start_keepalive_timer(Enabled, State).

%% Keep-alive disabled
start_keepalive_timer(false, State) ->
    State#state{keepalive_timer = undefined};

%% Keep-alive enabled - start timer
start_keepalive_timer(true, #state{opts = Opts} = State) ->
    Interval = maps:get(keepalive_interval, Opts, 30000),
    TimerRef = erlang:send_after(Interval, self(), keepalive_tick),
    State#state{keepalive_timer = TimerRef}.

%% @private Find and execute a procedure handler by searching all RPC handlers in the realm.
%% This is used for relay scenarios where the handler may be registered with a different
%% peer system than the one associated with the incoming connection.
-spec find_and_execute_handler(binary(), binary(), binary() | map()) ->
    {ok, term()} | {error, binary()}.
find_and_execute_handler(Realm, Procedure, Args) ->
    ?LOG_WARNING("[Connection] Searching ALL RPC handlers in realm ~s for procedure ~s",
                [Realm, Procedure]),

    %% Get all RPC handlers registered in gproc for this realm
    %% Pattern: {rpc_handler, Realm, _} matches any peer_id
    Pattern = {n, l, {rpc_handler, Realm, '_'}},
    RpcHandlers = gproc:lookup_pids(Pattern),

    ?LOG_WARNING("[Connection] Found ~p RPC handlers in realm", [length(RpcHandlers)]),

    %% Search each RPC handler's service registry for the procedure
    find_handler_in_registries(RpcHandlers, Procedure, Args).

%% @private Search through RPC handlers to find one that has the procedure registered.
-spec find_handler_in_registries([pid()], binary(), binary() | map()) ->
    {ok, term()} | {error, binary()}.
find_handler_in_registries([], Procedure, _Args) ->
    ?LOG_WARNING("[Connection] Procedure ~s not found in any RPC handler", [Procedure]),
    {error, <<"procedure_not_found">>};
find_handler_in_registries([RpcPid | Rest], Procedure, Args) ->
    ?LOG_WARNING("[Connection] Checking RPC handler ~p for procedure ~s", [RpcPid, Procedure]),
    case macula_rpc_handler:get_service_registry(RpcPid) of
        {error, _} ->
            %% Try next handler
            find_handler_in_registries(Rest, Procedure, Args);
        Registry ->
            case macula_service_registry:get_local_handler(Registry, Procedure) of
                {ok, Handler} ->
                    ?LOG_WARNING("[Connection] FOUND handler for ~s in RPC handler ~p",
                                [Procedure, RpcPid]),
                    execute_handler(Handler, Args);
                not_found ->
                    %% Try next handler
                    find_handler_in_registries(Rest, Procedure, Args)
            end
    end.

%% @private Execute a handler function with the provided arguments.
-spec execute_handler(fun((map()) -> {ok, term()} | {error, term()}), binary() | map()) ->
    {ok, term()} | {error, binary()}.
execute_handler(Handler, Args) ->
    try
        DecodedArgs = try macula_utils:decode_json(Args) catch _:_ -> Args end,
        HandlerResult = Handler(DecodedArgs),
        ?LOG_WARNING("[Connection] Handler executed, result=~p", [HandlerResult]),
        HandlerResult
    catch
        _:Error ->
            ?LOG_WARNING("[Connection] Handler THREW error: ~p", [Error]),
            {error, iolist_to_binary(io_lib:format("~p", [Error]))}
    end.
