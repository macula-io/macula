%%%-------------------------------------------------------------------
%%% @doc
%%% Peer Connector - Establishes direct QUIC connections to remote peers (v0.8.0+).
%%%
%%% This module enables peer-to-peer communication by establishing outbound
%%% QUIC connections to arbitrary peers. Used by DHT for STORE/FIND_VALUE
%%% message propagation and by RPC/PubSub for direct delivery.
%%%
%%% == Overview ==
%%%
%%% Pattern: Connection-pooled utility module with NAT-aware fallback
%%%   - Uses `macula_peer_connection_pool' for connection reuse
%%%   - Falls back to NAT-aware routing (hole punch, relay) on failure
%%%   - Fire-and-forget message sending
%%%
%%% == Connection Strategy (v0.12.0+) ==
%%%
%%% 1. Try pooled connection (fastest, cached)
%%% 2. Try direct QUIC connection (new connection)
%%% 3. Fall back to NAT-aware routing via `macula_nat_connector':
%%%    a. Direct connection (if NAT allows)
%%%    b. Hole punch (coordinated NAT traversal)
%%%    c. Relay via gateway (guaranteed fallback)
%%%
%%% == Usage ==
%%%
%%% Used internally by:
%%%   - `macula_pubsub_dht': Direct pub/sub delivery to discovered subscribers
%%%   - `macula_service_registry': DHT STORE propagation to k=20 nodes
%%%   - Future: Multi-hop RPC routing
%%%
%%% ```
%%% %% Send a DHT STORE message to a peer
%%% Endpoint = <<"192.168.1.100:9443">>,
%%% Message = #{
%%%     key => <<"service.calculator.add">>,
%%%     value => <<"192.168.1.50:9443">>,
%%%     ttl => 300
%%% },
%%% ok = macula_peer_connector:send_message(Endpoint, dht_store, Message).
%%% '''
%%%
%%% == Performance Characteristics ==
%%%
%%% v0.8.0: Fire-and-forget pattern (now legacy fallback)
%%%   - Creates new connection per message
%%%   - Simple but inefficient for high-frequency messaging
%%%
%%% v0.10.0: Connection pooling (current)
%%%   - Reuses existing connections via macula_peer_connection_pool
%%%   - 1.5-2x latency improvement for repeated messaging
%%%
%%% v0.12.0: NAT-aware routing (current)
%%%   - Automatic fallback to hole punch and relay
%%%   - Works across all NAT types
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peer_connector).

-include_lib("kernel/include/logger.hrl").
-include_lib("quicer/include/quicer.hrl").

%% API
-export([
    send_message/3,
    send_message/4,
    send_message_and_wait/4,  % For request-response patterns like NAT_PROBE
    send_message_nat_aware/4,
    send_message_nat_aware/5
]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Send a message to a remote peer (fire-and-forget).
%% Uses connection pool for efficiency, falls back to direct connection.
-spec send_message(binary(), atom(), map()) -> ok | {error, term()}.
send_message(Endpoint, MessageType, Message) ->
    send_message(Endpoint, MessageType, Message, 5000).

%% @doc Send a message to a remote peer with custom timeout.
-spec send_message(binary(), atom(), map(), timeout()) -> ok | {error, term()}.
send_message(Endpoint, MessageType, Message, _Timeout) ->
    %% Encode message
    MessageBinary = macula_protocol_encoder:encode(MessageType, Message),

    %% Try to use connection pool first
    PoolPid = whereis(macula_peer_connection_pool),
    send_via_connection(PoolPid, Endpoint, MessageBinary).

%% @doc Send a message and wait for a reply (request-response pattern).
%% This is used for messages like NAT_PROBE that expect a reply.
%% Returns {ok, ReplyMessage} on success, {error, timeout} if no reply received.
-spec send_message_and_wait(binary(), atom(), map(), timeout()) ->
    {ok, {atom(), map()}} | {error, term()}.
send_message_and_wait(Endpoint, MessageType, Message, Timeout) ->
    %% Encode message
    MessageBinary = macula_protocol_encoder:encode(MessageType, Message),

    %% Parse endpoint to get host and port
    case parse_endpoint(Endpoint) of
        {ok, Host, Port} ->
            send_and_wait_quic(Host, Port, MessageBinary, Timeout);
        {error, Reason} ->
            {error, {invalid_endpoint, Reason}}
    end.

%% @private Send message and wait for reply using direct QUIC connection.
send_and_wait_quic(Host, Port, MessageBinary, Timeout) ->
    ?LOG_INFO("[PeerConnector] send_and_wait to ~s:~p (~p bytes)", [Host, Port, byte_size(MessageBinary)]),
    ConnectOpts = [
        {alpn, ["macula"]},
        {verify, none},
        {idle_timeout_ms, 60000},
        {keep_alive_interval_ms, 20000},
        {handshake_idle_timeout_ms, 30000}
    ],
    case macula_quic:connect(Host, Port, ConnectOpts, Timeout) of
        {ok, Conn} ->
            send_and_wait_stream(Conn, MessageBinary, Timeout);
        {error, transport_down, _Details} ->
            {error, {connect_failed, transport_down}};
        {error, Reason} ->
            {error, {connect_failed, Reason}}
    end.

%% @private Open stream, send message, wait for reply.
send_and_wait_stream(Conn, MessageBinary, Timeout) ->
    case macula_quic:open_stream(Conn) of
        {ok, Stream} ->
            %% Set stream to active mode to receive reply
            case quicer:setopt(Stream, active, true) of
                ok ->
                    send_and_wait_reply(Conn, Stream, MessageBinary, Timeout);
                {error, Reason} ->
                    quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
                    {error, {setopt_failed, Reason}}
            end;
        {error, Reason} ->
            quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
            {error, {stream_failed, Reason}}
    end.

%% @private Send message and wait for reply data.
send_and_wait_reply(Conn, Stream, MessageBinary, Timeout) ->
    case macula_quic:send(Stream, MessageBinary) of
        ok ->
            %% Wait for reply with loop to ignore control messages
            wait_for_data_reply(Conn, Stream, Timeout, erlang:monotonic_time(millisecond));
        {error, Reason} ->
            quicer:async_shutdown_stream(Stream, ?QUIC_STREAM_SHUTDOWN_FLAG_ABORT, 0),
            quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
            {error, {send_failed, Reason}}
    end.

%% @private Wait for binary data reply, ignoring QUIC control messages and stale stream data.
%% NOTE: Data may arrive on OTHER streams (from previous/concurrent connections).
%% We ignore data on other streams and keep waiting for data on OUR stream.
wait_for_data_reply(Conn, Stream, Timeout, StartTime) ->
    RemainingTime = max(0, Timeout - (erlang:monotonic_time(millisecond) - StartTime)),
    receive
        {quic, Data, Stream, _Flags} when is_binary(Data) ->
            ?LOG_INFO("[PeerConnector] Received reply: ~p bytes", [byte_size(Data)]),
            ReplyResult = macula_protocol_decoder:decode(Data),
            ?LOG_INFO("[PeerConnector] Decoded reply: ~p", [ReplyResult]),
            quicer:async_shutdown_stream(Stream, ?QUIC_STREAM_SHUTDOWN_FLAG_GRACEFUL, 0),
            quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
            ReplyResult;
        {quic, Data, OtherStream, _Flags} when is_binary(Data) ->
            %% Data on different stream - ignore and keep waiting for OUR stream
            %% This happens with concurrent connections or stale streams
            ?LOG_DEBUG("[PeerConnector] Ignoring data on other stream ~p (waiting for ~p)",
                       [OtherStream, Stream]),
            wait_for_data_reply(Conn, Stream, Timeout, StartTime);
        %% Ignore QUIC control messages and continue waiting
        {quic, streams_available, _ConnRef, _Info} ->
            wait_for_data_reply(Conn, Stream, Timeout, StartTime);
        {quic, peer_needs_streams, _ConnRef, _Info} ->
            wait_for_data_reply(Conn, Stream, Timeout, StartTime);
        {quic, shutdown, Stream, _Reason} ->
            %% Only treat shutdown of OUR stream as error
            {error, connection_shutdown};
        {quic, shutdown, _OtherHandle, _Reason} ->
            %% Shutdown of other streams - ignore
            wait_for_data_reply(Conn, Stream, Timeout, StartTime);
        {quic, closed, Stream, _Reason} ->
            %% Only treat closure of OUR stream as error
            {error, connection_closed};
        {quic, closed, _OtherHandle, _Reason} ->
            %% Closure of other streams - ignore
            wait_for_data_reply(Conn, Stream, Timeout, StartTime)
    after RemainingTime ->
        quicer:async_shutdown_stream(Stream, ?QUIC_STREAM_SHUTDOWN_FLAG_ABORT, 0),
        quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
        {error, timeout}
    end.

%% @doc Send a message using NAT-aware routing (hole punch, relay fallback).
%% Use this when sending to peers that may be behind NAT.
%% LocalNodeId is required for hole punch coordination.
-spec send_message_nat_aware(binary(), binary(), atom(), map()) -> ok | {error, term()}.
send_message_nat_aware(LocalNodeId, TargetNodeId, MessageType, Message) ->
    send_message_nat_aware(LocalNodeId, TargetNodeId, MessageType, Message, #{}).

%% @doc Send a message using NAT-aware routing with options.
%% Options:
%%   - endpoint: Target endpoint (if known, skips DHT lookup)
%%   - relay_endpoint: Specific relay to use
%%   - skip_hole_punch: true to skip hole punch attempts
-spec send_message_nat_aware(binary(), binary(), atom(), map(), map()) -> ok | {error, term()}.
send_message_nat_aware(LocalNodeId, TargetNodeId, MessageType, Message, Opts) ->
    MessageBinary = macula_protocol_encoder:encode(MessageType, Message),

    %% Try endpoint from options first, then fall back to NAT-aware connection
    case maps:get(endpoint, Opts, undefined) of
        undefined ->
            send_via_nat_connector(LocalNodeId, TargetNodeId, MessageBinary, Opts);
        Endpoint ->
            %% Try direct first, then NAT-aware
            case send_message(Endpoint, MessageType, Message) of
                ok -> ok;
                {error, _Reason} ->
                    send_via_nat_connector(LocalNodeId, TargetNodeId, MessageBinary, Opts)
            end
    end.

%% @private Pool not running - fall back to direct connection
send_via_connection(undefined, Endpoint, MessageBinary) ->
    send_via_direct_connection(Endpoint, MessageBinary);
%% @private Pool available - use pooled connection
send_via_connection(_Pid, Endpoint, MessageBinary) ->
    send_via_pool(Endpoint, MessageBinary).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
%% @doc Send message using connection pool (preferred, 1.5-2x faster).
send_via_pool(Endpoint, MessageBinary) ->
    ConnResult = macula_peer_connection_pool:get_connection(Endpoint),
    do_pool_send(ConnResult, Endpoint, MessageBinary).

%% @private Pool connection acquired - attempt send
do_pool_send({ok, Conn, Stream}, Endpoint, MessageBinary) ->
    SendResult = macula_quic:send(Stream, MessageBinary),
    handle_pool_send_result(SendResult, Conn, Stream, Endpoint, MessageBinary);
%% @private Pool connection failed - fall back to direct
do_pool_send({error, Reason}, Endpoint, MessageBinary) ->
    ?LOG_DEBUG("Pool connection failed: ~p, using direct", [Reason]),
    send_via_direct_connection(Endpoint, MessageBinary).

%% @private Send succeeded - return connection to pool
handle_pool_send_result(ok, Conn, Stream, Endpoint, _MessageBinary) ->
    macula_peer_connection_pool:return_connection(Endpoint, {Conn, Stream}),
    ok;
%% @private Send failed - invalidate and retry with direct
handle_pool_send_result({error, Reason}, _Conn, _Stream, Endpoint, MessageBinary) ->
    macula_peer_connection_pool:invalidate(Endpoint),
    ?LOG_WARNING("Pool send failed: ~p, falling back to direct", [Reason]),
    send_via_direct_connection(Endpoint, MessageBinary).

%% @private
%% @doc Send message via direct connection (fallback, creates new connection).
send_via_direct_connection(Endpoint, MessageBinary) ->
    ParseResult = parse_endpoint(Endpoint),
    do_direct_send(ParseResult, Endpoint, MessageBinary).

%% @private Endpoint parsed successfully
do_direct_send({ok, Host, Port}, _Endpoint, MessageBinary) ->
    send_via_quic(Host, Port, MessageBinary, 5000);
%% @private Invalid endpoint format
do_direct_send({error, Reason}, Endpoint, _MessageBinary) ->
    ?LOG_ERROR("Invalid endpoint ~p: ~p", [Endpoint, Reason]),
    {error, {invalid_endpoint, Reason}}.

%% @private
%% @doc Parse endpoint string into host and port.
%% Supports formats: "host:port", "https://host:port", "http://host:port"
parse_endpoint(Endpoint) when is_binary(Endpoint) ->
    parse_endpoint(binary_to_list(Endpoint));
parse_endpoint(Endpoint) when is_list(Endpoint) ->
    %% Strip protocol prefix if present
    Stripped = strip_protocol(Endpoint),
    %% Split from the end to handle IPv6 addresses correctly
    %% For "host:port", splits into ["host", "port"]
    case string:split(Stripped, ":", trailing) of
        [Host, PortStr] -> parse_port(Host, PortStr);
        _ -> {error, invalid_format}
    end.

%% @private Strip protocol prefix (https://, http://) from endpoint
strip_protocol("https://" ++ Rest) -> Rest;
strip_protocol("http://" ++ Rest) -> Rest;
strip_protocol(Endpoint) -> Endpoint.

%% @private Parse port string to integer
parse_port(Host, PortStr) ->
    parse_port_result(Host, catch list_to_integer(PortStr)).

%% @private Port parsed successfully
parse_port_result(Host, Port) when is_integer(Port), Port > 0, Port < 65536 ->
    {ok, Host, Port};
%% @private Invalid port value or parse error
parse_port_result(_Host, _) ->
    {error, invalid_port}.

%% @private
%% @doc Send message via direct QUIC connection (legacy fallback).
send_via_quic(Host, Port, MessageBinary, Timeout) ->
    ConnectOpts = [
        {alpn, ["macula"]},
        {verify, none},
        {idle_timeout_ms, 60000},
        {keep_alive_interval_ms, 20000},
        {handshake_idle_timeout_ms, 30000}
    ],
    ConnResult = macula_quic:connect(Host, Port, ConnectOpts, Timeout),
    do_quic_connect(ConnResult, MessageBinary).

%% @private Connection established - open stream
do_quic_connect({ok, Conn}, MessageBinary) ->
    StreamResult = macula_quic:open_stream(Conn),
    do_quic_stream(StreamResult, Conn, MessageBinary);
%% @private Transport down
do_quic_connect({error, transport_down, _Details}, _MessageBinary) ->
    {error, {connect_failed, transport_down}};
%% @private Connection failed
do_quic_connect({error, Reason}, _MessageBinary) ->
    {error, {connect_failed, Reason}}.

%% @private Stream opened - send message
do_quic_stream({ok, Stream}, Conn, MessageBinary) ->
    SendResult = macula_quic:send(Stream, MessageBinary),
    do_quic_send(SendResult, Conn, Stream);
%% @private Stream open failed
do_quic_stream({error, Reason}, Conn, _MessageBinary) ->
    quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
    {error, {stream_failed, Reason}}.

%% @private Send succeeded - graceful shutdown
do_quic_send(ok, Conn, Stream) ->
    timer:sleep(50),
    quicer:async_shutdown_stream(Stream, ?QUIC_STREAM_SHUTDOWN_FLAG_GRACEFUL, 0),
    quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
    ok;
%% @private Send failed - abort stream
do_quic_send({error, Reason}, Conn, Stream) ->
    quicer:async_shutdown_stream(Stream, ?QUIC_STREAM_SHUTDOWN_FLAG_ABORT, 0),
    quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
    {error, {send_failed, Reason}}.

%%%===================================================================
%%% NAT-Aware Routing (v0.12.0+)
%%%===================================================================

%% @private
%% @doc Send message via NAT-aware connector (hole punch, relay fallback).
-spec send_via_nat_connector(binary(), binary(), binary(), map()) -> ok | {error, term()}.
send_via_nat_connector(LocalNodeId, TargetNodeId, MessageBinary, Opts) ->
    ?LOG_DEBUG("NAT-aware send: ~s -> ~s", [LocalNodeId, TargetNodeId]),

    %% Use macula_nat_connector for intelligent routing
    ConnectResult = macula_nat_connector:connect(LocalNodeId, TargetNodeId, Opts),
    send_on_nat_connection(ConnectResult, MessageBinary, TargetNodeId).

%% @private NAT connection established - send message
send_on_nat_connection({ok, Conn, Strategy}, MessageBinary, TargetNodeId) ->
    ?LOG_INFO("NAT connection established to ~s via ~p", [TargetNodeId, Strategy]),
    StreamResult = macula_quic:open_stream(Conn),
    send_on_nat_stream(StreamResult, Conn, MessageBinary, Strategy);
%% @private NAT connection failed
send_on_nat_connection({error, Reason}, _MessageBinary, TargetNodeId) ->
    ?LOG_WARNING("NAT connection to ~s failed: ~p", [TargetNodeId, Reason]),
    {error, {nat_connection_failed, Reason}}.

%% @private Stream opened - send message
send_on_nat_stream({ok, Stream}, Conn, MessageBinary, Strategy) ->
    SendResult = macula_quic:send(Stream, MessageBinary),
    handle_nat_send_result(SendResult, Conn, Stream, Strategy);
%% @private Stream open failed
send_on_nat_stream({error, Reason}, Conn, _MessageBinary, _Strategy) ->
    macula_nat_connector:disconnect(Conn),
    {error, {stream_failed, Reason}}.

%% @private Send succeeded - graceful shutdown and return connection to pool
handle_nat_send_result(ok, Conn, Stream, Strategy) ->
    timer:sleep(50),
    quicer:async_shutdown_stream(Stream, ?QUIC_STREAM_SHUTDOWN_FLAG_GRACEFUL, 0),
    %% Don't disconnect - keep connection for potential reuse
    ?LOG_DEBUG("NAT-aware send succeeded via ~p", [Strategy]),
    maybe_cache_nat_connection(Conn, Strategy),
    ok;
%% @private Send failed
handle_nat_send_result({error, Reason}, Conn, Stream, _Strategy) ->
    quicer:async_shutdown_stream(Stream, ?QUIC_STREAM_SHUTDOWN_FLAG_ABORT, 0),
    macula_nat_connector:disconnect(Conn),
    {error, {send_failed, Reason}}.

%% @private Cache successful NAT connections for reuse
maybe_cache_nat_connection(_Conn, relay) ->
    %% Don't cache relay connections - they may have per-message overhead
    ok;
maybe_cache_nat_connection(Conn, _Strategy) ->
    %% For direct and hole_punch, could cache the connection
    %% For now, just disconnect to keep it simple
    macula_nat_connector:disconnect(Conn),
    ok.
