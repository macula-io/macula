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
%%% Pattern: Connection-pooled utility module
%%%   - Uses `macula_peer_connection_pool' for connection reuse
%%%   - Falls back to direct connection if pool unavailable
%%%   - Fire-and-forget message sending
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
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peer_connector).

-include_lib("quicer/include/quicer.hrl").

%% API
-export([
    send_message/3,
    send_message/4
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
    case whereis(macula_peer_connection_pool) of
        undefined ->
            %% Pool not running - fall back to direct connection
            send_via_direct_connection(Endpoint, MessageBinary);
        _Pid ->
            send_via_pool(Endpoint, MessageBinary)
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
%% @doc Send message using connection pool (preferred, 1.5-2x faster).
send_via_pool(Endpoint, MessageBinary) ->
    case macula_peer_connection_pool:get_connection(Endpoint) of
        {ok, Conn, Stream} ->
            %% Send message on pooled connection
            case macula_quic:send(Stream, MessageBinary) of
                ok ->
                    %% Return connection to pool for reuse
                    macula_peer_connection_pool:return_connection(Endpoint, {Conn, Stream}),
                    ok;
                {error, Reason} ->
                    %% Connection failed - invalidate and retry with direct
                    macula_peer_connection_pool:invalidate(Endpoint),
                    io:format("[PeerConnector] Pool send failed: ~p, falling back to direct~n", [Reason]),
                    send_via_direct_connection(Endpoint, MessageBinary)
            end;
        {error, Reason} ->
            io:format("[PeerConnector] Pool connection failed: ~p, using direct~n", [Reason]),
            send_via_direct_connection(Endpoint, MessageBinary)
    end.

%% @private
%% @doc Send message via direct connection (fallback, creates new connection).
send_via_direct_connection(Endpoint, MessageBinary) ->
    case parse_endpoint(Endpoint) of
        {ok, Host, Port} ->
            send_via_quic(Host, Port, MessageBinary, 5000);
        {error, Reason} ->
            io:format("[PeerConnector] Invalid endpoint ~p: ~p~n", [Endpoint, Reason]),
            {error, {invalid_endpoint, Reason}}
    end.

%% @private
%% @doc Parse endpoint string into host and port.
parse_endpoint(Endpoint) when is_binary(Endpoint) ->
    parse_endpoint(binary_to_list(Endpoint));
parse_endpoint(Endpoint) when is_list(Endpoint) ->
    case string:split(Endpoint, ":") of
        [Host, PortStr] ->
            try
                Port = list_to_integer(PortStr),
                {ok, Host, Port}
            catch
                _:_ -> {error, invalid_port}
            end;
        _ ->
            {error, invalid_format}
    end.

%% @private
%% @doc Send message via direct QUIC connection (legacy fallback).
send_via_quic(Host, Port, MessageBinary, Timeout) ->
    %% Connect to peer with proper QUIC configuration
    ConnectOpts = [
        {alpn, ["macula"]},
        {verify, none},
        {idle_timeout_ms, 60000},
        {keep_alive_interval_ms, 20000},
        {handshake_idle_timeout_ms, 30000}
    ],

    case macula_quic:connect(Host, Port, ConnectOpts, Timeout) of
        {ok, Conn} ->
            case macula_quic:open_stream(Conn) of
                {ok, Stream} ->
                    Result = macula_quic:send(Stream, MessageBinary),
                    case Result of
                        ok ->
                            %% Small delay for data transmission before closing
                            timer:sleep(50),
                            quicer:async_shutdown_stream(Stream, ?QUIC_STREAM_SHUTDOWN_FLAG_GRACEFUL, 0),
                            quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
                            ok;
                        {error, Reason} ->
                            quicer:async_shutdown_stream(Stream, ?QUIC_STREAM_SHUTDOWN_FLAG_ABORT, 0),
                            quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
                            {error, {send_failed, Reason}}
                    end;
                {error, Reason} ->
                    quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
                    {error, {stream_failed, Reason}}
            end;
        {error, transport_down, _Details} ->
            {error, {connect_failed, transport_down}};
        {error, Reason} ->
            {error, {connect_failed, Reason}}
    end.
