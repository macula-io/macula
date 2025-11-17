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
%%% Pattern: Stateless utility module (no GenServer)
%%%   - Direct QUIC connections using `macula_quic'
%%%   - Fire-and-forget message sending
%%%   - No connection pooling (future optimization for v0.9.0)
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
%%% v0.8.0: Fire-and-forget pattern
%%%   - Creates new connection per message
%%%   - Simple but inefficient for high-frequency messaging
%%%
%%% v0.9.0 (planned): Connection pooling
%%%   - Reuse existing connections
%%%   - 10x performance improvement for repeated messaging
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
%% Establishes connection, sends message, closes connection.
%% This is simple but creates a new connection per message.
-spec send_message(binary(), atom(), map()) -> ok | {error, term()}.
send_message(Endpoint, MessageType, Message) ->
    send_message(Endpoint, MessageType, Message, 5000).

%% @doc Send a message to a remote peer with custom timeout.
-spec send_message(binary(), atom(), map(), timeout()) -> ok | {error, term()}.
send_message(Endpoint, MessageType, Message, Timeout) ->
    %% Parse endpoint (format: "host:port" or <<"host:port">>)
    case parse_endpoint(Endpoint) of
        {ok, Host, Port} ->
            %% Encode message
            MessageBinary = macula_protocol_encoder:encode(MessageType, Message),

            %% Send via QUIC
            send_via_quic(Host, Port, MessageBinary, Timeout);
        {error, Reason} ->
            io:format("[PeerConnector] Invalid endpoint ~p: ~p~n", [Endpoint, Reason]),
            {error, {invalid_endpoint, Reason}}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

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
%% @doc Send message via QUIC connection.
send_via_quic(Host, Port, MessageBinary, Timeout) ->
    io:format("[PeerConnector] Connecting to ~s:~p~n", [Host, Port]),

    %% Connect to peer
    ConnectOpts = [
        {alpn, ["macula"]},
        {verify, none}  %% TODO(v0.9.0): Add proper certificate verification - see TODO.md
    ],

    case macula_quic:connect(Host, Port, ConnectOpts, Timeout) of
        {ok, Conn} ->
            %% Open stream
            case macula_quic:open_stream(Conn) of
                {ok, Stream} ->
                    %% Send message
                    Result = macula_quic:send(Stream, MessageBinary),

                    case Result of
                        ok ->
                            io:format("[PeerConnector] Message sent successfully to ~s:~p~n", [Host, Port]),

                            %% Wait a bit for data to be transmitted before closing
                            %% This prevents race condition where receiver tries to read from closed stream
                            timer:sleep(100),

                            %% Close stream and connection gracefully
                            quicer:async_shutdown_stream(Stream, ?QUIC_STREAM_SHUTDOWN_FLAG_GRACEFUL, 0),
                            quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
                            ok;
                        {error, Reason} ->
                            io:format("[PeerConnector] Failed to send message: ~p~n", [Reason]),
                            quicer:async_shutdown_stream(Stream, ?QUIC_STREAM_SHUTDOWN_FLAG_ABORT, 0),
                            quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
                            {error, {send_failed, Reason}}
                    end;
                {error, Reason} ->
                    io:format("[PeerConnector] Failed to open stream: ~p~n", [Reason]),
                    quicer:async_shutdown_connection(Conn, ?QUIC_CONNECTION_SHUTDOWN_FLAG_NONE, 0),
                    {error, {stream_failed, Reason}}
            end;
        {error, transport_down, Details} ->
            io:format("[PeerConnector] Failed to connect to ~s:~p: transport_down ~p~n", [Host, Port, Details]),
            {error, {connect_failed, transport_down}};
        {error, Reason} ->
            io:format("[PeerConnector] Failed to connect to ~s:~p: ~p~n", [Host, Port, Reason]),
            {error, {connect_failed, Reason}}
    end.
