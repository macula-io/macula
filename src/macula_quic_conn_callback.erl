%%%-------------------------------------------------------------------
%%% @doc
%%% QUIC connection callback module for Macula.
%%% Implements quicer_connection behavior to handle connection lifecycle.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_conn_callback).

-behaviour(quicer_connection).

-include_lib("kernel/include/logger.hrl").

%% Callback init
-export([init/1]).

%% Connection Callbacks
-export([
    new_conn/3,
    connected/3,
    transport_shutdown/3,
    shutdown/3,
    closed/3,
    local_address_changed/3,
    peer_address_changed/3,
    streams_available/3,
    peer_needs_streams/3,
    resumed/3,
    nst_received/3,
    new_stream/3
]).

-export([handle_info/2]).

%%%===================================================================
%%% Callback Implementation
%%%===================================================================

%% @doc Initialize connection callback state
init(ConnOpts) when is_map(ConnOpts) ->
    ?LOG_DEBUG("Initializing with opts: ~p", [maps:keys(ConnOpts)]),
    {ok, ConnOpts};
init(ConnOpts) when is_list(ConnOpts) ->
    init(maps:from_list(ConnOpts)).

%% @doc Handle new connection
%% With quicer_server, streams are delivered automatically via new_stream/3
new_conn(Conn, #{version := Vsn}, #{gateway_pid := GatewayPid} = State) ->
    ?LOG_INFO("New connection: ~p (QUIC v~p)", [Conn, Vsn]),
    ?LOG_DEBUG("Gateway PID: ~p", [GatewayPid]),

    %% quicer_server will automatically deliver streams via new_stream/3 callback
    %% No need to spawn stream acceptor - quicer_server handles it
    ?LOG_DEBUG("Using quicer_server automatic stream delivery"),

    %% Store connection and gateway_pid in state
    {ok, State#{
        conn => Conn,
        gateway_pid => GatewayPid
    }};
new_conn(Conn, ConnProps, State) ->
    %% Fallback if gateway_pid not provided
    ?LOG_WARNING("New connection without gateway_pid: ~p", [Conn]),
    new_conn(Conn, ConnProps, State#{gateway_pid => whereis(macula_gateway)}).

%% @doc Handle connection established
connected(_Conn, _Flags, State) ->
    ?LOG_INFO("Connection established"),
    {ok, State}.

%% @doc Handle transport shutdown
transport_shutdown(Conn, #{error := Error, status := Status}, State) ->
    ?LOG_WARNING("Transport shutdown: Conn=~p, Error=~p, Status=~p",
              [Conn, Error, Status]),
    {ok, State}.

%% @doc Handle connection shutdown
shutdown(Conn, Reason, State) ->
    ?LOG_INFO("Connection shutdown: Conn=~p, Reason=~p", [Conn, Reason]),
    {ok, State}.

%% @doc Handle connection closed
closed(_Conn, _Flags, State) ->
    ?LOG_INFO("Connection closed"),
    {stop, normal, State}.

%% @doc Handle local address changed
local_address_changed(_Conn, _NewAddr, State) ->
    {ok, State}.

%% @doc Handle peer address changed (NAT rebinding)
%% This callback is triggered when the peer's observed address changes,
%% typically due to NAT rebinding. We need to:
%% 1. Log the address change
%% 2. Invalidate cached NAT profile for the peer
%% 3. Update connection tracking
peer_address_changed(Conn, NewAddr, #{gateway_pid := GatewayPid} = State) ->
    ?LOG_WARNING("Peer address changed!"),
    ?LOG_INFO("  Connection: ~p", [Conn]),
    ?LOG_INFO("  New address: ~p", [NewAddr]),

    %% Notify gateway of address change (it can update client tracking)
    GatewayPid ! {peer_address_changed, Conn, NewAddr},

    %% If we have a node_id for this connection, invalidate its NAT profile
    %% The new address means NAT rebinding occurred - cached profile is stale
    case maps:get(node_id, State, undefined) of
        undefined ->
            ?LOG_DEBUG("No node_id in state, skipping NAT cache invalidation");
        NodeId ->
            ?LOG_INFO("Invalidating NAT cache for node ~s", [NodeId]),
            case whereis(macula_nat_cache) of
                undefined ->
                    ?LOG_DEBUG("NAT cache not running");
                _Pid ->
                    macula_nat_cache:invalidate(NodeId)
            end
    end,

    {ok, State#{last_peer_address => NewAddr}};
peer_address_changed(Conn, NewAddr, State) ->
    %% Fallback without gateway_pid
    ?LOG_WARNING("Peer address changed (no gateway): ~p -> ~p", [Conn, NewAddr]),
    {ok, State#{last_peer_address => NewAddr}}.

%% @doc Handle streams available
streams_available(_Conn, #{bidi_streams := Bidi, unidi_streams := Unidi}, State) ->
    ?LOG_DEBUG("Streams available: Bidi=~p, Unidi=~p", [Bidi, Unidi]),
    {ok, State}.

%% @doc Handle peer needs streams
peer_needs_streams(_Conn, _Undefined, State) ->
    {ok, State}.

%% @doc Handle connection resumed
resumed(_Conn, _Data, State) ->
    {ok, State}.

%% @doc Handle NST received (not used for server)
nst_received(_Conn, _Data, State) ->
    {stop, no_nst_for_server, State}.

%% @doc Handle new stream
%% With quicer_server, ALL streams are delivered here (not just orphans)
%% Forward them to the gateway for processing
new_stream(Stream, Props, #{gateway_pid := GatewayPid} = State) ->
    ?LOG_DEBUG("========================================"),
    ?LOG_INFO("NEW STREAM RECEIVED!"),
    ?LOG_DEBUG("Stream: ~p", [Stream]),
    ?LOG_DEBUG("Props: ~p", [Props]),
    ?LOG_DEBUG("========================================"),

    %% Forward stream to gateway
    ?LOG_DEBUG("Forwarding stream to gateway: ~p", [GatewayPid]),
    GatewayPid ! {quic_stream, Stream, Props},

    {ok, State};
new_stream(Stream, Props, State) ->
    %% Fallback if no gateway_pid in state
    ?LOG_WARNING("Stream received but no gateway_pid in state"),
    ?LOG_WARNING("Stream: ~p, Props: ~p", [Stream, Props]),
    {ok, State}.

%% @doc Handle other messages
handle_info(Info, State) ->
    ?LOG_WARNING("Unhandled info: ~p", [Info]),
    {ok, State}.
