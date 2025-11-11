%%%-------------------------------------------------------------------
%%% @doc
%%% QUIC connection callback module for Macula.
%%% Implements quicer_connection behavior to handle connection lifecycle.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_conn_callback).

-behavior(quicer_connection).

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
    io:format("[ConnCallback] Initializing with opts: ~p~n", [maps:keys(ConnOpts)]),
    {ok, ConnOpts};
init(ConnOpts) when is_list(ConnOpts) ->
    init(maps:from_list(ConnOpts)).

%% @doc Handle new connection
%% Spawns dedicated stream acceptor process for this connection
new_conn(Conn, #{version := Vsn}, #{gateway_pid := GatewayPid} = State) ->
    io:format("[ConnCallback] New connection: ~p (QUIC v~p)~n", [Conn, Vsn]),

    %% Spawn stream acceptor process
    case macula_quic_stream_acceptor:start_link(GatewayPid, Conn) of
        {ok, AcceptorPid} ->
            io:format("[ConnCallback] Stream acceptor started: ~p~n", [AcceptorPid]),
            %% Complete handshake
            ok = quicer:async_handshake(Conn),
            {ok, State#{
                conn => Conn,
                stream_acceptor => AcceptorPid
            }};
        {error, Reason} = Error ->
            io:format("[ConnCallback] Failed to start stream acceptor: ~p~n", [Reason]),
            Error
    end;
new_conn(Conn, ConnProps, State) ->
    %% Fallback if gateway_pid not provided
    io:format("[ConnCallback] New connection without gateway_pid: ~p~n", [Conn]),
    new_conn(Conn, ConnProps, State#{gateway_pid => whereis(macula_gateway)}).

%% @doc Handle connection established
connected(_Conn, _Flags, State) ->
    io:format("[ConnCallback] Connection established~n"),
    {ok, State}.

%% @doc Handle transport shutdown
transport_shutdown(Conn, #{error := Error, status := Status}, State) ->
    io:format("[ConnCallback] Transport shutdown: Conn=~p, Error=~p, Status=~p~n",
              [Conn, Error, Status]),
    {ok, State}.

%% @doc Handle connection shutdown
shutdown(Conn, Reason, State) ->
    io:format("[ConnCallback] Connection shutdown: Conn=~p, Reason=~p~n", [Conn, Reason]),
    {ok, State}.

%% @doc Handle connection closed
closed(_Conn, _Flags, State) ->
    io:format("[ConnCallback] Connection closed~n"),
    {stop, normal, State}.

%% @doc Handle local address changed
local_address_changed(_Conn, _NewAddr, State) ->
    {ok, State}.

%% @doc Handle peer address changed
peer_address_changed(_Conn, _NewAddr, State) ->
    {ok, State}.

%% @doc Handle streams available
streams_available(_Conn, #{bidi_streams := Bidi, unidi_streams := Unidi}, State) ->
    io:format("[ConnCallback] Streams available: Bidi=~p, Unidi=~p~n", [Bidi, Unidi]),
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

%% @doc Handle new stream when there's no stream acceptor waiting (orphan stream)
%% This shouldn't happen if stream acceptor is working correctly
new_stream(Stream, #{is_orphan := true} = Props, #{gateway_pid := GatewayPid} = State) ->
    io:format("[ConnCallback] WARNING: Orphan stream received: ~p~n", [Stream]),
    io:format("[ConnCallback] Stream props: ~p~n", [Props]),

    %% Forward directly to gateway as fallback
    GatewayPid ! {quic_stream, Stream, Props},

    {ok, State};
new_stream(Stream, Props, State) ->
    io:format("[ConnCallback] Unexpected new_stream call: ~p, Props: ~p~n", [Stream, Props]),
    {ok, State}.

%% @doc Handle other messages
handle_info(Info, State) ->
    io:format("[ConnCallback] Unhandled info: ~p~n", [Info]),
    {ok, State}.
