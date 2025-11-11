%%%-------------------------------------------------------------------
%%% @doc
%%% QUIC stream acceptor process.
%%% Dedicated process that waits for incoming streams on a connection
%%% and forwards them to the gateway for processing.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_stream_acceptor).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    gateway_pid :: pid(),
    conn :: term(),
    accepting = false :: boolean()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start stream acceptor process
-spec start_link(pid(), term()) -> {ok, pid()} | {error, term()}.
start_link(GatewayPid, Conn) ->
    gen_server:start_link(?MODULE, [GatewayPid, Conn], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([GatewayPid, Conn]) ->
    io:format("[StreamAcceptor] Starting for connection ~p~n", [Conn]),
    io:format("[StreamAcceptor] Gateway PID: ~p~n", [GatewayPid]),

    %% Register for stream events with active mode
    StreamOpts = #{active => true},
    case quicer:async_accept_stream(Conn, StreamOpts) of
        {ok, Conn} ->
            io:format("[StreamAcceptor] Registered for streams (active mode)~n"),
            {ok, #state{
                gateway_pid = GatewayPid,
                conn = Conn,
                accepting = true
            }};
        {error, Reason} ->
            io:format("[StreamAcceptor] ERROR: Failed to register: ~p~n", [Reason]),
            {stop, Reason}
    end.

handle_call(Request, _From, State) ->
    io:format("[StreamAcceptor] Unexpected call: ~p~n", [Request]),
    {reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
    io:format("[StreamAcceptor] Unexpected cast: ~p~n", [Msg]),
    {noreply, State}.

%% @doc Handle new stream from peer - THIS IS THE KEY MESSAGE!
handle_info({quic, new_stream, Stream, Props}, #state{gateway_pid = GatewayPid, conn = Conn} = State) ->
    io:format("[StreamAcceptor] ========================================~n"),
    io:format("[StreamAcceptor] NEW STREAM RECEIVED!~n"),
    io:format("[StreamAcceptor] Stream: ~p~n", [Stream]),
    io:format("[StreamAcceptor] Props: ~p~n", [Props]),
    io:format("[StreamAcceptor] ========================================~n"),

    %% Forward stream to gateway
    GatewayPid ! {quic_stream, Stream, Props},

    %% Register for next stream
    StreamOpts = #{active => true},
    case quicer:async_accept_stream(Conn, StreamOpts) of
        {ok, Conn} ->
            io:format("[StreamAcceptor] Re-registered for next stream~n"),
            {noreply, State};
        {error, Reason} ->
            io:format("[StreamAcceptor] ERROR: Failed to re-register: ~p~n", [Reason]),
            {stop, {re_register_failed, Reason}, State}
    end;

%% Handle stream data (if stream is in active mode)
handle_info({quic, Data, Stream, Flags}, #state{gateway_pid = GatewayPid} = State) when is_binary(Data) ->
    io:format("[StreamAcceptor] Stream data: Stream=~p, Size=~p bytes~n",
              [Stream, byte_size(Data)]),
    %% Forward to gateway
    GatewayPid ! {quic_stream_data, Stream, Data, Flags},
    {noreply, State};

%% Handle stream closed
handle_info({quic, stream_closed, Stream, Flags}, State) ->
    io:format("[StreamAcceptor] Stream closed: ~p, Flags: ~p~n", [Stream, Flags]),
    {noreply, State};

%% Handle connection closed
handle_info({quic, closed, Conn, _Flags}, #state{conn = Conn} = State) ->
    io:format("[StreamAcceptor] Connection closed: ~p~n", [Conn]),
    {stop, normal, State};

handle_info(Info, State) ->
    io:format("[StreamAcceptor] Unhandled message: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, #state{conn = Conn}) ->
    io:format("[StreamAcceptor] Terminating: ~p (Conn: ~p)~n", [Reason, Conn]),
    ok.
