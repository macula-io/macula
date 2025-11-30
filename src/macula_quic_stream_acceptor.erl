%%%-------------------------------------------------------------------
%%% @doc
%%% QUIC stream acceptor process.
%%% Dedicated process that waits for incoming streams on a connection
%%% and forwards them to the gateway for processing.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_quic_stream_acceptor).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

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
    ?LOG_INFO("Starting for connection ~p", [Conn]),
    ?LOG_DEBUG("Gateway PID: ~p", [GatewayPid]),

    %% Register for stream events with active mode
    StreamOpts = #{active => true},
    case quicer:async_accept_stream(Conn, StreamOpts) of
        {ok, Conn} ->
            ?LOG_DEBUG("Registered for streams (active mode)"),
            {ok, #state{
                gateway_pid = GatewayPid,
                conn = Conn,
                accepting = true
            }};
        {error, Reason} ->
            ?LOG_ERROR("Failed to register: ~p", [Reason]),
            {stop, Reason}
    end.

handle_call(Request, _From, State) ->
    ?LOG_WARNING("Unexpected call: ~p", [Request]),
    {reply, {error, unknown_call}, State}.

handle_cast(Msg, State) ->
    ?LOG_WARNING("Unexpected cast: ~p", [Msg]),
    {noreply, State}.

%% @doc Handle new stream from peer - THIS IS THE KEY MESSAGE!
handle_info({quic, new_stream, Stream, Props}, #state{gateway_pid = GatewayPid, conn = Conn} = State) ->
    ?LOG_DEBUG("========================================"),
    ?LOG_INFO("NEW STREAM RECEIVED!"),
    ?LOG_DEBUG("Stream: ~p", [Stream]),
    ?LOG_DEBUG("Props: ~p", [Props]),
    ?LOG_DEBUG("========================================"),

    %% Forward stream to gateway
    GatewayPid ! {quic_stream, Stream, Props},

    %% Register for next stream
    StreamOpts = #{active => true},
    case quicer:async_accept_stream(Conn, StreamOpts) of
        {ok, Conn} ->
            ?LOG_DEBUG("Re-registered for next stream"),
            {noreply, State};
        {error, Reason} ->
            ?LOG_ERROR("Failed to re-register: ~p", [Reason]),
            {stop, {re_register_failed, Reason}, State}
    end;

%% Handle stream data (if stream is in active mode)
handle_info({quic, Data, Stream, Flags}, #state{gateway_pid = GatewayPid} = State) when is_binary(Data) ->
    ?LOG_DEBUG("Stream data: Stream=~p, Size=~p bytes",
              [Stream, byte_size(Data)]),
    %% Forward to gateway
    GatewayPid ! {quic_stream_data, Stream, Data, Flags},
    {noreply, State};

%% Handle stream closed
handle_info({quic, stream_closed, Stream, Flags}, State) ->
    ?LOG_INFO("Stream closed: ~p, Flags: ~p", [Stream, Flags]),
    {noreply, State};

%% Handle connection closed
handle_info({quic, closed, Conn, _Flags}, #state{conn = Conn} = State) ->
    ?LOG_INFO("Connection closed: ~p", [Conn]),
    {stop, normal, State};

handle_info(Info, State) ->
    ?LOG_WARNING("Unhandled message: ~p", [Info]),
    {noreply, State}.

terminate(Reason, #state{conn = Conn}) ->
    ?LOG_INFO("Terminating: ~p (Conn: ~p)", [Reason, Conn]),
    ok.
