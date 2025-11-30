%%%-------------------------------------------------------------------
%%% @doc
%%% Dedicated process for accepting QUIC streams on a connection.
%%%
%%% This process runs a blocking loop calling quicer:accept_stream/3,
%%% ensuring we're ready to accept streams before the client creates them.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_acceptor).

-include_lib("kernel/include/logger.hrl").

-export([start_link/2, init/2]).

%% @doc Start a stream acceptor for a connection.
%% Gateway is the process that should receive stream data events.
start_link(Conn, Gateway) ->
    Pid = spawn_link(?MODULE, init, [Conn, Gateway]),
    {ok, Pid}.

%% @doc Initialize the stream acceptor loop.
init(Conn, Gateway) ->
    ?LOG_INFO("Started for connection ~p, gateway ~p", [Conn, Gateway]),
    %% Register interest in incoming streams
    case quicer:async_accept_stream(Conn, #{}) of
        {ok, Conn} ->
            ?LOG_DEBUG("Async stream acceptance registered"),
            receive_loop(Conn, Gateway);
        {error, AcceptErr} ->
            ?LOG_ERROR("Failed to register async accept: ~p", [AcceptErr]),
            receive_loop(Conn, Gateway)
    end.

%% @doc Receive stream events in a passive loop.
receive_loop(Conn, Gateway) ->
    receive
        {quic, new_stream, Stream, _StreamProps} ->
            ?LOG_DEBUG("========================================"),
            ?LOG_INFO("STREAM RECEIVED: ~p", [Stream]),
            ?LOG_DEBUG("========================================"),

            %% Set stream to active mode for automatic data delivery
            case quicer:setopt(Stream, active, true) of
                ok ->
                    ?LOG_DEBUG("Stream set to active mode"),
                    %% Transfer ownership to gateway
                    case quicer:controlling_process(Stream, Gateway) of
                        ok ->
                            ?LOG_DEBUG("Stream ownership transferred to gateway"),
                            %% Notify gateway about new stream
                            Gateway ! {stream_accepted, Stream},
                            %% Continue receiving more streams
                            receive_loop(Conn, Gateway);
                        {error, TransferErr} ->
                            ?LOG_ERROR("Failed to transfer stream ownership: ~p", [TransferErr]),
                            quicer:close_stream(Stream),
                            receive_loop(Conn, Gateway)
                    end;
                {error, SetOptErr} ->
                    ?LOG_ERROR("Failed to set stream active: ~p", [SetOptErr]),
                    quicer:close_stream(Stream),
                    receive_loop(Conn, Gateway)
            end;

        {quic, closed, Conn, _Flags} ->
            ?LOG_INFO("Connection closed, exiting"),
            ok;

        Other ->
            ?LOG_WARNING("Unexpected message: ~p", [Other]),
            receive_loop(Conn, Gateway)
    end.
