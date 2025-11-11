%%%-------------------------------------------------------------------
%%% @doc
%%% Dedicated process for accepting QUIC streams on a connection.
%%%
%%% This process runs a blocking loop calling quicer:accept_stream/3,
%%% ensuring we're ready to accept streams before the client creates them.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_stream_acceptor).

-export([start_link/2, init/2]).

%% @doc Start a stream acceptor for a connection.
%% Gateway is the process that should receive stream data events.
start_link(Conn, Gateway) ->
    Pid = spawn_link(?MODULE, init, [Conn, Gateway]),
    {ok, Pid}.

%% @doc Initialize the stream acceptor loop.
init(Conn, Gateway) ->
    io:format("[StreamAcceptor] Started for connection ~p, gateway ~p~n", [Conn, Gateway]),
    %% Register interest in incoming streams
    case quicer:async_accept_stream(Conn, #{}) of
        {ok, Conn} ->
            io:format("[StreamAcceptor] Async stream acceptance registered~n"),
            receive_loop(Conn, Gateway);
        {error, AcceptErr} ->
            io:format("[StreamAcceptor] ERROR: Failed to register async accept: ~p~n", [AcceptErr]),
            receive_loop(Conn, Gateway)
    end.

%% @doc Receive stream events in a passive loop.
receive_loop(Conn, Gateway) ->
    receive
        {quic, new_stream, Stream, _StreamProps} ->
            io:format("[StreamAcceptor] ========================================~n"),
            io:format("[StreamAcceptor] STREAM RECEIVED: ~p~n", [Stream]),
            io:format("[StreamAcceptor] ========================================~n"),

            %% Set stream to active mode for automatic data delivery
            case quicer:setopt(Stream, active, true) of
                ok ->
                    io:format("[StreamAcceptor] Stream set to active mode~n"),
                    %% Transfer ownership to gateway
                    case quicer:controlling_process(Stream, Gateway) of
                        ok ->
                            io:format("[StreamAcceptor] Stream ownership transferred to gateway~n"),
                            %% Notify gateway about new stream
                            Gateway ! {stream_accepted, Stream},
                            %% Continue receiving more streams
                            receive_loop(Conn, Gateway);
                        {error, TransferErr} ->
                            io:format("[StreamAcceptor] ERROR: Failed to transfer stream ownership: ~p~n", [TransferErr]),
                            quicer:close_stream(Stream),
                            receive_loop(Conn, Gateway)
                    end;
                {error, SetOptErr} ->
                    io:format("[StreamAcceptor] ERROR: Failed to set stream active: ~p~n", [SetOptErr]),
                    quicer:close_stream(Stream),
                    receive_loop(Conn, Gateway)
            end;

        {quic, closed, Conn, _Flags} ->
            io:format("[StreamAcceptor] Connection closed, exiting~n"),
            ok;

        Other ->
            io:format("[StreamAcceptor] Unexpected message: ~p~n", [Other]),
            receive_loop(Conn, Gateway)
    end.
