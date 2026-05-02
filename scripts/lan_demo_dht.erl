%%%-------------------------------------------------------------------
%%% @doc Tiny TCP-based mock DHT server for the netns3 Phase 2 demo.
%%%
%%% Listens on a fixed port, accepts repeated length-prefixed
%%% Erlang-term frames, and serves `put' / `find' against an
%%% in-memory map. Both demo nodes (netns A/B/C) connect here for
%%% advertise_station's put_fn and resolve_address's find_fn.
%%%
%%% Wire format per frame: `<<Len:32/big, Bin:Len/binary>>' where Bin
%%% = `term_to_binary(Msg)'. Replies use the same shape on the same
%%% socket.
%%%
%%% Messages:
%%%   {put,  Record}  -> ok | {error, term()}
%%%   {find, Key}     -> {ok, Record} | {error, not_found}
%%% @end
%%%-------------------------------------------------------------------
-module(lan_demo_dht).

-export([start/0, start/1, stop/0]).

-define(DEFAULT_PORT, 5555).
-define(SERVER, ?MODULE).
-define(TABLE,  lan_demo_dht_table).

%% =============================================================================
%% Public API
%% =============================================================================

start() -> start(?DEFAULT_PORT).

start(Port) when is_integer(Port) ->
    ensure_table(),
    {ok, ListenSock} = gen_tcp:listen(Port, [
        binary, {packet, 0}, {active, false},
        {reuseaddr, true}, {ip, {0,0,0,0}}
    ]),
    Pid = spawn(fun() -> register(?SERVER, self()), accept_loop(ListenSock) end),
    io:format("[dht] listening on port ~p, pid ~p~n", [Port, Pid]),
    {ok, Pid}.

stop() ->
    case whereis(?SERVER) of
        undefined -> ok;
        Pid       -> exit(Pid, kill), unregister(?SERVER), ok
    end.

%% =============================================================================
%% Server internals
%% =============================================================================

ensure_table() ->
    case ets:info(?TABLE) of
        undefined -> ets:new(?TABLE, [named_table, public, set]);
        _         -> ok
    end.

accept_loop(ListenSock) ->
    case gen_tcp:accept(ListenSock) of
        {ok, Sock} ->
            spawn(fun() -> client_loop(Sock) end),
            accept_loop(ListenSock);
        {error, _} = Err ->
            io:format("[dht] accept stopped: ~p~n", [Err])
    end.

client_loop(Sock) ->
    case read_frame(Sock) of
        {ok, Bin}  -> dispatch(binary_to_term(Bin), Sock), client_loop(Sock);
        {error, _} -> gen_tcp:close(Sock)
    end.

dispatch({put, Record}, Sock) ->
    Key = macula_record:storage_key(Record),
    true = ets:insert(?TABLE, {Key, Record}),
    send_frame(Sock, ok);
dispatch({find, Key}, Sock) ->
    Reply = case ets:lookup(?TABLE, Key) of
        [{_, R}] -> {ok, R};
        []       -> {error, not_found}
    end,
    send_frame(Sock, Reply);
dispatch(Other, Sock) ->
    send_frame(Sock, {error, {bad_msg, Other}}).

read_frame(Sock) ->
    case gen_tcp:recv(Sock, 4) of
        {ok, <<Len:32/big>>} -> gen_tcp:recv(Sock, Len);
        {error, _} = E -> E
    end.

send_frame(Sock, Term) ->
    Bin = term_to_binary(Term),
    Frame = <<(byte_size(Bin)):32/big, Bin/binary>>,
    gen_tcp:send(Sock, Frame).
