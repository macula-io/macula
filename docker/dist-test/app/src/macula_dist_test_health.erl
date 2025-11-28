%%%-------------------------------------------------------------------
%%% @doc Simple HTTP Health Server for Distribution Testing.
%%%
%%% Provides a basic HTTP endpoint for Docker health checks.
%%% Uses a simple TCP socket to avoid adding cowboy dependency.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_test_health).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 9100).

-record(state, {
    listen_socket :: gen_tcp:socket() | undefined,
    port :: integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Port = get_health_port(),

    case gen_tcp:listen(Port, [
        binary,
        {packet, http},
        {active, false},
        {reuseaddr, true}
    ]) of
        {ok, ListenSocket} ->
            io:format("[Health] Listening on port ~p~n", [Port]),
            %% Start accepting connections
            self() ! accept,
            {ok, #state{listen_socket = ListenSocket, port = Port}};
        {error, Reason} ->
            io:format("[Health] Failed to listen on port ~p: ~p~n", [Port, Reason]),
            {stop, {listen_failed, Reason}}
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(accept, #state{listen_socket = ListenSocket} = State) ->
    %% Accept connection with timeout
    case gen_tcp:accept(ListenSocket, 1000) of
        {ok, Socket} ->
            %% Handle the request
            spawn(fun() -> handle_request(Socket) end),
            %% Continue accepting
            self() ! accept,
            {noreply, State};
        {error, timeout} ->
            %% No connection, try again
            self() ! accept,
            {noreply, State};
        {error, closed} ->
            {stop, normal, State};
        {error, Reason} ->
            io:format("[Health] Accept error: ~p~n", [Reason]),
            self() ! accept,
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listen_socket = ListenSocket}) ->
    case ListenSocket of
        undefined -> ok;
        _ -> gen_tcp:close(ListenSocket)
    end,
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

get_health_port() ->
    case os:getenv("HEALTH_PORT") of
        false -> ?DEFAULT_PORT;
        PortStr -> list_to_integer(PortStr)
    end.

handle_request(Socket) ->
    %% Read HTTP request
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, {http_request, 'GET', {abs_path, Path}, _Version}} ->
            %% Consume headers
            consume_headers(Socket),
            %% Handle path
            Response = handle_path(Path),
            gen_tcp:send(Socket, Response);
        {ok, {http_request, _Method, _Uri, _Version}} ->
            consume_headers(Socket),
            gen_tcp:send(Socket, method_not_allowed_response());
        _ ->
            ok
    end,
    gen_tcp:close(Socket).

consume_headers(Socket) ->
    case gen_tcp:recv(Socket, 0, 1000) of
        {ok, http_eoh} -> ok;
        {ok, {http_header, _, _, _, _}} -> consume_headers(Socket);
        _ -> ok
    end.

handle_path("/health") ->
    health_response();
handle_path("/status") ->
    status_response();
handle_path("/whisper") ->
    whisper_response(<<"Hello QUIC World!">>);
handle_path("/whisper/" ++ Message) ->
    whisper_response(list_to_binary(Message));
handle_path("/babel/history") ->
    babel_history_response();
handle_path("/") ->
    health_response();
handle_path(_) ->
    not_found_response().

health_response() ->
    Connected = nodes(),
    ClusterSize = length(Connected) + 1,

    Body = io_lib:format(
        "{\"status\":\"ok\",\"node\":\"~s\",\"cluster_size\":~p,\"connected\":~p}",
        [node(), ClusterSize, length(Connected)]
    ),

    http_response(200, "OK", "application/json", Body).

status_response() ->
    Status = macula_dist_test_connector:status(),
    Protocol = detect_distribution_protocol(),

    Body = io_lib:format(
        "{\"this_node\":\"~p\","
        "\"connected_nodes\":~p,"
        "\"cluster_size\":~p,"
        "\"distribution_protocol\":\"~s\"}",
        [
            maps:get(this_node, Status),
            [atom_to_list(N) || N <- maps:get(connected_nodes, Status)],
            maps:get(cluster_size, Status),
            Protocol
        ]
    ),

    http_response(200, "OK", "application/json", Body).

detect_distribution_protocol() ->
    case init:get_argument(proto_dist) of
        {ok, [["macula"]]} -> "macula_dist (QUIC)";
        {ok, [[Proto]]} -> Proto;
        _ -> "inet_tcp_dist (TCP)"
    end.

not_found_response() ->
    http_response(404, "Not Found", "text/plain", "Not Found").

whisper_response(Message) ->
    %% Start a whisper chain
    macula_dist_test_babel:whisper(Message),

    Connected = nodes(),
    Body = io_lib:format(
        "{\"action\":\"whisper_started\","
        "\"message\":\"~s\","
        "\"origin\":\"~p\","
        "\"target_nodes\":~p,"
        "\"note\":\"Check docker logs for whisper chain output\"}",
        [Message, node(), [atom_to_list(N) || N <- lists:sort(Connected)]]
    ),

    http_response(200, "OK", "application/json", Body).

babel_history_response() ->
    History = macula_dist_test_babel:history(),

    %% Format history entries as JSON
    Entries = lists:map(fun(Entry) ->
        io_lib:format("~p", [Entry])
    end, History),

    Body = io_lib:format(
        "{\"node\":\"~p\",\"history_count\":~p,\"entries\":[~s]}",
        [node(), length(History), string:join(Entries, ",")]
    ),

    http_response(200, "OK", "application/json", Body).

method_not_allowed_response() ->
    http_response(405, "Method Not Allowed", "text/plain", "Method Not Allowed").

http_response(Code, Status, ContentType, Body) ->
    BodyBin = iolist_to_binary(Body),
    iolist_to_binary([
        io_lib:format("HTTP/1.1 ~p ~s\r\n", [Code, Status]),
        io_lib:format("Content-Type: ~s\r\n", [ContentType]),
        io_lib:format("Content-Length: ~p\r\n", [byte_size(BodyBin)]),
        "Connection: close\r\n",
        "\r\n",
        BodyBin
    ]).
