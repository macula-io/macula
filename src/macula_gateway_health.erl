%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Gateway Health Check Server
%%%
%%% Provides HTTP health endpoints for Kubernetes liveness and readiness probes.
%%% Runs on a separate port (8080) from the main QUIC gateway (9443).
%%%
%%% Endpoints:
%%%   GET /health       - Overall health status
%%%   GET /ready        - Readiness check (can accept traffic)
%%%   GET /live         - Liveness check (process is alive)
%%%   GET /metrics      - Basic metrics (optional)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_gateway_health).

-behaviour(gen_server).

-include("macula_config.hrl").

%% API
-export([
    start_link/1,
    stop/0,
    is_healthy/0,
    set_ready/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {
    port :: inet:port_number(),
    listen_socket :: gen_tcp:socket() | undefined,
    ready :: boolean(),
    started_at :: integer()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the health check server.
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Stop the health check server.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Check if the gateway is healthy.
-spec is_healthy() -> boolean().
is_healthy() ->
    try
        gen_server:call(?MODULE, is_healthy, 1000)
    catch
        _:_ -> false
    end.

%% @doc Set the readiness state.
-spec set_ready(boolean()) -> ok.
set_ready(Ready) ->
    gen_server:cast(?MODULE, {set_ready, Ready}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    Port = proplists:get_value(health_port, Opts, ?DEFAULT_HEALTH_PORT),

    io:format("Starting health check server on port ~p~n", [Port]),

    case gen_tcp:listen(Port, [
        {active, false},
        {reuseaddr, true},
        binary
    ]) of
        {ok, ListenSocket} ->
            %% Start accepting connections
            spawn_link(fun() -> accept_loop(ListenSocket) end),

            io:format("Health check server listening on port ~p~n", [Port]),

            State = #state{
                port = Port,
                listen_socket = ListenSocket,
                ready = false,  % Not ready until gateway starts
                started_at = erlang:system_time(second)
            },

            {ok, State};

        {error, Reason} ->
            io:format("Failed to start health server: ~p~n", [Reason]),
            {stop, {health_server_failed, Reason}}
    end.

handle_call(is_healthy, _From, State) ->
    {reply, true, State};

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({set_ready, Ready}, State) ->
    {noreply, State#state{ready = Ready}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{listen_socket = Socket}) ->
    case Socket of
        undefined -> ok;
        _ -> gen_tcp:close(Socket)
    end,
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private
accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket, 5000) of
        {ok, Socket} ->
            spawn(fun() -> handle_request(Socket) end),
            accept_loop(ListenSocket);
        {error, timeout} ->
            accept_loop(ListenSocket);
        {error, closed} ->
            ok;
        {error, Reason} ->
            io:format("Accept error: ~p~n", [Reason]),
            timer:sleep(1000),
            accept_loop(ListenSocket)
    end.

%% @private
handle_request(Socket) ->
    %% Read the HTTP request line (e.g., "GET /health HTTP/1.1\r\n")
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Data} ->
            %% Parse the request line to extract the path
            Path = parse_http_request(Data),

            %% Handle the request
            Response = case Path of
                <<"/health">> -> health_response();
                <<"/ready">> -> ready_response();
                <<"/live">> -> live_response();
                <<"/metrics">> -> metrics_response();
                _ -> not_found_response()
            end,

            gen_tcp:send(Socket, Response),
            gen_tcp:close(Socket);

        {error, _Reason} ->
            gen_tcp:close(Socket)
    end.

%% @private
parse_http_request(Data) ->
    %% Extract path from HTTP request line like "GET /path HTTP/1.1\r\n..."
    extract_path(binary:split(Data, <<" ">>, [global])).

%% @doc Extract path from HTTP request parts.
extract_path([<<"GET">>, Path | _Rest]) ->
    %% Remove query string if present
    strip_query_string(binary:split(Path, <<"?">>));
extract_path(_) ->
    <<"/">>.

%% @doc Strip query string from path.
strip_query_string([PathOnly | _]) -> PathOnly;
strip_query_string(_) -> <<"/">>.

%% @private
health_response() ->
    try
        State = gen_server:call(?MODULE, get_state, 1000),
        Uptime = erlang:system_time(second) - State#state.started_at,

        Body = iolist_to_binary(io_lib:format(
            "{\"status\":\"healthy\",\"ready\":~s,\"uptime\":~p}",
            [atom_to_list(State#state.ready), Uptime]
        )),

        http_response(200, "OK", "application/json", Body)
    catch
        _:_ ->
            http_response(503, "Service Unavailable", "text/plain", <<"Service unavailable">>)
    end.

%% @private
ready_response() ->
    try
        State = gen_server:call(?MODULE, get_state, 1000),
        case State#state.ready of
            true ->
                http_response(200, "OK", "text/plain", <<"Ready">>);
            false ->
                http_response(503, "Service Unavailable", "text/plain", <<"Not ready">>)
        end
    catch
        _:_ ->
            http_response(503, "Service Unavailable", "text/plain", <<"Service unavailable">>)
    end.

%% @private
live_response() ->
    %% Always return 200 if we can handle the request
    http_response(200, "OK", "text/plain", <<"Live">>).

%% @private
metrics_response() ->
    try
        State = gen_server:call(?MODULE, get_state, 1000),
        Uptime = erlang:system_time(second) - State#state.started_at,

        %% Get diagnostics info
        DiagInfo = get_diagnostics_metrics(),

        %% Get gateway stats (if available)
        GatewayStats = get_gateway_stats(),

        %% Build Prometheus-formatted metrics
        Body = iolist_to_binary([
            %% Health server metrics
            "# HELP macula_gateway_uptime_seconds Gateway uptime in seconds\n",
            "# TYPE macula_gateway_uptime_seconds gauge\n",
            io_lib:format("macula_gateway_uptime_seconds ~p~n", [Uptime]),

            "# HELP macula_gateway_ready Gateway ready status (1=ready, 0=not ready)\n",
            "# TYPE macula_gateway_ready gauge\n",
            io_lib:format("macula_gateway_ready ~p~n", [case State#state.ready of true -> 1; false -> 0 end]),

            %% System metrics from diagnostics
            "# HELP macula_gateway_process_count Number of Erlang processes\n",
            "# TYPE macula_gateway_process_count gauge\n",
            io_lib:format("macula_gateway_process_count ~p~n", [maps:get(process_count, DiagInfo, 0)]),

            "# HELP macula_gateway_memory_bytes Total memory used by the VM in bytes\n",
            "# TYPE macula_gateway_memory_bytes gauge\n",
            io_lib:format("macula_gateway_memory_bytes ~p~n", [maps:get(memory_bytes, DiagInfo, 0)]),

            "# HELP macula_gateway_process_memory_bytes Memory used by Erlang processes in bytes\n",
            "# TYPE macula_gateway_process_memory_bytes gauge\n",
            io_lib:format("macula_gateway_process_memory_bytes ~p~n", [maps:get(process_memory_bytes, DiagInfo, 0)]),

            %% Diagnostics service availability
            "# HELP macula_gateway_diagnostics_available Diagnostics service availability (1=available, 0=unavailable)\n",
            "# TYPE macula_gateway_diagnostics_available gauge\n",
            io_lib:format("macula_gateway_diagnostics_available ~p~n", [maps:get(diagnostics_available, DiagInfo, 0)]),

            %% Gateway connection metrics
            "# HELP macula_gateway_clients_total Number of connected clients\n",
            "# TYPE macula_gateway_clients_total gauge\n",
            io_lib:format("macula_gateway_clients_total ~p~n", [maps:get(clients, GatewayStats, 0)]),

            "# HELP macula_gateway_subscriptions_total Number of active subscriptions\n",
            "# TYPE macula_gateway_subscriptions_total gauge\n",
            io_lib:format("macula_gateway_subscriptions_total ~p~n", [maps:get(subscriptions, GatewayStats, 0)]),

            "# HELP macula_gateway_registrations_total Number of registered procedures\n",
            "# TYPE macula_gateway_registrations_total gauge\n",
            io_lib:format("macula_gateway_registrations_total ~p~n", [maps:get(registrations, GatewayStats, 0)])
        ]),

        http_response(200, "OK", "text/plain; version=0.0.4", Body)
    catch
        _:_ ->
            http_response(503, "Service Unavailable", "text/plain", <<"Service unavailable">>)
    end.

%% @private
%% @doc Get diagnostics metrics from the diagnostics service
get_diagnostics_metrics() ->
    try
        %% Try to get info from diagnostics service
        case whereis(macula_gateway_diagnostics) of
            undefined ->
                #{diagnostics_available => 0};
            _Pid ->
                %% Get system info directly
                MemoryInfo = erlang:memory(),
                #{
                    diagnostics_available => 1,
                    process_count => erlang:system_info(process_count),
                    memory_bytes => proplists:get_value(total, MemoryInfo, 0),
                    process_memory_bytes => proplists:get_value(processes, MemoryInfo, 0)
                }
        end
    catch
        _:_ ->
            #{diagnostics_available => 0}
    end.

%% @private
%% @doc Get gateway statistics
get_gateway_stats() ->
    try
        case whereis(macula_gateway) of
            undefined ->
                #{clients => 0, subscriptions => 0, registrations => 0};
            Pid ->
                case catch macula_gateway:get_stats(Pid) of
                    Stats when is_map(Stats) -> Stats;
                    _ -> #{clients => 0, subscriptions => 0, registrations => 0}
                end
        end
    catch
        _:_ ->
            #{clients => 0, subscriptions => 0, registrations => 0}
    end.

%% @private
not_found_response() ->
    http_response(404, "Not Found", "text/plain", <<"Not found">>).

%% @private
http_response(Status, StatusText, ContentType, Body) ->
    ContentLength = byte_size(Body),
    [
        <<"HTTP/1.1 ">>, integer_to_binary(Status), <<" ">>, list_to_binary(StatusText), <<"\r\n">>,
        <<"Content-Type: ">>, list_to_binary(ContentType), <<"\r\n">>,
        <<"Content-Length: ">>, integer_to_binary(ContentLength), <<"\r\n">>,
        <<"Connection: close\r\n">>,
        <<"\r\n">>,
        Body
    ].
