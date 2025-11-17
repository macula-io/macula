%%%-------------------------------------------------------------------
%%% @doc
%%% Helper functions for integration tests.
%%% Provides utilities for Docker interaction, RPC testing, and assertions.
%%% @end
%%%-------------------------------------------------------------------
-module(test_helpers).

%% Docker helpers
-export([
    docker_exec/2,
    docker_exec_erl/2,
    wait_for_healthy_services/0,
    wait_for_healthy_services/1
]).

%% RPC test helpers
-export([
    register_test_service/3,
    discover_service/2,
    call_rpc_service/4,
    wait_for_dht_propagation/0,
    wait_for_dht_propagation/1
]).

%% HTTP helpers
-export([
    http_get/1,
    parse_health_json/1
]).

%% Assertion helpers
-export([
    assert_service_registered/3,
    assert_service_discovered/2,
    assert_rpc_success/2,
    assert_rpc_error/2
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Docker Helpers
%%%===================================================================

%% @doc Execute command in Docker container
-spec docker_exec(Container :: string(), Command :: string()) -> {ok, string()} | {error, term()}.
docker_exec(Container, Command) ->
    FullCmd = io_lib:format("docker exec ~s ~s 2>&1", [Container, Command]),
    Output = os:cmd(FullCmd),
    case string:str(Output, "Error") of
        0 -> {ok, Output};
        _ -> {error, Output}
    end.

%% @doc Execute Erlang expression in Docker container
-spec docker_exec_erl(Container :: string(), ErlExpr :: string()) -> {ok, string()} | {error, term()}.
docker_exec_erl(Container, ErlExpr) ->
    %% Escape single quotes in expression
    EscapedExpr = re:replace(ErlExpr, "'", "\\\\'", [global, {return, list}]),
    Cmd = io_lib:format("docker exec ~s bin/macula eval '~s'", [Container, EscapedExpr]),
    Output = os:cmd(Cmd),
    {ok, Output}.

%% @doc Wait for all Docker services to be healthy
-spec wait_for_healthy_services() -> ok | {error, timeout}.
wait_for_healthy_services() ->
    wait_for_healthy_services(60).

-spec wait_for_healthy_services(Timeout :: pos_integer()) -> ok | {error, timeout}.
wait_for_healthy_services(Timeout) ->
    ct:pal("Waiting for Docker services to be healthy (timeout: ~ps)...", [Timeout]),
    wait_for_healthy_loop(Timeout).

wait_for_healthy_loop(0) ->
    ct:pal("ERROR: Timeout waiting for services to be healthy"),
    {error, timeout};
wait_for_healthy_loop(Retries) ->
    Cmd = "docker ps --filter \"name=macula-\" --format \"{{.Names}}: {{.Status}}\"",
    Output = os:cmd(Cmd),

    case string:str(Output, "(unhealthy)") of
        0 ->
            %% All healthy
            ct:pal("All services healthy!"),
            timer:sleep(2000),  % Stabilization time
            ok;
        _ ->
            %% Some unhealthy, wait and retry
            timer:sleep(1000),
            wait_for_healthy_loop(Retries - 1)
    end.

%%%===================================================================
%%% RPC Test Helpers
%%%===================================================================

%% @doc Register RPC service in DHT from a container
-spec register_test_service(Container :: string(), ServiceName :: binary(), Endpoint :: binary()) ->
    ok | {error, term()}.
register_test_service(Container, ServiceName, Endpoint) ->
    ErlCode = io_lib:format("
        case whereis(macula_routing_server) of
            undefined ->
                io:format('ERROR: Routing server not running~n'),
                {error, no_routing_server};
            Pid ->
                ServiceKey = ~p,
                NodeId = crypto:strong_rand_bytes(32),
                ServiceValue = #{node_id => NodeId, endpoint => ~p},
                ok = macula_routing_server:store_local(Pid, ServiceKey, ServiceValue),
                io:format('Service registered: ~~p~n', [ServiceKey]),
                ok
        end.
    ", [ServiceName, Endpoint]),

    case docker_exec_erl(Container, ErlCode) of
        {ok, Output} ->
            case string:str(Output, "Service registered") of
                0 -> {error, Output};
                _ -> ok
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Discover service in DHT from a container
-spec discover_service(Container :: string(), ServiceName :: binary()) ->
    {ok, found} | {error, not_found} | {error, term()}.
discover_service(Container, ServiceName) ->
    ErlCode = io_lib:format("
        case whereis(macula_routing_server) of
            undefined ->
                {error, no_routing_server};
            Pid ->
                ServiceKey = ~p,
                case macula_routing_server:get_local(Pid, ServiceKey) of
                    {ok, Value} ->
                        io:format('Service found: ~~p~~n', [Value]),
                        {ok, found};
                    not_found ->
                        io:format('Service not found~~n'),
                        {error, not_found}
                end
        end.
    ", [ServiceName]),

    case docker_exec_erl(Container, ErlCode) of
        {ok, Output} ->
            case string:str(Output, "Service found") of
                0 -> {error, not_found};
                _ -> {ok, found}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Call RPC service via macula_peer API
-spec call_rpc_service(Container :: string(), ServiceName :: binary(), Args :: map(), Timeout :: pos_integer()) ->
    {ok, term()} | {error, term()}.
call_rpc_service(Container, ServiceName, Args, Timeout) ->
    %% Convert args map to string representation
    ArgsStr = io_lib:format("~p", [Args]),

    ErlCode = io_lib:format("
        Service = ~p,
        Args = ~s,
        Timeout = ~p,
        %% TODO: Implement actual RPC call via macula_peer
        io:format('Calling RPC service ~~p with args ~~p~n', [Service, Args]),
        {ok, #{result => test_result}}
    ", [ServiceName, ArgsStr, Timeout]),

    docker_exec_erl(Container, ErlCode).

%% @doc Wait for DHT propagation
-spec wait_for_dht_propagation() -> ok.
wait_for_dht_propagation() ->
    wait_for_dht_propagation(2000).

-spec wait_for_dht_propagation(Millis :: pos_integer()) -> ok.
wait_for_dht_propagation(Millis) ->
    ct:pal("Waiting ~pms for DHT propagation...", [Millis]),
    timer:sleep(Millis),
    ok.

%%%===================================================================
%%% HTTP Helpers
%%%===================================================================

%% @doc HTTP GET request
-spec http_get(Url :: string()) -> {ok, string()} | {error, term()}.
http_get(Url) ->
    Cmd = io_lib:format("curl -s ~s", [Url]),
    Output = os:cmd(Cmd),
    {ok, Output}.

%% @doc Parse health check JSON response
-spec parse_health_json(JsonStr :: string()) -> map().
parse_health_json(JsonStr) ->
    %% Simple JSON parsing for health checks
    %% In production, use jsx or jiffy
    case string:str(JsonStr, "\"status\":\"healthy\"") of
        0 -> #{<<"status">> => <<"unhealthy">>};
        _ -> #{<<"status">> => <<"healthy">>}
    end.

%%%===================================================================
%%% Assertion Helpers
%%%===================================================================

%% @doc Assert that service was registered successfully
-spec assert_service_registered(Container :: string(), ServiceName :: binary(), Endpoint :: binary()) -> ok.
assert_service_registered(Container, ServiceName, Endpoint) ->
    case register_test_service(Container, ServiceName, Endpoint) of
        ok ->
            ct:pal("✓ Service ~s registered at ~s", [ServiceName, Endpoint]),
            ok;
        {error, Reason} ->
            ct:fail("Failed to register service ~s: ~p", [ServiceName, Reason])
    end.

%% @doc Assert that service can be discovered
-spec assert_service_discovered(Container :: string(), ServiceName :: binary()) -> ok.
assert_service_discovered(Container, ServiceName) ->
    case discover_service(Container, ServiceName) of
        {ok, found} ->
            ct:pal("✓ Service ~s discovered", [ServiceName]),
            ok;
        {error, not_found} ->
            ct:fail("Service ~s not found in DHT", [ServiceName]);
        {error, Reason} ->
            ct:fail("Failed to discover service ~s: ~p", [ServiceName, Reason])
    end.

%% @doc Assert that RPC call succeeds
-spec assert_rpc_success(Result :: term(), Expected :: term()) -> ok.
assert_rpc_success(Result, Expected) ->
    case Result of
        {ok, Expected} ->
            ct:pal("✓ RPC call succeeded with expected result"),
            ok;
        {ok, Other} ->
            ct:fail("RPC call returned unexpected result: ~p (expected ~p)", [Other, Expected]);
        {error, Reason} ->
            ct:fail("RPC call failed: ~p", [Reason])
    end.

%% @doc Assert that RPC call fails with expected error
-spec assert_rpc_error(Result :: term(), ExpectedError :: atom()) -> ok.
assert_rpc_error(Result, ExpectedError) ->
    case Result of
        {error, ExpectedError} ->
            ct:pal("✓ RPC call failed with expected error: ~p", [ExpectedError]),
            ok;
        {error, OtherError} ->
            ct:fail("RPC call failed with unexpected error: ~p (expected ~p)", [OtherError, ExpectedError]);
        {ok, Value} ->
            ct:fail("RPC call unexpectedly succeeded: ~p", [Value])
    end.
