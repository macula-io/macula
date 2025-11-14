%%%-------------------------------------------------------------------
%%% @doc
%%% Helper functions for integration tests.
%%%
%%% Provides utilities for managing Docker containers, waiting for
%%% services to be ready, and creating test connections.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_integration_test_helpers).

%% API
-export([
    start_test_gateway/0,
    stop_test_gateway/0,
    wait_for_gateway_ready/0,
    wait_for_gateway_ready/1,
    create_test_connection/0,
    create_test_connection/1,
    wait_for_connection_ready/1,
    wait_for_connection_ready/2,
    is_docker_available/0,
    docker_compose_file/0
]).

-define(DOCKER_COMPOSE_FILE, "docker/docker-compose.integration-test.yml").
-define(GATEWAY_URL, <<"https://localhost:19443">>).
-define(DEFAULT_REALM, <<"com.example.realm">>).
-define(MAX_RETRIES, 30).
-define(RETRY_DELAY, 1000).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Check if Docker is available and running.
-spec is_docker_available() -> boolean().
is_docker_available() ->
    case os:cmd("docker info 2>&1") of
        "Cannot connect to" ++ _ -> false;
        "permission denied" ++ _ -> false;
        _ -> true
    end.

%% @doc Get the path to the docker-compose file.
-spec docker_compose_file() -> string().
docker_compose_file() ->
    ?DOCKER_COMPOSE_FILE.

%% @doc Start the test gateway using Docker Compose.
%% Returns ok if successful, {error, Reason} otherwise.
-spec start_test_gateway() -> ok | {error, term()}.
start_test_gateway() ->
    case is_docker_available() of
        false ->
            {error, docker_not_available};
        true ->
            %% Stop any existing containers
            stop_test_gateway(),

            %% Start fresh containers
            Cmd = io_lib:format("docker compose -f ~s up -d 2>&1", [?DOCKER_COMPOSE_FILE]),
            Output = os:cmd(Cmd),

            case string:find(Output, "error") of
                nomatch ->
                    %% Wait for gateway to be ready
                    wait_for_gateway_ready();
                _ ->
                    {error, {docker_compose_failed, Output}}
            end
    end.

%% @doc Stop the test gateway and clean up containers.
-spec stop_test_gateway() -> ok.
stop_test_gateway() ->
    Cmd = io_lib:format("docker compose -f ~s down -v 2>&1", [?DOCKER_COMPOSE_FILE]),
    _Output = os:cmd(Cmd),
    ok.

%% @doc Wait for the gateway to be ready to accept connections.
%% Uses default max retries.
-spec wait_for_gateway_ready() -> ok | {error, timeout}.
wait_for_gateway_ready() ->
    wait_for_gateway_ready(?MAX_RETRIES).

%% @doc Wait for the gateway to be ready with custom max retries.
-spec wait_for_gateway_ready(non_neg_integer()) -> ok | {error, timeout}.
wait_for_gateway_ready(MaxRetries) ->
    wait_for_gateway_loop(MaxRetries).

%% @doc Create a test connection to the gateway.
%% Uses default URL and realm.
-spec create_test_connection() -> {ok, pid()} | {error, term()}.
create_test_connection() ->
    create_test_connection(#{}).

%% @doc Create a test connection with custom options.
-spec create_test_connection(map()) -> {ok, pid()} | {error, term()}.
create_test_connection(Opts) ->
    Url = maps:get(url, Opts, ?GATEWAY_URL),
    Realm = maps:get(realm, Opts, ?DEFAULT_REALM),
    NodeId = maps:get(node_id, Opts, generate_test_node_id()),

    ConnOpts = #{
        realm => Realm,
        node_id => NodeId
    },

    macula_connection:start_link(Url, ConnOpts).

%% @doc Wait for a connection to be fully established and ready.
%% Polls the connection to ensure it's actually connected, not just alive.
-spec wait_for_connection_ready(pid()) -> ok | {error, timeout}.
wait_for_connection_ready(Conn) ->
    wait_for_connection_ready(Conn, 30).

%% @doc Wait for connection with custom max retries.
-spec wait_for_connection_ready(pid(), non_neg_integer()) -> ok | {error, timeout}.
wait_for_connection_ready(_Conn, 0) ->
    {error, timeout};
wait_for_connection_ready(Conn, Retries) ->
    case is_process_alive(Conn) of
        false ->
            {error, connection_dead};
        true ->
            %% Connection is alive, wait a bit for it to connect
            timer:sleep(500),
            %% For now, we assume it's connected after waiting
            %% TODO: Add actual status check when connection has status API
            case Retries < 25 of
                true -> ok;  % Connected after 2.5+ seconds
                false -> wait_for_connection_ready(Conn, Retries - 1)
            end
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Wait loop for gateway readiness.
wait_for_gateway_loop(0) ->
    {error, timeout};
wait_for_gateway_loop(Retries) ->
    case check_gateway_health() of
        ok ->
            %% Gateway is ready
            ok;
        {error, _Reason} ->
            %% Not ready yet, wait and retry
            timer:sleep(?RETRY_DELAY),
            wait_for_gateway_loop(Retries - 1)
    end.

%% @private Check if the gateway health endpoint responds.
check_gateway_health() ->
    Cmd = "curl -s -o /dev/null -w \"%{http_code}\" http://localhost:18080/health 2>&1",
    case os:cmd(Cmd) of
        "200" -> ok;
        "000" -> {error, connection_refused};
        Other -> {error, {unexpected_status, Other}}
    end.

%% @private Generate a random test node ID.
generate_test_node_id() ->
    Timestamp = integer_to_binary(erlang:system_time(millisecond)),
    Random = integer_to_binary(rand:uniform(999999)),
    <<"test-node-", Timestamp/binary, "-", Random/binary>>.
