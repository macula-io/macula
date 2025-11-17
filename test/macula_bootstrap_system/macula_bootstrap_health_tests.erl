%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for macula_bootstrap_health.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bootstrap_health_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

bootstrap_health_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Fixture) ->
         [
          {"health check returns status", fun() -> test_health_check(Fixture) end},
          {"is_healthy when all services running", fun() -> test_is_healthy(Fixture) end},
          {"health check detects missing server", fun() -> test_missing_server(Fixture) end},
          {"health check detects missing registry", fun() -> test_missing_registry(Fixture) end}
         ]
     end}.

%%%===================================================================
%%% Setup/Cleanup
%%%===================================================================

setup() ->
    %% Start all dependencies
    LocalNodeId = crypto:hash(sha256, <<"test_health_node">>),
    RoutingConfig = #{k => 20, alpha => 3},
    {ok, RoutingPid} = macula_routing_server:start_link(LocalNodeId, RoutingConfig),

    {ok, RegistryPid} = macula_bootstrap_registry:start_link(#{}),

    ServerConfig = #{realm => <<"test.realm">>},
    {ok, ServerPid} = macula_bootstrap_server:start_link(ServerConfig),

    %% Start health monitor
    HealthConfig = #{health_check_interval => 60000},  % Don't auto-check during tests
    {ok, HealthPid} = macula_bootstrap_health:start_link(HealthConfig),

    #{
        routing_pid => RoutingPid,
        registry_pid => RegistryPid,
        server_pid => ServerPid,
        health_pid => HealthPid
    }.

cleanup(#{routing_pid := RoutingPid, registry_pid := RegistryPid,
          server_pid := ServerPid, health_pid := HealthPid}) ->
    catch gen_server:stop(HealthPid),
    catch gen_server:stop(ServerPid),
    catch gen_server:stop(RegistryPid),
    catch gen_server:stop(RoutingPid),
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

test_health_check(_Fixture) ->
    %% Trigger a manual health check (send message directly)
    whereis(macula_bootstrap_health) ! health_check,
    timer:sleep(100),

    %% Get health status
    {ok, Health} = macula_bootstrap_health:get_health(),

    %% Should have expected fields
    ?assertMatch(#{status := _, message := _, checks := _, timestamp := _}, Health),

    %% Status should be healthy
    ?assertEqual(healthy, maps:get(status, Health)).

test_is_healthy(_Fixture) ->
    %% Trigger health check
    whereis(macula_bootstrap_health) ! health_check,
    timer:sleep(100),

    %% Check is_healthy
    ?assert(macula_bootstrap_health:is_healthy()).

test_missing_server(_Fixture) ->
    %% Stop bootstrap server
    catch gen_server:stop(whereis(macula_bootstrap_server)),
    timer:sleep(100),

    %% Trigger health check
    whereis(macula_bootstrap_health) ! health_check,
    timer:sleep(100),

    %% Should be unhealthy
    ?assertNot(macula_bootstrap_health:is_healthy()),

    %% Get health details
    {ok, Health} = macula_bootstrap_health:get_health(),
    ?assertEqual(unhealthy, maps:get(status, Health)).

test_missing_registry(_Fixture) ->
    %% Stop bootstrap registry
    catch gen_server:stop(whereis(macula_bootstrap_registry)),
    timer:sleep(100),

    %% Trigger health check
    whereis(macula_bootstrap_health) ! health_check,
    timer:sleep(100),

    %% Should be unhealthy
    ?assertNot(macula_bootstrap_health:is_healthy()),

    %% Get health details
    {ok, Health} = macula_bootstrap_health:get_health(),
    ?assertEqual(unhealthy, maps:get(status, Health)).
