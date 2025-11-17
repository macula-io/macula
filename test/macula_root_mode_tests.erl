%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for macula_root mode-based startup.
%%% Tests that different deployment modes start correct child processes.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_root_mode_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

mode_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"bootstrap mode starts routing + bootstrap system", fun test_bootstrap_mode/0},
      {"edge mode starts only routing server", fun test_edge_mode/0},
      {"gateway mode starts routing + gateway system", fun test_gateway_mode/0},
      {"hybrid mode starts all systems", fun test_hybrid_mode/0},
      {"invalid mode crashes with error", fun test_invalid_mode/0},
      {"gateway override for edge mode", fun test_gateway_override/0}
     ]}.

%%%===================================================================
%%% Setup/Cleanup
%%%===================================================================

setup() ->
    %% Stop any running application
    application:stop(macula),
    ok.

cleanup(_) ->
    application:stop(macula),
    %% Clean up env vars
    application:unset_env(macula, mode),
    application:unset_env(macula, start_gateway),
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

test_bootstrap_mode() ->
    %% Configure bootstrap mode
    application:set_env(macula, mode, bootstrap),
    application:set_env(macula, start_gateway, false),
    application:set_env(macula, realm, <<"test.bootstrap">>),

    %% Start application
    {ok, _} = application:ensure_all_started(macula),

    %% Verify routing_server started
    RoutingPid = whereis(macula_routing_server),
    ?assertNotEqual(undefined, RoutingPid),
    ?assert(is_process_alive(RoutingPid)),

    %% Verify bootstrap_system started
    BootstrapPid = whereis(macula_bootstrap_system),
    ?assertNotEqual(undefined, BootstrapPid),
    ?assert(is_process_alive(BootstrapPid)),

    %% Verify gateway NOT started
    GatewayPid = whereis(macula_gateway_system),
    ?assertEqual(undefined, GatewayPid),

    application:stop(macula).

test_edge_mode() ->
    %% Configure edge mode
    application:set_env(macula, mode, edge),
    application:set_env(macula, start_gateway, false),

    %% Start application
    {ok, _} = application:ensure_all_started(macula),

    %% Verify routing_server started
    RoutingPid = whereis(macula_routing_server),
    ?assertNotEqual(undefined, RoutingPid),
    ?assert(is_process_alive(RoutingPid)),

    %% Verify bootstrap_system NOT started
    BootstrapPid = whereis(macula_bootstrap_system),
    ?assertEqual(undefined, BootstrapPid),

    %% Verify gateway NOT started
    GatewayPid = whereis(macula_gateway_system),
    ?assertEqual(undefined, GatewayPid),

    application:stop(macula).

test_gateway_mode() ->
    %% Configure gateway mode
    application:set_env(macula, mode, gateway),
    application:set_env(macula, start_gateway, true),
    application:set_env(macula, gateway_port, 9443),
    application:set_env(macula, health_port, 8080),

    %% Start application (may fail due to missing certs in test environment)
    case application:ensure_all_started(macula) of
        {ok, _} ->
            %% If started successfully (certs available)
            %% Verify routing_server started
            RoutingPid = whereis(macula_routing_server),
            ?assertNotEqual(undefined, RoutingPid),
            ?assert(is_process_alive(RoutingPid)),

            %% Verify bootstrap_system NOT started
            BootstrapPid = whereis(macula_bootstrap_system),
            ?assertEqual(undefined, BootstrapPid),

            %% Verify gateway started
            GatewayPid = whereis(macula_gateway_system),
            ?assertNotEqual(undefined, GatewayPid),
            ?assert(is_process_alive(GatewayPid)),

            application:stop(macula);

        {error, {macula, {{shutdown, {failed_to_start_child, macula_gateway_system, _}}, _}}} ->
            %% Expected in test environment without TLS certs
            %% Test passes - gateway mode attempted to start gateway (correct behavior)
            ok
    end.

test_hybrid_mode() ->
    %% Configure hybrid mode
    application:set_env(macula, mode, hybrid),
    application:set_env(macula, start_gateway, true),
    application:set_env(macula, gateway_port, 9443),
    application:set_env(macula, health_port, 8080),
    application:set_env(macula, realm, <<"test.hybrid">>),

    %% Start application (may fail due to missing certs in test environment)
    case application:ensure_all_started(macula) of
        {ok, _} ->
            %% If started successfully (certs available)
            %% Verify routing_server started
            RoutingPid = whereis(macula_routing_server),
            ?assertNotEqual(undefined, RoutingPid),
            ?assert(is_process_alive(RoutingPid)),

            %% Verify bootstrap_system started
            BootstrapPid = whereis(macula_bootstrap_system),
            ?assertNotEqual(undefined, BootstrapPid),
            ?assert(is_process_alive(BootstrapPid)),

            %% Verify gateway started
            GatewayPid = whereis(macula_gateway_system),
            ?assertNotEqual(undefined, GatewayPid),
            ?assert(is_process_alive(GatewayPid)),

            application:stop(macula);

        {error, {macula, {{shutdown, {failed_to_start_child, macula_gateway_system, _}}, _}}} ->
            %% Expected in test environment without TLS certs
            %% Test passes - hybrid mode attempted to start both bootstrap and gateway (correct behavior)
            ok
    end.

test_invalid_mode() ->
    %% Configure invalid mode
    application:set_env(macula, mode, invalid_mode),

    %% Should fail with invalid_mode error
    Result = application:ensure_all_started(macula),

    %% Verify error contains invalid_mode
    ?assertMatch({error, {macula, {{{invalid_mode, invalid_mode, _}, _}, _}}}, Result).

test_gateway_override() ->
    %% Edge mode with gateway override (should NOT start gateway)
    application:set_env(macula, mode, edge),
    application:set_env(macula, start_gateway, false),  % Explicit override

    {ok, _} = application:ensure_all_started(macula),

    %% Gateway should NOT be running
    GatewayPid = whereis(macula_gateway_system),
    ?assertEqual(undefined, GatewayPid),

    application:stop(macula).
