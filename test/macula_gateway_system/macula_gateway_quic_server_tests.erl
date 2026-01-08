-module(macula_gateway_quic_server_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Infrastructure Check
%%%===================================================================

%% These tests require QUIC infrastructure (TLS certificates, listening ports).
%% They are skipped when running in unit test mode and should be run in
%% Docker integration tests where QUIC infrastructure is available.

quic_server_test_() ->
    case is_quic_available() of
        true ->
            %% Run tests if QUIC infrastructure is available
            [
             {"start_stop", fun test_start_stop/0},
             {"init_with_default_port", fun test_init_with_default_port/0},
             {"init_with_node_id", fun test_init_with_node_id/0}
            ];
        false ->
            %% Return empty test list when QUIC infrastructure is not available
            %% Tests requiring QUIC should be run in Docker integration environment
            []
    end.

is_quic_available() ->
    %% Check if TLS certificates exist
    CertFile = os:getenv("MACULA_TLS_CERTFILE", "/opt/macula/certs/cert.pem"),
    KeyFile = os:getenv("MACULA_TLS_KEYFILE", "/opt/macula/certs/key.pem"),
    filelib:is_file(CertFile) andalso filelib:is_file(KeyFile).

%%%===================================================================
%%% Basic Gen_Server Tests (Step 1)
%%% Note: Functions named without _test suffix to prevent EUnit auto-discovery
%%%===================================================================

test_start_stop() ->
    Opts = [{port, 9999}, {realm, <<"test">>}],
    {ok, Pid} = macula_gateway_quic_server:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    ok = gen_server:stop(Pid).

test_init_with_default_port() ->
    Opts = [{realm, <<"test.default">>}],
    {ok, Pid} = macula_gateway_quic_server:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    ok = gen_server:stop(Pid).

test_init_with_node_id() ->
    Opts = [{port, 8888}, {realm, <<"test.realm">>}],
    {ok, Pid} = macula_gateway_quic_server:start_link(Opts),

    %% Verify process state contains node_id
    State = sys:get_state(Pid),
    NodeId = element(4, State),  % node_id is 4th field in #state{} (after listener, gateway, node_id)
    ?assert(is_binary(NodeId)),

    ok = gen_server:stop(Pid).

%%%===================================================================
%%% Helper Function Tests (Step 2)
%%%===================================================================

parse_endpoint_test() ->
    %% Test undefined endpoint
    ?assertEqual({{0,0,0,0}, 0},
                 macula_gateway_quic_server:parse_endpoint(undefined)),

    %% Test valid endpoint with port
    ?assertEqual({{127,0,0,1}, 9443},
                 macula_gateway_quic_server:parse_endpoint(<<"https://localhost:9443">>)),

    %% Test valid endpoint without port (should default to 9443)
    Result = macula_gateway_quic_server:parse_endpoint(<<"https://localhost">>),
    ?assertMatch({{127,0,0,1}, 9443}, Result).

resolve_host_test() ->
    %% Test localhost resolution
    {IP, Port} = macula_gateway_quic_server:resolve_host(<<"localhost">>, 8080),
    ?assertEqual(8080, Port),
    ?assert(is_tuple(IP) andalso tuple_size(IP) == 4),

    %% Test with numeric IP
    {IP2, Port2} = macula_gateway_quic_server:resolve_host(<<"127.0.0.1">>, 9000),
    ?assertEqual({127,0,0,1}, IP2),
    ?assertEqual(9000, Port2).

%% Test that get_node_id/2 handles both binary and charlist realm
get_node_id_charlist_realm_test() ->
    %% Clear any existing env vars to ensure consistent test
    os:unsetenv("NODE_NAME"),
    os:unsetenv("HOSTNAME"),

    %% Binary realm
    BinaryResult = macula_gateway_quic_server:get_node_id(<<"io.macula">>, 9443),
    ?assert(is_binary(BinaryResult)),
    ?assertEqual(32, byte_size(BinaryResult)),

    %% Charlist realm should produce identical result
    CharlistResult = macula_gateway_quic_server:get_node_id("io.macula", 9443),
    ?assertEqual(BinaryResult, CharlistResult).

%% Test get_node_id/2 with HOSTNAME env var and charlist realm
get_node_id_with_hostname_test() ->
    %% Set HOSTNAME, clear NODE_NAME
    os:unsetenv("NODE_NAME"),
    os:putenv("HOSTNAME", "test-container-123"),

    %% Binary realm
    BinaryResult = macula_gateway_quic_server:get_node_id(<<"io.macula">>, 9443),
    ?assert(is_binary(BinaryResult)),

    %% Charlist realm should produce identical result
    CharlistResult = macula_gateway_quic_server:get_node_id("io.macula", 9443),
    ?assertEqual(BinaryResult, CharlistResult),

    %% Cleanup
    os:unsetenv("HOSTNAME").
