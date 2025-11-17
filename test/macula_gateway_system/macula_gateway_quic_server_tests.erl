-module(macula_gateway_quic_server_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Basic Gen_Server Tests (Step 1)
%%%===================================================================

start_stop_test() ->
    Opts = [{port, 9999}, {realm, <<"test">>}],
    {ok, Pid} = macula_gateway_quic_server:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    ok = gen_server:stop(Pid).

init_with_default_port_test() ->
    Opts = [{realm, <<"test.default">>}],
    {ok, Pid} = macula_gateway_quic_server:start_link(Opts),
    ?assert(is_process_alive(Pid)),
    ok = gen_server:stop(Pid).

init_with_node_id_test() ->
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
