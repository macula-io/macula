-module(macula_gateway_pubsub_router_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Endpoint Parsing Tests
%%%===================================================================

%% Test parsing full HTTPS URL with port
parse_endpoint_https_with_port_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint(<<"https://192.168.1.100:4433">>),
    ?assertMatch({ok, {{192,168,1,100}, 4433}}, Result).

%% Test parsing HTTPS URL without port (should default to 4433)
parse_endpoint_https_without_port_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint(<<"https://192.168.1.100">>),
    ?assertMatch({ok, {{192,168,1,100}, 4433}}, Result).

%% Test parsing hostname with port
parse_endpoint_hostname_with_port_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint(<<"https://arcade-peer1:9443">>),
    ?assertMatch({ok, {"arcade-peer1", 9443}}, Result).

%% Test parsing hostname without port
parse_endpoint_hostname_without_port_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint(<<"https://arcade-peer1">>),
    ?assertMatch({ok, {"arcade-peer1", 4433}}, Result).

%% Test parsing localhost with port
parse_endpoint_localhost_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint(<<"https://localhost:4433">>),
    ?assertMatch({ok, {"localhost", 4433}}, Result).

%% Test parsing IPv6 address with port
parse_endpoint_ipv6_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint(<<"https://[::1]:4433">>),
    ?assertMatch({ok, {{0,0,0,0,0,0,0,1}, 4433}}, Result).

%% Test parsing invalid URL format
parse_endpoint_invalid_url_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint(<<"not-a-url">>),
    ?assertMatch({error, _}, Result).

%% Test parsing non-binary input
parse_endpoint_non_binary_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint("string-not-binary"),
    ?assertEqual({error, invalid_endpoint_type}, Result).

%% Test parsing undefined
parse_endpoint_undefined_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint(undefined),
    ?assertEqual({error, invalid_endpoint_type}, Result).

%% Test parsing empty binary
parse_endpoint_empty_binary_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint(<<>>),
    ?assertMatch({error, _}, Result).

%%%===================================================================
%%% Endpoint Host Parsing Tests
%%%===================================================================

%% Test parsing IPv4 address string
parse_endpoint_host_ipv4_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint_host("192.168.1.100", 4433),
    ?assertEqual({ok, {{192,168,1,100}, 4433}}, Result).

%% Test parsing IPv4 address binary
parse_endpoint_host_ipv4_binary_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint_host(<<"192.168.1.100">>, 4433),
    ?assertEqual({ok, {{192,168,1,100}, 4433}}, Result).

%% Test parsing hostname as string
parse_endpoint_host_hostname_string_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint_host("example.com", 9443),
    ?assertEqual({ok, {"example.com", 9443}}, Result).

%% Test parsing hostname as binary
parse_endpoint_host_hostname_binary_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint_host(<<"example.com">>, 9443),
    ?assertEqual({ok, {"example.com", 9443}}, Result).

%% Test parsing localhost
parse_endpoint_host_localhost_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint_host("localhost", 4433),
    ?assertEqual({ok, {"localhost", 4433}}, Result).

%% Test parsing IPv6 loopback
parse_endpoint_host_ipv6_loopback_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint_host("::1", 4433),
    ?assertEqual({ok, {{0,0,0,0,0,0,0,1}, 4433}}, Result).

%%%===================================================================
%%% Real-World Scenario Tests
%%%===================================================================

%% Test parsing arcade-gateway endpoint
parse_endpoint_arcade_gateway_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint(<<"https://arcade-gateway:4433">>),
    ?assertMatch({ok, {"arcade-gateway", 4433}}, Result).

%% Test parsing arcade-peer1 endpoint
parse_endpoint_arcade_peer1_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint(<<"https://arcade-peer1:4433">>),
    ?assertMatch({ok, {"arcade-peer1", 4433}}, Result).

%% Test parsing arcade-peer2 endpoint
parse_endpoint_arcade_peer2_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint(<<"https://arcade-peer2:4433">>),
    ?assertMatch({ok, {"arcade-peer2", 4433}}, Result).

%% Test parsing Docker internal IP endpoint
parse_endpoint_docker_ip_test() ->
    Result = macula_gateway_pubsub_router:parse_endpoint(<<"https://172.18.0.5:4433">>),
    ?assertMatch({ok, {{172,18,0,5}, 4433}}, Result).

%%%===================================================================
%%% Client Stream Delivery Tests (Documentation)
%%%===================================================================

%% Note: The dual-path delivery logic (client streams vs mesh connections)
%% is tested through integration tests with the full gateway stack.
%%
%% Expected behavior:
%% 1. When routing to a subscriber with node_id that IS in client_streams:
%%    - Should call macula_gateway_clients:get_client_stream/2 -> {ok, Stream}
%%    - Should send PUBLISH message directly to Stream
%%    - Should NOT create mesh connection
%%
%% 2. When routing to a subscriber with node_id that IS NOT in client_streams:
%%    - Should call macula_gateway_clients:get_client_stream/2 -> {error, not_found}
%%    - Should extract endpoint from DHT subscriber info
%%    - Should create mesh connection to endpoint
%%    - Should send pubsub_route message via mesh
%%
%% The logic is in route_to_single_subscriber/6 in macula_gateway_pubsub_router.erl
