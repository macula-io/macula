-module(macula_gateway_endpoint_tests).
-include_lib("eunit/include/eunit.hrl").

%%% Tests for macula_gateway:parse_endpoint/1
%%%
%%% This test suite verifies that parse_endpoint/1 handles DNS resolution failures
%%% gracefully without crashing the gateway process.

%% Test that parse_endpoint/1 handles valid endpoints correctly
parse_endpoint_valid_with_port_test() ->
    %% Valid endpoint with port should resolve to IP tuple
    Result = macula_gateway:parse_endpoint(<<"https://localhost:9443">>),
    ?assertMatch({{_, _, _, _}, 9443}, Result).

parse_endpoint_valid_without_port_test() ->
    %% Valid endpoint without port should use default 9443
    Result = macula_gateway:parse_endpoint(<<"https://localhost">>),
    ?assertMatch({{_, _, _, _}, 9443}, Result).

parse_endpoint_undefined_test() ->
    %% undefined should return placeholder
    Result = macula_gateway:parse_endpoint(undefined),
    ?assertEqual({{0,0,0,0}, 0}, Result).

%% Test that parse_endpoint/1 handles DNS resolution failures
%% This is the critical bug fix - it should NOT crash when DNS fails
parse_endpoint_dns_failure_test() ->
    %% Use a hostname that definitely won't resolve
    InvalidHost = <<"https://this-hostname-definitely-does-not-exist-anywhere-12345.invalid:9443">>,

    %% This should NOT crash - it should return a fallback address
    Result = macula_gateway:parse_endpoint(InvalidHost),

    %% Should return localhost fallback instead of crashing
    ?assertMatch({{127, 0, 0, 1}, 9443}, Result).

parse_endpoint_dns_failure_no_port_test() ->
    %% Test DNS failure with no port specified (should use default 9443)
    InvalidHost = <<"https://this-hostname-definitely-does-not-exist-anywhere-67890.invalid">>,

    %% Should NOT crash - should return fallback with default port
    Result = macula_gateway:parse_endpoint(InvalidHost),

    ?assertMatch({{127, 0, 0, 1}, 9443}, Result).

%% Test that parse_endpoint/1 handles invalid URL formats
parse_endpoint_invalid_format_test() ->
    %% Invalid URL format should return placeholder instead of crashing
    Result = macula_gateway:parse_endpoint(<<"not-a-valid-url">>),

    %% Should return placeholder address
    ?assertEqual({{0,0,0,0}, 0}, Result).

parse_endpoint_empty_binary_test() ->
    %% Empty binary should be handled gracefully
    Result = macula_gateway:parse_endpoint(<<"">>),

    %% Should return placeholder address
    ?assertEqual({{0,0,0,0}, 0}, Result).

%% Test that the fix doesn't break normal operation
parse_endpoint_real_host_test() ->
    %% Test with a real host that should resolve (localhost should always work)
    Result = macula_gateway:parse_endpoint(<<"https://127.0.0.1:9443">>),

    %% Should resolve to the IP address
    ?assertEqual({{127, 0, 0, 1}, 9443}, Result).

%%% Tests for extracted resolve_host/2 function
%%% This function eliminates duplicate DNS resolution logic

%% Test resolve_host/2 with valid hostname
resolve_host_valid_test() ->
    %% Verify function is exported for testing
    Exports = macula_gateway:module_info(exports),
    ?assert(lists:member({resolve_host, 2}, Exports)).

%% Test resolve_host/2 with localhost (should always work)
resolve_host_localhost_test() ->
    Result = macula_gateway:resolve_host(<<"localhost">>, 9443),
    %% Should resolve to 127.0.0.1
    ?assertMatch({{127, 0, 0, 1}, 9443}, Result).

%% Test resolve_host/2 with IP address (should work without DNS)
resolve_host_ip_address_test() ->
    Result = macula_gateway:resolve_host(<<"192.168.1.1">>, 8080),
    %% Should parse IP directly
    ?assertEqual({{192, 168, 1, 1}, 8080}, Result).

%% Test resolve_host/2 with invalid hostname (should return localhost fallback)
resolve_host_invalid_hostname_test() ->
    Result = macula_gateway:resolve_host(<<"this-definitely-does-not-exist-12345.invalid">>, 9443),
    %% Should fallback to localhost
    ?assertEqual({{127, 0, 0, 1}, 9443}, Result).
