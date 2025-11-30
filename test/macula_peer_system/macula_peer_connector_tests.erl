%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_peer_connector module.
%%%
%%% Tests peer-to-peer connection utilities including endpoint parsing.
%%% Note: Tests that require actual QUIC connections are integration tests
%%% and are not included here to avoid timeouts.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_peer_connector_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Valid ping message that passes validation
valid_ping_msg() ->
    #{timestamp => erlang:system_time(millisecond)}.

%%%===================================================================
%%% Endpoint Parsing Tests - Invalid Endpoints
%%%
%%% These test the internal parse_endpoint function indirectly through
%%% the public API behavior. Invalid endpoints fail before attempting
%%% any network connection, so these tests complete quickly.
%%%===================================================================

send_message_invalid_endpoint_empty_test() ->
    %% GIVEN: An empty endpoint
    Endpoint = <<>>,

    %% WHEN: Trying to send a message
    Result = macula_peer_connector:send_message(Endpoint, ping, valid_ping_msg()),

    %% THEN: Should fail with invalid_endpoint error
    ?assertMatch({error, {invalid_endpoint, _}}, Result).

send_message_invalid_endpoint_no_port_test() ->
    %% GIVEN: Endpoint without port
    Endpoint = <<"192.168.1.100">>,

    %% WHEN: Trying to send a message
    Result = macula_peer_connector:send_message(Endpoint, ping, valid_ping_msg()),

    %% THEN: Should fail with invalid_endpoint error
    ?assertMatch({error, {invalid_endpoint, _}}, Result).

send_message_invalid_endpoint_bad_port_test() ->
    %% GIVEN: Endpoint with non-numeric port
    Endpoint = <<"192.168.1.100:abc">>,

    %% WHEN: Trying to send a message
    Result = macula_peer_connector:send_message(Endpoint, ping, valid_ping_msg()),

    %% THEN: Should fail with invalid_endpoint error
    ?assertMatch({error, {invalid_endpoint, invalid_port}}, Result).

send_message_invalid_endpoint_port_too_high_test() ->
    %% GIVEN: Endpoint with port > 65535
    Endpoint = <<"192.168.1.100:99999">>,

    %% WHEN: Trying to send a message
    Result = macula_peer_connector:send_message(Endpoint, ping, valid_ping_msg()),

    %% THEN: Should fail with invalid_endpoint error
    ?assertMatch({error, {invalid_endpoint, invalid_port}}, Result).

send_message_invalid_endpoint_port_zero_test() ->
    %% GIVEN: Endpoint with port = 0
    Endpoint = <<"192.168.1.100:0">>,

    %% WHEN: Trying to send a message
    Result = macula_peer_connector:send_message(Endpoint, ping, valid_ping_msg()),

    %% THEN: Should fail with invalid_endpoint error
    ?assertMatch({error, {invalid_endpoint, invalid_port}}, Result).

send_message_invalid_endpoint_negative_port_test() ->
    %% GIVEN: Endpoint with negative port
    Endpoint = <<"192.168.1.100:-1">>,

    %% WHEN: Trying to send a message
    Result = macula_peer_connector:send_message(Endpoint, ping, valid_ping_msg()),

    %% THEN: Should fail with invalid_endpoint error
    ?assertMatch({error, {invalid_endpoint, invalid_port}}, Result).

%%%===================================================================
%%% send_message_and_wait Endpoint Parsing Tests
%%%===================================================================

send_message_and_wait_invalid_endpoint_test() ->
    %% GIVEN: Invalid endpoint
    Endpoint = <<"invalid">>,

    %% WHEN: Trying to send and wait
    Result = macula_peer_connector:send_message_and_wait(Endpoint, ping, valid_ping_msg(), 1000),

    %% THEN: Should fail with invalid_endpoint error
    ?assertMatch({error, {invalid_endpoint, _}}, Result).

send_message_and_wait_empty_endpoint_test() ->
    %% GIVEN: Empty endpoint
    Endpoint = <<>>,

    %% WHEN: Trying to send and wait
    Result = macula_peer_connector:send_message_and_wait(Endpoint, ping, valid_ping_msg(), 1000),

    %% THEN: Should fail with invalid_endpoint error
    ?assertMatch({error, {invalid_endpoint, _}}, Result).

send_message_and_wait_bad_port_test() ->
    %% GIVEN: Endpoint with bad port
    Endpoint = <<"localhost:abc">>,

    %% WHEN: Trying to send and wait
    Result = macula_peer_connector:send_message_and_wait(Endpoint, ping, valid_ping_msg(), 1000),

    %% THEN: Should fail with invalid_endpoint error
    ?assertMatch({error, {invalid_endpoint, invalid_port}}, Result).

%%%===================================================================
%%% Endpoint Format Pattern Tests
%%%
%%% Test various endpoint format patterns are correctly parsed.
%%% These use known-bad formats to avoid connection attempts.
%%%===================================================================

endpoint_with_only_colon_test() ->
    %% GIVEN: Endpoint with only colon (no host)
    Endpoint = <<":9443">>,

    %% WHEN: Trying to send
    Result = macula_peer_connector:send_message(Endpoint, ping, valid_ping_msg()),

    %% THEN: Should fail (empty host is technically valid for split, but connect fails)
    %% The behavior depends on how the QUIC stack handles empty host
    ?assertMatch({error, _}, Result).

endpoint_with_multiple_colons_test() ->
    %% GIVEN: Endpoint with multiple colons (could be IPv6 or malformed)
    Endpoint = <<"host:port:extra">>,

    %% WHEN: Trying to send
    Result = macula_peer_connector:send_message(Endpoint, ping, valid_ping_msg()),

    %% THEN: Should fail with invalid_port (trailing split gets "extra")
    ?assertMatch({error, {invalid_endpoint, invalid_port}}, Result).

%%%===================================================================
%%% Protocol Encoder Integration Tests
%%%
%%% Verify that invalid message payloads are caught.
%%%===================================================================

send_message_invalid_payload_ping_missing_timestamp_test() ->
    %% GIVEN: Ping message without required timestamp
    Endpoint = <<"192.168.1.100:9443">>,
    InvalidMsg = #{},

    %% WHEN: Trying to send
    Result = (catch macula_peer_connector:send_message(Endpoint, ping, InvalidMsg)),

    %% THEN: Should fail during encoding validation (badmatch error)
    %% The exact error depends on how validation fails
    case Result of
        {'EXIT', _} -> ok;  % Crashed during validation - expected
        {error, _} -> ok    % Returned error - also acceptable
    end.

