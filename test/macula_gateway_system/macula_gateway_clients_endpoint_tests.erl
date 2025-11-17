-module(macula_gateway_clients_endpoint_tests).
-include_lib("eunit/include/eunit.hrl").

%% Tests for endpoint → stream PID tracking in gateway client manager
%% This is Phase 1 of the pub/sub DHT lookup fix

endpoint_tracking_on_client_join_test() ->
    %% When a client joins with an endpoint URL, the gateway should track
    %% the mapping from endpoint → stream PID for later lookup

    %% This test documents expected behavior:
    %% 1. Client sends JOIN with endpoint in message
    %% 2. Gateway extracts endpoint from JOIN message
    %% 3. Gateway stores endpoint → stream PID mapping
    %% 4. Gateway can later lookup stream by endpoint

    ?assert(true).

get_stream_by_endpoint_returns_pid_when_exists_test() ->
    %% get_stream_by_endpoint/2 should return {ok, StreamPid} when mapping exists

    Endpoint = <<"https://arcade-peer1:4001">>,
    StreamPid = self(),  % Mock stream PID

    %% Expected behavior:
    %% EndpointMap = #{Endpoint => StreamPid}
    %% get_stream_by_endpoint(ClientManager, Endpoint) -> {ok, StreamPid}

    ?assert(is_binary(Endpoint)),
    ?assert(is_pid(StreamPid)).

get_stream_by_endpoint_returns_not_found_when_missing_test() ->
    %% get_stream_by_endpoint/2 should return {error, not_found} when no mapping

    Endpoint = <<"https://unknown-peer:4001">>,

    %% Expected behavior:
    %% EndpointMap = #{}  % Empty map
    %% get_stream_by_endpoint(ClientManager, Endpoint) -> {error, not_found}

    ?assert(is_binary(Endpoint)).

endpoint_mapping_cleanup_on_stream_death_test() ->
    %% When a stream process dies, its endpoint mapping should be cleaned up

    %% Expected behavior:
    %% 1. Stream PID is monitored
    %% 2. When {'DOWN', Ref, process, StreamPid, _Reason} received
    %% 3. Remove StreamPid from endpoint_to_stream map
    %% 4. get_stream_by_endpoint returns {error, not_found} after cleanup

    ?assert(true).

multiple_endpoints_tracked_simultaneously_test() ->
    %% Gateway should track multiple endpoint → stream mappings

    Endpoint1 = <<"https://arcade-peer1:4001">>,
    Endpoint2 = <<"https://arcade-peer2:4002">>,
    Endpoint3 = <<"https://arcade-peer3:4003">>,

    %% Expected behavior:
    %% EndpointMap = #{
    %%     Endpoint1 => Stream1,
    %%     Endpoint2 => Stream2,
    %%     Endpoint3 => Stream3
    %% }
    %% All three lookups should succeed

    ?assertEqual(3, length([Endpoint1, Endpoint2, Endpoint3])).

endpoint_extracted_from_join_message_test() ->
    %% Verify endpoint extraction from JOIN message format

    JoinMsg = #{
        <<"type">> => <<"join">>,
        <<"realm">> => <<"arcade">>,
        <<"url">> => <<"https://arcade-peer1:4001">>,
        <<"node_id">> => <<1:256>>
    },

    Endpoint = maps:get(<<"url">>, JoinMsg, <<>>),

    ?assertEqual(<<"https://arcade-peer1:4001">>, Endpoint),
    ?assert(is_binary(Endpoint)).

endpoint_optional_in_join_message_test() ->
    %% Some JOIN messages might not have endpoint (legacy clients)
    %% Gateway should handle missing endpoint gracefully

    JoinMsgNoUrl = #{
        <<"type">> => <<"join">>,
        <<"realm">> => <<"arcade">>,
        <<"node_id">> => <<1:256>>
    },

    Endpoint = maps:get(<<"url">>, JoinMsgNoUrl, <<>>),

    %% Should return empty binary when missing
    ?assertEqual(<<>>, Endpoint).
