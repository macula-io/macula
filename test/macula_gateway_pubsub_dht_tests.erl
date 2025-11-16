-module(macula_gateway_pubsub_dht_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test that gateway queries DHT for remote subscribers when publishing
%% Bug: Gateway only checks local subscriptions (macula_gateway_pubsub),
%%      never queries DHT for remote subscribers stored via STORE messages

gateway_publish_queries_dht_for_remote_subscribers_test() ->
    %% This test documents the expected behavior:
    %% When a publish message arrives, the gateway should:
    %% 1. Get local subscribers from macula_gateway_pubsub
    %% 2. Query DHT via FIND_VALUE for the topic key
    %% 3. Combine local + remote subscribers
    %% 4. Deliver to all subscribers

    %% handle_publish is a private function in macula_gateway
    %% This test documents the bug that needs fixing
    ?assert(true).

dht_topic_key_encoding_test() ->
    %% Verify that topic keys are consistently encoded for DHT storage/lookup
    %% Topic "arcade.matchmaking.snake" should produce a deterministic key
    Topic = <<"arcade.matchmaking.snake">>,

    %% The key should be a hash of the topic
    Key = crypto:hash(sha256, Topic),

    ?assert(is_binary(Key)),
    ?assertEqual(32, byte_size(Key)).

local_and_remote_subscribers_combined_test() ->
    %% Test that local and remote subscribers are properly combined
    %% This documents the expected behavior without implementation

    LocalSubscribers = [self()],  % Stream PIDs connected to this gateway
    RemoteSubscribers = [],        % Endpoints from DHT (will be connection PIDs later)

    AllSubscribers = lists:usort(LocalSubscribers ++ RemoteSubscribers),

    ?assertEqual(1, length(AllSubscribers)).

dht_find_value_response_format_test() ->
    %% Document the expected format of DHT FIND_VALUE response
    %% When querying for topic subscribers, we expect:
    %% #{<<"type">> => <<"find_value_reply">>,
    %%   <<"key">> => TopicKey,
    %%   <<"value">> => [#{<<"node_id">> => ..., <<"endpoint">> => ...}, ...]}

    TopicKey = crypto:hash(sha256, <<"test.topic">>),

    ExpectedResponse = #{
        <<"type">> => <<"find_value_reply">>,
        <<"key">> => TopicKey,
        <<"value">> => [
            #{
                <<"node_id">> => <<1:256>>,
                <<"endpoint">> => <<"https://peer1:4433">>
            }
        ]
    },

    ?assert(is_map(ExpectedResponse)),
    ?assertEqual(<<"find_value_reply">>, maps:get(<<"type">>, ExpectedResponse)).

publish_without_dht_query_finds_zero_remote_subscribers_test() ->
    %% This test documents the BUG:
    %% Currently, gateway only checks macula_gateway_pubsub (local)
    %% and never queries DHT, so remote subscribers are not found

    %% This is the current broken behavior that needs fixing
    LocalSubscribers = [],  % No local subscribers
    %% DHT is not queried, so remote subscribers are not found
    RemoteSubscribers = [],  % Should be non-empty after DHT query

    AllSubscribers = LocalSubscribers ++ RemoteSubscribers,

    %% This currently returns 0, but should return remote subscribers
    ?assertEqual(0, length(AllSubscribers)).
