%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for macula_content_dht module.
%%%
%%% Tests DHT-based content discovery and announcement:
%%% - Manifest announcement to DHT
%%% - Provider discovery
%%% - TTL and re-advertisement
%%% - DHT key generation
%%% @end
%%%-------------------------------------------------------------------
-module(macula_content_dht_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Descriptions
%%%===================================================================

macula_content_dht_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      %% Key generation tests
      {"DHT key from MCID is deterministic", fun dht_key_deterministic/0},
      {"Different MCIDs produce different keys", fun dht_key_unique/0},

      %% Provider info tests
      {"Create provider info with all fields", fun create_provider_info_complete/0},
      {"Create provider info validates node_id", fun create_provider_info_validates/0},

      %% Announcement tests
      {"Announce manifest creates correct DHT entry", fun announce_manifest_entry/0},
      {"Announce includes manifest metadata", fun announce_includes_metadata/0},
      {"Unannounce removes DHT entry", fun unannounce_removes_entry/0},

      %% Discovery tests
      {"Locate providers returns list", fun locate_providers_list/0},
      {"Locate non-existent returns empty", fun locate_nonexistent_empty/0},
      {"Locate returns provider info map", fun locate_returns_info/0},

      %% TTL tests
      {"Default TTL is 5 minutes", fun default_ttl_5_minutes/0},
      {"Custom TTL is respected", fun custom_ttl_respected/0},
      {"Calculate re-announce interval", fun reannounce_interval/0},

      %% Multiple providers tests
      {"Multiple providers can announce same content", fun multiple_providers/0},
      {"Discovery returns all providers", fun discover_all_providers/0}
     ]}.

%%%===================================================================
%%% Setup and Cleanup
%%%===================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%%===================================================================
%%% Test Helpers
%%%===================================================================

%% Create a test MCID (34 bytes: version + codec + hash)
test_mcid() ->
    Hash = crypto:strong_rand_bytes(32),
    <<1:8, 16#55:8, Hash/binary>>.

test_mcid(Seed) ->
    Hash = crypto:hash(sha256, Seed),
    <<1:8, 16#55:8, Hash/binary>>.

test_node_id() ->
    crypto:strong_rand_bytes(32).

test_endpoint() ->
    <<"quic://192.168.1.100:9443">>.

%%%===================================================================
%%% Key Generation Tests
%%%===================================================================

dht_key_deterministic() ->
    MCID = test_mcid(<<"deterministic">>),
    Key1 = macula_content_dht:dht_key(MCID),
    Key2 = macula_content_dht:dht_key(MCID),
    ?assertEqual(Key1, Key2),
    ?assertEqual(32, byte_size(Key1)).

dht_key_unique() ->
    MCID1 = test_mcid(<<"unique1">>),
    MCID2 = test_mcid(<<"unique2">>),
    Key1 = macula_content_dht:dht_key(MCID1),
    Key2 = macula_content_dht:dht_key(MCID2),
    ?assertNotEqual(Key1, Key2).

%%%===================================================================
%%% Provider Info Tests
%%%===================================================================

create_provider_info_complete() ->
    NodeId = test_node_id(),
    Endpoint = test_endpoint(),
    Metadata = #{name => <<"my_app.tar.gz">>, size => 1024000},

    Info = macula_content_dht:create_provider_info(NodeId, Endpoint, Metadata),

    ?assertEqual(NodeId, maps:get(node_id, Info)),
    ?assertEqual(Endpoint, maps:get(endpoint, Info)),
    ?assertEqual(Metadata, maps:get(metadata, Info)),
    ?assert(is_integer(maps:get(advertised_at, Info))).

create_provider_info_validates() ->
    Endpoint = test_endpoint(),
    Metadata = #{},

    %% Valid node_id (32 bytes)
    ValidNodeId = test_node_id(),
    Info = macula_content_dht:create_provider_info(ValidNodeId, Endpoint, Metadata),
    ?assertEqual(ValidNodeId, maps:get(node_id, Info)),

    %% Invalid node_id (not 32 bytes) - should still work but be flagged
    ShortNodeId = <<"short">>,
    ShortInfo = macula_content_dht:create_provider_info(ShortNodeId, Endpoint, Metadata),
    ?assertEqual(ShortNodeId, maps:get(node_id, ShortInfo)).

%%%===================================================================
%%% Announcement Tests
%%%===================================================================

announce_manifest_entry() ->
    MCID = test_mcid(),
    NodeId = test_node_id(),
    Endpoint = test_endpoint(),
    ManifestInfo = #{
        name => <<"test_app-1.0.0.tar.gz">>,
        size => 1024000,
        chunk_count => 5
    },

    {Key, Value} = macula_content_dht:create_announcement(MCID, NodeId, Endpoint, ManifestInfo),

    ?assertEqual(32, byte_size(Key)),
    ?assertEqual(NodeId, maps:get(node_id, Value)),
    ?assertEqual(Endpoint, maps:get(endpoint, Value)).

announce_includes_metadata() ->
    MCID = test_mcid(),
    NodeId = test_node_id(),
    Endpoint = test_endpoint(),
    ManifestInfo = #{
        name => <<"app.tar.gz">>,
        size => 5000,
        chunk_count => 1,
        hash_algorithm => blake3
    },

    {_Key, Value} = macula_content_dht:create_announcement(MCID, NodeId, Endpoint, ManifestInfo),

    Metadata = maps:get(metadata, Value, #{}),
    ?assertEqual(<<"app.tar.gz">>, maps:get(name, Metadata)),
    ?assertEqual(5000, maps:get(size, Metadata)),
    ?assertEqual(1, maps:get(chunk_count, Metadata)).

unannounce_removes_entry() ->
    MCID = test_mcid(),
    NodeId = test_node_id(),

    %% Verify DHT key is generated (32 bytes)
    Key = macula_content_dht:dht_key(MCID),
    ?assertEqual(32, byte_size(Key)),

    %% Verify removal value has correct structure
    RemoveValue = macula_content_dht:create_removal(NodeId),
    ?assertEqual(NodeId, maps:get(node_id, RemoveValue)),
    ?assertEqual(true, maps:get(removed, RemoveValue)).

%%%===================================================================
%%% Discovery Tests
%%%===================================================================

locate_providers_list() ->
    Providers = [
        #{node_id => test_node_id(), endpoint => <<"quic://a:9443">>},
        #{node_id => test_node_id(), endpoint => <<"quic://b:9443">>}
    ],

    Result = macula_content_dht:format_providers(Providers),

    ?assertEqual(2, length(Result)),
    ?assert(is_list(Result)).

locate_nonexistent_empty() ->
    Result = macula_content_dht:format_providers([]),
    ?assertEqual([], Result).

locate_returns_info() ->
    NodeId = test_node_id(),
    Endpoint = <<"quic://test:9443">>,
    Provider = #{
        node_id => NodeId,
        endpoint => Endpoint,
        metadata => #{name => <<"app.tar.gz">>},
        advertised_at => erlang:system_time(second)
    },

    [Formatted] = macula_content_dht:format_providers([Provider]),

    ?assertEqual(NodeId, maps:get(node_id, Formatted)),
    ?assertEqual(Endpoint, maps:get(endpoint, Formatted)).

%%%===================================================================
%%% TTL Tests
%%%===================================================================

default_ttl_5_minutes() ->
    TTL = macula_content_dht:default_ttl(),
    ?assertEqual(300, TTL).

custom_ttl_respected() ->
    CustomTTL = 600,
    Opts = #{ttl => CustomTTL},
    TTL = macula_content_dht:get_ttl(Opts),
    ?assertEqual(CustomTTL, TTL).

reannounce_interval() ->
    %% Re-announce should happen before TTL expires (TTL - 60s, min 30s)
    TTL = 300,
    Interval = macula_content_dht:reannounce_interval(TTL),
    ?assertEqual(240, Interval),  % 300 - 60

    %% With short TTL
    ShortTTL = 60,
    ShortInterval = macula_content_dht:reannounce_interval(ShortTTL),
    ?assertEqual(30, ShortInterval).  % minimum 30s

%%%===================================================================
%%% Multiple Providers Tests
%%%===================================================================

multiple_providers() ->
    MCID = test_mcid(<<"shared_content">>),

    %% Provider 1
    NodeId1 = test_node_id(),
    {Key1, Value1} = macula_content_dht:create_announcement(
        MCID, NodeId1, <<"quic://a:9443">>, #{name => <<"app.tar.gz">>}),

    %% Provider 2
    NodeId2 = test_node_id(),
    {Key2, Value2} = macula_content_dht:create_announcement(
        MCID, NodeId2, <<"quic://b:9443">>, #{name => <<"app.tar.gz">>}),

    %% Same MCID produces same key
    ?assertEqual(Key1, Key2),

    %% Different providers have different node_ids
    ?assertNotEqual(maps:get(node_id, Value1), maps:get(node_id, Value2)).

discover_all_providers() ->
    Providers = [
        #{node_id => <<"node1">>, endpoint => <<"quic://a:9443">>, metadata => #{}, advertised_at => 1},
        #{node_id => <<"node2">>, endpoint => <<"quic://b:9443">>, metadata => #{}, advertised_at => 2},
        #{node_id => <<"node3">>, endpoint => <<"quic://c:9443">>, metadata => #{}, advertised_at => 3}
    ],

    Formatted = macula_content_dht:format_providers(Providers),

    ?assertEqual(3, length(Formatted)),
    NodeIds = [maps:get(node_id, P) || P <- Formatted],
    ?assert(lists:member(<<"node1">>, NodeIds)),
    ?assert(lists:member(<<"node2">>, NodeIds)),
    ?assert(lists:member(<<"node3">>, NodeIds)).
