%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_rpc_dht module.
%%%
%%% Tests DHT provider discovery and announcement for RPC.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_dht_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% find_providers/2 Tests
%%%===================================================================

find_providers_with_results_test() ->
    %% GIVEN: DHT lookup function that returns providers
    Provider1 = #{
        node_id => <<"node1">>,
        address => {{127,0,0,1}, 4000},
        metadata => #{},
        last_seen => erlang:system_time(second)
    },
    DhtLookupFun = fun(_Uri) -> {ok, [Provider1]} end,

    %% WHEN: Finding providers
    Result = macula_rpc_dht:find_providers(<<"com.example.service">>, DhtLookupFun),

    %% THEN: Should return providers
    ?assertMatch({ok, [Provider1]}, Result).

find_providers_empty_test() ->
    %% GIVEN: DHT lookup function that returns empty list
    DhtLookupFun = fun(_Uri) -> {ok, []} end,

    %% WHEN: Finding providers
    Result = macula_rpc_dht:find_providers(<<"com.example.missing">>, DhtLookupFun),

    %% THEN: Should return empty list
    ?assertEqual({ok, []}, Result).

find_providers_error_test() ->
    %% GIVEN: DHT lookup function that returns error
    DhtLookupFun = fun(_Uri) -> {error, not_found} end,

    %% WHEN: Finding providers
    Result = macula_rpc_dht:find_providers(<<"com.example.error">>, DhtLookupFun),

    %% THEN: Should return error
    ?assertEqual({error, not_found}, Result).

%%%===================================================================
%%% find_with_cache/3,4 Tests
%%%===================================================================

find_with_cache_default_ttl_test() ->
    %% GIVEN: Cache and DHT lookup function
    Cache = macula_cache:new(100),
    Provider = #{
        node_id => <<"node1">>,
        address => {{127,0,0,1}, 4000},
        metadata => #{},
        last_seen => erlang:system_time(second)
    },
    DhtLookupFun = fun(_Uri) -> {ok, [Provider]} end,

    %% WHEN: Finding with cache (default TTL)
    Result = macula_rpc_dht:find_with_cache(
        <<"com.example.cached">>, Cache, DhtLookupFun
    ),

    %% THEN: Should return providers and updated cache
    ?assertMatch({ok, [Provider], _NewCache}, Result).

find_with_cache_custom_ttl_test() ->
    %% GIVEN: Cache and DHT lookup function
    Cache = macula_cache:new(100),
    Provider = #{
        node_id => <<"node1">>,
        address => {{127,0,0,1}, 4000},
        metadata => #{},
        last_seen => erlang:system_time(second)
    },
    DhtLookupFun = fun(_Uri) -> {ok, [Provider]} end,

    %% WHEN: Finding with cache (custom TTL of 60 seconds)
    Result = macula_rpc_dht:find_with_cache(
        <<"com.example.cached">>, Cache, DhtLookupFun, 60
    ),

    %% THEN: Should return providers and updated cache
    ?assertMatch({ok, [Provider], _NewCache}, Result).

%%%===================================================================
%%% announce/5 Tests
%%%===================================================================

announce_success_test() ->
    %% GIVEN: DHT publish function that succeeds
    DhtPublishFun = fun(_Uri, _NodeId, _Address, _Metadata) -> ok end,

    %% WHEN: Announcing service
    Result = macula_rpc_dht:announce(
        <<"com.example.myservice">>,
        <<"my_node_id">>,
        {{192,168,1,100}, 9443},
        #{version => <<"1.0">>},
        DhtPublishFun
    ),

    %% THEN: Should return ok
    ?assertEqual(ok, Result).

announce_error_test() ->
    %% GIVEN: DHT publish function that fails
    DhtPublishFun = fun(_Uri, _NodeId, _Address, _Metadata) -> {error, dht_unavailable} end,

    %% WHEN: Announcing service
    Result = macula_rpc_dht:announce(
        <<"com.example.myservice">>,
        <<"my_node_id">>,
        {{192,168,1,100}, 9443},
        #{},
        DhtPublishFun
    ),

    %% THEN: Should return error
    ?assertEqual({error, dht_unavailable}, Result).

announce_receives_correct_params_test() ->
    %% GIVEN: DHT publish function that captures parameters
    Self = self(),
    DhtPublishFun = fun(Uri, NodeId, Address, Metadata) ->
        Self ! {publish_called, Uri, NodeId, Address, Metadata},
        ok
    end,

    %% WHEN: Announcing service
    Uri = <<"com.example.paramtest">>,
    NodeId = <<"test_node">>,
    Address = {{10,0,0,1}, 8080},
    Metadata = #{capability => <<"rpc">>},
    macula_rpc_dht:announce(Uri, NodeId, Address, Metadata, DhtPublishFun),

    %% THEN: Should receive correct parameters
    receive
        {publish_called, ReceivedUri, ReceivedNodeId, ReceivedAddress, ReceivedMetadata} ->
            ?assertEqual(Uri, ReceivedUri),
            ?assertEqual(NodeId, ReceivedNodeId),
            ?assertEqual(Address, ReceivedAddress),
            ?assertEqual(Metadata, ReceivedMetadata)
    after 100 ->
        ?assert(false, "Did not receive publish_called message")
    end.

%%%===================================================================
%%% unannounce/3 Tests
%%%===================================================================

unannounce_success_test() ->
    %% GIVEN: DHT unpublish function that succeeds
    DhtUnpublishFun = fun(_Uri, _NodeId) -> ok end,

    %% WHEN: Unannouncing service
    Result = macula_rpc_dht:unannounce(
        <<"com.example.myservice">>,
        <<"my_node_id">>,
        DhtUnpublishFun
    ),

    %% THEN: Should return ok
    ?assertEqual(ok, Result).

unannounce_error_test() ->
    %% GIVEN: DHT unpublish function that fails
    DhtUnpublishFun = fun(_Uri, _NodeId) -> {error, not_found} end,

    %% WHEN: Unannouncing service
    Result = macula_rpc_dht:unannounce(
        <<"com.example.missing">>,
        <<"my_node_id">>,
        DhtUnpublishFun
    ),

    %% THEN: Should return error
    ?assertEqual({error, not_found}, Result).

%%%===================================================================
%%% filter_available/2 Tests
%%%===================================================================

filter_available_all_recent_test() ->
    %% GIVEN: Providers all seen recently (timestamps in milliseconds)
    Now = erlang:system_time(millisecond),
    Providers = [
        #{node_id => <<"n1">>, address => {{1,1,1,1}, 1}, metadata => #{}, last_seen => Now},
        #{node_id => <<"n2">>, address => {{2,2,2,2}, 2}, metadata => #{}, last_seen => Now - 10000}
    ],

    %% WHEN: Filtering with 60 second TTL
    Result = macula_rpc_dht:filter_available(Providers, 60),

    %% THEN: Should return all providers
    ?assertEqual(2, length(Result)).

filter_available_some_expired_test() ->
    %% GIVEN: Mix of recent and expired providers (timestamps in milliseconds)
    Now = erlang:system_time(millisecond),
    RecentProvider = #{node_id => <<"recent">>, address => {{1,1,1,1}, 1}, metadata => #{}, last_seen => Now},
    ExpiredProvider = #{node_id => <<"expired">>, address => {{2,2,2,2}, 2}, metadata => #{}, last_seen => Now - 120000},
    Providers = [RecentProvider, ExpiredProvider],

    %% WHEN: Filtering with 60 second TTL
    Result = macula_rpc_dht:filter_available(Providers, 60),

    %% THEN: Should only return recent provider
    ?assertEqual(1, length(Result)),
    [Returned] = Result,
    ?assertEqual(<<"recent">>, maps:get(node_id, Returned)).

filter_available_empty_list_test() ->
    %% GIVEN: Empty provider list
    Providers = [],

    %% WHEN: Filtering
    Result = macula_rpc_dht:filter_available(Providers, 60),

    %% THEN: Should return empty list
    ?assertEqual([], Result).
