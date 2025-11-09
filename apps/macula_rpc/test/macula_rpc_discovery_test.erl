%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_rpc_discovery module.
%%% Tests written FIRST (TDD red phase).
%%% DHT integration for finding RPC service providers.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_discovery_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Mock DHT Setup
%%%===================================================================

%% Mock provider info
mock_provider(NodeId, Port) ->
    #{
        node_id => <<NodeId:256>>,
        address => {{127,0,0,1}, Port},
        metadata => #{},
        last_seen => erlang:system_time(millisecond)
    }.

%% Mock DHT that returns predefined providers
mock_dht_lookup(_Uri) ->
    {ok, [
        mock_provider(1, 8080),
        mock_provider(2, 8081)
    ]}.

mock_dht_lookup_empty(_Uri) ->
    {ok, []}.

mock_dht_lookup_error(_Uri) ->
    {error, timeout}.

%%%===================================================================
%%% Find Providers Tests
%%%===================================================================

%% Test: find_providers returns providers from DHT
find_providers_from_dht_test() ->
    Uri = <<"be.cortexiq.home.get_measurement">>,

    Result = macula_rpc_discovery:find_providers(Uri, fun mock_dht_lookup/1),

    ?assertMatch({ok, [_, _]}, Result),
    {ok, Providers} = Result,
    ?assertEqual(2, length(Providers)).

%% Test: find_providers returns empty list when no providers
find_providers_empty_test() ->
    Uri = <<"be.cortexiq.home.get_measurement">>,

    Result = macula_rpc_discovery:find_providers(Uri, fun mock_dht_lookup_empty/1),

    ?assertEqual({ok, []}, Result).

%% Test: find_providers propagates DHT errors
find_providers_error_test() ->
    Uri = <<"be.cortexiq.home.get_measurement">>,

    Result = macula_rpc_discovery:find_providers(Uri, fun mock_dht_lookup_error/1),

    ?assertEqual({error, timeout}, Result).

%%%===================================================================
%%% Announce Tests
%%%===================================================================

%% Test: announce_registration publishes to DHT
announce_registration_test() ->
    Uri = <<"be.cortexiq.home.get_measurement">>,
    LocalNodeId = <<123:256>>,
    LocalAddress = {{192,168,1,100}, 8080},
    Metadata = #{rate_limit => 100},

    %% Mock DHT publish function
    PublishFun = fun(U, NodeId, Addr, Meta) ->
        ?assertEqual(Uri, U),
        ?assertEqual(LocalNodeId, NodeId),
        ?assertEqual(LocalAddress, Addr),
        ?assertEqual(Metadata, Meta),
        ok
    end,

    Result = macula_rpc_discovery:announce_registration(Uri, LocalNodeId, LocalAddress, Metadata, PublishFun),

    ?assertEqual(ok, Result).

%% Test: announce_registration propagates DHT errors
announce_registration_error_test() ->
    Uri = <<"be.cortexiq.home.get_measurement">>,
    LocalNodeId = <<123:256>>,
    LocalAddress = {{192,168,1,100}, 8080},

    %% Mock DHT publish that fails
    PublishFun = fun(_, _, _, _) -> {error, network_error} end,

    Result = macula_rpc_discovery:announce_registration(Uri, LocalNodeId, LocalAddress, #{}, PublishFun),

    ?assertEqual({error, network_error}, Result).

%%%===================================================================
%%% Remove Tests
%%%===================================================================

%% Test: remove_registration removes from DHT
remove_registration_test() ->
    Uri = <<"be.cortexiq.home.get_measurement">>,
    LocalNodeId = <<123:256>>,

    %% Mock DHT unpublish function
    UnpublishFun = fun(U, NodeId) ->
        ?assertEqual(Uri, U),
        ?assertEqual(LocalNodeId, NodeId),
        ok
    end,

    Result = macula_rpc_discovery:remove_registration(Uri, LocalNodeId, UnpublishFun),

    ?assertEqual(ok, Result).

%% Test: remove_registration propagates DHT errors
remove_registration_error_test() ->
    Uri = <<"be.cortexiq.home.get_measurement">>,
    LocalNodeId = <<123:256>>,

    %% Mock DHT unpublish that fails
    UnpublishFun = fun(_, _) -> {error, not_found} end,

    Result = macula_rpc_discovery:remove_registration(Uri, LocalNodeId, UnpublishFun),

    ?assertEqual({error, not_found}, Result).

%%%===================================================================
%%% Filter Tests
%%%===================================================================

%% Test: filter_available removes stale providers
filter_available_test() ->
    Now = erlang:system_time(millisecond),
    StaleTime = Now - 400000,  % 400 seconds ago

    Providers = [
        (mock_provider(1, 8080))#{last_seen => Now},           % Fresh
        (mock_provider(2, 8081))#{last_seen => StaleTime},     % Stale
        (mock_provider(3, 8082))#{last_seen => Now}            % Fresh
    ],

    TTL = 300,  % 300 seconds
    Available = macula_rpc_discovery:filter_available(Providers, TTL),

    ?assertEqual(2, length(Available)).

%% Test: filter_available keeps all when none stale
filter_available_all_fresh_test() ->
    Now = erlang:system_time(millisecond),

    Providers = [
        (mock_provider(1, 8080))#{last_seen => Now},
        (mock_provider(2, 8081))#{last_seen => Now}
    ],

    TTL = 300,
    Available = macula_rpc_discovery:filter_available(Providers, TTL),

    ?assertEqual(2, length(Available)).

%% Test: filter_available returns empty when all stale
filter_available_all_stale_test() ->
    StaleTime = erlang:system_time(millisecond) - 400000,

    Providers = [
        (mock_provider(1, 8080))#{last_seen => StaleTime},
        (mock_provider(2, 8081))#{last_seen => StaleTime}
    ],

    TTL = 300,
    Available = macula_rpc_discovery:filter_available(Providers, TTL),

    ?assertEqual([], Available).
