%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for macula_service_registry - DHT-based service advertisement.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_service_registry_test).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% gen_server callbacks for mock DHT
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% Test Descriptions
%%%===================================================================

%% This test suite covers:
%% 1. Registry creation with default and custom options
%% 2. Local service advertisement and lookup
%% 3. Service discovery with caching
%% 4. Cache expiration and pruning
%% 5. Multiple providers for same service

%%%===================================================================
%%% Setup/Teardown
%%%===================================================================

setup() ->
    macula_service_registry:new().

%%%===================================================================
%%% Registry Creation Tests
%%%===================================================================

new_registry_test() ->
    Registry = macula_service_registry:new(),
    ?assertMatch(#{local_services := _}, Registry),
    ?assertMatch(#{cache := _}, Registry),
    ?assertMatch(#{default_ttl := 300}, Registry),
    ?assertMatch(#{cache_ttl := 60}, Registry).

new_registry_with_options_test() ->
    Registry = macula_service_registry:new(#{
        default_ttl => 120,
        cache_ttl => 45
    }),
    ?assertMatch(#{default_ttl := 120}, Registry),
    ?assertMatch(#{cache_ttl := 45}, Registry).

%%%===================================================================
%%% Local Service Advertisement Tests
%%%===================================================================

advertise_local_service_test() ->
    Registry = setup(),
    Handler = fun(#{user_id := UserId}) ->
        {ok, #{user_id => UserId, name => <<"Alice">>}}
    end,
    Metadata = #{version => <<"1.0">>},

    %% Advertise service
    Registry2 = macula_service_registry:advertise_local(
        Registry,
        <<"my.app.get_user">>,
        Handler,
        Metadata
    ),

    %% Verify it's in the registry
    ?assertMatch(
        {ok, _Handler},
        macula_service_registry:get_local_handler(Registry2, <<"my.app.get_user">>)
    ),

    %% List services
    Services = macula_service_registry:list_local_services(Registry2),
    ?assert(lists:member(<<"my.app.get_user">>, Services)).

get_local_handler_test() ->
    Registry = setup(),
    Handler = fun(Args) -> {ok, Args} end,

    Registry2 = macula_service_registry:advertise_local(
        Registry,
        <<"test.procedure">>,
        Handler,
        #{}
    ),

    %% Found
    {ok, RetrievedHandler} = macula_service_registry:get_local_handler(
        Registry2,
        <<"test.procedure">>
    ),
    ?assertEqual({ok, #{foo => bar}}, RetrievedHandler(#{foo => bar})),

    %% Not found
    ?assertEqual(
        not_found,
        macula_service_registry:get_local_handler(Registry2, <<"nonexistent">>)
    ).

unadvertise_local_service_test() ->
    Registry = setup(),
    Handler = fun(Args) -> {ok, Args} end,

    %% Advertise
    Registry2 = macula_service_registry:advertise_local(
        Registry,
        <<"test.service">>,
        Handler,
        #{}
    ),

    %% Verify exists
    ?assertMatch(
        {ok, _},
        macula_service_registry:get_local_handler(Registry2, <<"test.service">>)
    ),

    %% Unadvertise
    Registry3 = macula_service_registry:unadvertise_local(Registry2, <<"test.service">>),

    %% Verify removed
    ?assertEqual(
        not_found,
        macula_service_registry:get_local_handler(Registry3, <<"test.service">>)
    ).

multiple_local_services_test() ->
    Registry = setup(),
    Handler1 = fun(_) -> {ok, service1} end,
    Handler2 = fun(_) -> {ok, service2} end,

    Registry2 = macula_service_registry:advertise_local(
        Registry,
        <<"service.one">>,
        Handler1,
        #{}
    ),

    Registry3 = macula_service_registry:advertise_local(
        Registry2,
        <<"service.two">>,
        Handler2,
        #{}
    ),

    Services = macula_service_registry:list_local_services(Registry3),
    ?assertEqual(2, length(Services)),
    ?assert(lists:member(<<"service.one">>, Services)),
    ?assert(lists:member(<<"service.two">>, Services)).

%%%===================================================================
%%% Service Discovery Cache Tests
%%%===================================================================

cache_service_test() ->
    Registry = setup(),
    ServiceId = <<"energy.home.get">>,
    Providers = [
        #{
            node_id => <<"node-001">>,
            endpoint => <<"https://node1.local:9443">>,
            metadata => #{},
            advertised_at => erlang:system_time(second)
        },
        #{
            node_id => <<"node-002">>,
            endpoint => <<"https://node2.local:9443">>,
            metadata => #{},
            advertised_at => erlang:system_time(second)
        }
    ],

    %% Cache the providers
    Registry2 = macula_service_registry:cache_service(Registry, ServiceId, Providers, 60),

    %% Discover should return cached value
    {ok, CachedProviders, _Registry3} = macula_service_registry:discover_service(
        Registry2,
        ServiceId
    ),

    ?assertEqual(2, length(CachedProviders)),
    ?assertEqual(Providers, CachedProviders).

discover_service_cache_miss_test() ->
    Registry = setup(),

    %% No cache entry
    {cache_miss, _Registry2} = macula_service_registry:discover_service(
        Registry,
        <<"nonexistent.service">>
    ).

discover_service_cache_hit_test() ->
    Registry = setup(),
    ServiceId = <<"test.service">>,
    Providers = [
        #{
            node_id => <<"node-123">>,
            endpoint => <<"https://test.local:9443">>,
            metadata => #{version => <<"1.0">>},
            advertised_at => erlang:system_time(second)
        }
    ],

    Registry2 = macula_service_registry:cache_service(Registry, ServiceId, Providers, 60),

    %% Should hit cache
    {ok, CachedProviders, _Registry3} = macula_service_registry:discover_service(
        Registry2,
        ServiceId
    ),

    ?assertEqual(Providers, CachedProviders).

discover_service_expired_cache_test() ->
    %% Use shorter TTL for testing (0 seconds = instant expiry)
    Registry = macula_service_registry:new(#{cache_ttl => 0}),

    ServiceId = <<"test.service">>,
    Providers = [#{
        node_id => <<"node-old">>,
        endpoint => <<"https://old.local:9443">>,
        metadata => #{},
        advertised_at => erlang:system_time(second)
    }],

    Registry2 = macula_service_registry:cache_service(Registry, ServiceId, Providers, 0),

    %% Wait a moment to ensure time has passed
    timer:sleep(100),

    %% Should be cache miss now (expired immediately)
    {cache_miss, _Registry3} = macula_service_registry:discover_service(
        Registry2,
        ServiceId
    ).

discover_service_force_refresh_test() ->
    Registry = setup(),
    ServiceId = <<"test.service">>,
    Providers = [#{
        node_id => <<"node-cached">>,
        endpoint => <<"https://cached.local:9443">>,
        metadata => #{},
        advertised_at => erlang:system_time(second)
    }],

    Registry2 = macula_service_registry:cache_service(Registry, ServiceId, Providers, 300),

    %% Force refresh (skip cache)
    {cache_miss, _Registry3} = macula_service_registry:discover_service(
        Registry2,
        ServiceId,
        #{force_refresh => true}
    ).

%%%===================================================================
%%% Cache Management Tests
%%%===================================================================

prune_expired_test() ->
    %% Create registry with 0-second TTL (instant expiry)
    Registry = macula_service_registry:new(#{cache_ttl => 0}),

    %% Add multiple services
    ServiceId1 = <<"service.one">>,
    ServiceId2 = <<"service.two">>,
    Providers = [#{
        node_id => <<"node-test">>,
        endpoint => <<"https://test.local:9443">>,
        metadata => #{},
        advertised_at => erlang:system_time(second)
    }],

    Registry2 = macula_service_registry:cache_service(Registry, ServiceId1, Providers, 0),
    Registry3 = macula_service_registry:cache_service(Registry2, ServiceId2, Providers, 0),

    %% Wait a moment for time to pass
    timer:sleep(100),

    %% Prune
    {Registry4, RemovedCount} = macula_service_registry:prune_expired(Registry3),

    %% Should have removed 2 entries
    ?assertEqual(2, RemovedCount),

    %% Cache should be empty
    {cache_miss, _} = macula_service_registry:discover_service(Registry4, ServiceId1),
    {cache_miss, _} = macula_service_registry:discover_service(Registry4, ServiceId2).

clear_cache_test() ->
    Registry = setup(),
    Providers = [#{
        node_id => <<"node-test">>,
        endpoint => <<"https://test.local:9443">>,
        metadata => #{},
        advertised_at => erlang:system_time(second)
    }],

    %% Add some cached entries
    Registry2 = macula_service_registry:cache_service(Registry, <<"service.one">>, Providers, 60),
    Registry3 = macula_service_registry:cache_service(Registry2, <<"service.two">>, Providers, 60),

    %% Clear cache
    Registry4 = macula_service_registry:clear_cache(Registry3),

    %% All should be cache misses
    {cache_miss, _} = macula_service_registry:discover_service(Registry4, <<"service.one">>),
    {cache_miss, _} = macula_service_registry:discover_service(Registry4, <<"service.two">>).

prune_expired_local_services_test() ->
    %% Create registry with 0-second service TTL (instant expiry)
    Registry = macula_service_registry:new(#{service_ttl => 0}),

    %% Advertise multiple local services
    Handler1 = fun(_) -> {ok, service1} end,
    Handler2 = fun(_) -> {ok, service2} end,

    Registry2 = macula_service_registry:advertise_local(Registry, <<"service.one">>, Handler1, #{}),
    Registry3 = macula_service_registry:advertise_local(Registry2, <<"service.two">>, Handler2, #{}),

    %% Verify both services exist
    Services = macula_service_registry:list_local_services(Registry3),
    ?assertEqual(2, length(Services)),

    %% Wait a moment for time to pass
    timer:sleep(100),

    %% Prune expired services
    {Registry4, RemovedCount} = macula_service_registry:prune_expired_local_services(Registry3),

    %% Should have removed 2 entries
    ?assertEqual(2, RemovedCount),

    %% Services should be gone
    ?assertEqual(not_found, macula_service_registry:get_local_handler(Registry4, <<"service.one">>)),
    ?assertEqual(not_found, macula_service_registry:get_local_handler(Registry4, <<"service.two">>)),

    %% List should be empty
    ?assertEqual([], macula_service_registry:list_local_services(Registry4)).

%%%===================================================================
%%% Subscriber Cache Tests (PubSub)
%%%===================================================================

cache_subscribers_test() ->
    Registry = setup(),
    Topic = <<"energy.home.measured">>,
    Subscribers = [
        #{
            node_id => <<"sub-node-001">>,
            endpoint => <<"https://sub1.local:9443">>,
            metadata => #{},
            subscribed_at => erlang:system_time(second)
        },
        #{
            node_id => <<"sub-node-002">>,
            endpoint => <<"https://sub2.local:9443">>,
            metadata => #{},
            subscribed_at => erlang:system_time(second)
        }
    ],

    %% Cache the subscribers
    Registry2 = macula_service_registry:cache_subscribers(Registry, Topic, Subscribers, 60),

    %% Discover should return cached value
    {ok, CachedSubscribers, _Registry3} = macula_service_registry:discover_subscribers(
        Registry2,
        Topic
    ),

    ?assertEqual(2, length(CachedSubscribers)),
    ?assertEqual(Subscribers, CachedSubscribers).

discover_subscribers_cache_miss_test() ->
    Registry = setup(),

    %% No cache entry
    {cache_miss, _Registry2} = macula_service_registry:discover_subscribers(
        Registry,
        <<"nonexistent.topic">>
    ).

discover_subscribers_cache_hit_test() ->
    Registry = setup(),
    Topic = <<"test.topic">>,
    Subscribers = [
        #{
            node_id => <<"sub-123">>,
            endpoint => <<"https://test.local:9443">>,
            metadata => #{version => <<"1.0">>},
            subscribed_at => erlang:system_time(second)
        }
    ],

    Registry2 = macula_service_registry:cache_subscribers(Registry, Topic, Subscribers, 60),

    %% Should hit cache
    {ok, CachedSubscribers, _Registry3} = macula_service_registry:discover_subscribers(
        Registry2,
        Topic
    ),

    ?assertEqual(Subscribers, CachedSubscribers).

discover_subscribers_expired_cache_test() ->
    %% Use shorter TTL for testing (0 seconds = instant expiry)
    Registry = macula_service_registry:new(#{cache_ttl => 0}),

    Topic = <<"test.topic">>,
    Subscribers = [#{
        node_id => <<"sub-old">>,
        endpoint => <<"https://old.local:9443">>,
        metadata => #{},
        subscribed_at => erlang:system_time(second)
    }],

    Registry2 = macula_service_registry:cache_subscribers(Registry, Topic, Subscribers, 0),

    %% Wait a moment to ensure time has passed
    timer:sleep(100),

    %% Should be cache miss now (expired immediately)
    {cache_miss, _Registry3} = macula_service_registry:discover_subscribers(
        Registry2,
        Topic
    ).

prune_expired_subscribers_test() ->
    %% Create registry with 0-second TTL (instant expiry)
    Registry = macula_service_registry:new(#{cache_ttl => 0}),

    %% Add multiple topics
    Topic1 = <<"topic.one">>,
    Topic2 = <<"topic.two">>,
    Subscribers = [#{
        node_id => <<"sub-test">>,
        endpoint => <<"https://test.local:9443">>,
        metadata => #{},
        subscribed_at => erlang:system_time(second)
    }],

    Registry2 = macula_service_registry:cache_subscribers(Registry, Topic1, Subscribers, 0),
    Registry3 = macula_service_registry:cache_subscribers(Registry2, Topic2, Subscribers, 0),

    %% Wait a moment for time to pass
    timer:sleep(100),

    %% Prune
    {Registry4, RemovedCount} = macula_service_registry:prune_expired_subscribers(Registry3),

    %% Should have removed 2 entries
    ?assertEqual(2, RemovedCount),

    %% Cache should be empty
    {cache_miss, _} = macula_service_registry:discover_subscribers(Registry4, Topic1),
    {cache_miss, _} = macula_service_registry:discover_subscribers(Registry4, Topic2).

clear_subscriber_cache_test() ->
    Registry = setup(),
    Subscribers = [#{
        node_id => <<"sub-test">>,
        endpoint => <<"https://test.local:9443">>,
        metadata => #{},
        subscribed_at => erlang:system_time(second)
    }],

    %% Add some cached entries
    Registry2 = macula_service_registry:cache_subscribers(Registry, <<"topic.one">>, Subscribers, 60),
    Registry3 = macula_service_registry:cache_subscribers(Registry2, <<"topic.two">>, Subscribers, 60),

    %% Clear subscriber cache
    Registry4 = macula_service_registry:clear_subscriber_cache(Registry3),

    %% All should be cache misses
    {cache_miss, _} = macula_service_registry:discover_subscribers(Registry4, <<"topic.one">>),
    {cache_miss, _} = macula_service_registry:discover_subscribers(Registry4, <<"topic.two">>).

multiple_subscribers_per_topic_test() ->
    Registry = setup(),
    Topic = <<"sensor.temperature">>,

    %% Cache 5 subscribers
    Subscribers = lists:map(fun(N) ->
        #{
            node_id => iolist_to_binary(io_lib:format("sub-~3..0B", [N])),
            endpoint => iolist_to_binary(io_lib:format("https://sub~p.local:9443", [N])),
            metadata => #{index => N},
            subscribed_at => erlang:system_time(second)
        }
    end, lists:seq(1, 5)),

    Registry2 = macula_service_registry:cache_subscribers(Registry, Topic, Subscribers, 60),

    %% Verify all 5 are cached
    {ok, CachedSubscribers, _} = macula_service_registry:discover_subscribers(Registry2, Topic),
    ?assertEqual(5, length(CachedSubscribers)),
    ?assertEqual(Subscribers, CachedSubscribers).

subscriber_cache_independent_from_service_cache_test() ->
    Registry = setup(),

    %% Cache a service
    ServiceProviders = [#{
        node_id => <<"provider-001">>,
        endpoint => <<"https://provider.local:9443">>,
        metadata => #{},
        advertised_at => erlang:system_time(second)
    }],
    Registry2 = macula_service_registry:cache_service(Registry, <<"rpc.service">>, ServiceProviders, 60),

    %% Cache subscribers for different topic
    Subscribers = [#{
        node_id => <<"sub-001">>,
        endpoint => <<"https://sub.local:9443">>,
        metadata => #{},
        subscribed_at => erlang:system_time(second)
    }],
    Registry3 = macula_service_registry:cache_subscribers(Registry2, <<"pubsub.topic">>, Subscribers, 60),

    %% Both caches should be independent
    {ok, CachedProviders, _} = macula_service_registry:discover_service(Registry3, <<"rpc.service">>),
    {ok, CachedSubscribers, _} = macula_service_registry:discover_subscribers(Registry3, <<"pubsub.topic">>),

    ?assertEqual(ServiceProviders, CachedProviders),
    ?assertEqual(Subscribers, CachedSubscribers).

%%%===================================================================
%%% Integration Scenarios
%%%===================================================================

full_lifecycle_test() ->
    Registry = setup(),

    %% 1. Advertise local service
    Handler = fun(#{name := Name}) ->
        {ok, #{greeting => <<"Hello, ", Name/binary>>}}
    end,

    Registry2 = macula_service_registry:advertise_local(
        Registry,
        <<"greeter.hello">>,
        Handler,
        #{version => <<"1.0">>}
    ),

    %% 2. Verify local handler works
    {ok, LocalHandler} = macula_service_registry:get_local_handler(
        Registry2,
        <<"greeter.hello">>
    ),
    {ok, Result} = LocalHandler(#{name => <<"Alice">>}),
    ?assertEqual(#{greeting => <<"Hello, Alice">>}, Result),

    %% 3. Cache remote providers (simulating DHT discovery)
    RemoteProviders = [#{
        node_id => <<"remote-node-001">>,
        endpoint => <<"https://remote.local:9443">>,
        metadata => #{version => <<"1.0">>},
        advertised_at => erlang:system_time(second)
    }],

    Registry3 = macula_service_registry:cache_service(
        Registry2,
        <<"remote.service">>,
        RemoteProviders,
        60
    ),

    %% 4. Discover remote service (cache hit)
    {ok, Providers, _Registry4} = macula_service_registry:discover_service(
        Registry3,
        <<"remote.service">>
    ),
    ?assertEqual(RemoteProviders, Providers),

    %% 5. Unadvertise local service
    Registry5 = macula_service_registry:unadvertise_local(Registry3, <<"greeter.hello">>),
    ?assertEqual(
        not_found,
        macula_service_registry:get_local_handler(Registry5, <<"greeter.hello">>)
    ).

%%%===================================================================
%%% DHT Integration Tests
%%%===================================================================

dht_publish_success_test() ->
    %% Start mock DHT server
    {ok, DhtPid} = start_mock_dht(),

    ProviderInfo = #{
        node_id => <<"test-node-123">>,
        endpoint => <<"https://localhost:9443">>,
        metadata => #{version => <<"1.0">>}
    },

    %% Publish to DHT
    ok = macula_service_registry:publish_to_dht(
        DhtPid,
        <<"energy.home.get">>,
        ProviderInfo,
        300,  % TTL
        20    % K
    ),

    %% Verify it was stored (DHT now returns a list)
    Key = crypto:hash(sha256, <<"energy.home.get">>),
    {ok, Stored} = gen_server:call(DhtPid, {get_local, Key}),

    %% Check that we got a list with one provider
    ?assert(is_list(Stored)),
    ?assertEqual(1, length(Stored)),

    %% Check the stored provider has the expected fields
    [Provider] = Stored,
    ?assertMatch(#{node_id := <<"test-node-123">>}, Provider),
    ?assertMatch(#{endpoint := <<"https://localhost:9443">>}, Provider),
    ?assertMatch(#{ttl := 300}, Provider),
    ?assert(maps:is_key(advertised_at, Provider)),

    stop_mock_dht(DhtPid).

dht_query_success_test() ->
    %% Start mock DHT server
    {ok, DhtPid} = start_mock_dht(),

    ProviderInfo = #{
        node_id => <<"provider-456">>,
        endpoint => <<"https://provider.local:9443">>,
        metadata => #{},
        advertised_at => erlang:system_time(second),
        ttl => 300
    },

    %% Pre-populate DHT
    Key = crypto:hash(sha256, <<"calculator.add">>),
    ok = gen_server:call(DhtPid, {store_local, Key, ProviderInfo}),

    %% Query DHT
    {ok, Providers} = macula_service_registry:query_dht_for_service(
        DhtPid,
        <<"calculator.add">>,
        20  % K
    ),

    ?assertEqual([ProviderInfo], Providers),

    stop_mock_dht(DhtPid).

dht_query_not_found_test() ->
    %% Start mock DHT server
    {ok, DhtPid} = start_mock_dht(),

    %% Query for non-existent service
    {ok, Providers} = macula_service_registry:query_dht_for_service(
        DhtPid,
        <<"nonexistent.service">>,
        20
    ),

    ?assertEqual([], Providers),

    stop_mock_dht(DhtPid).

dht_remove_success_test() ->
    %% Start mock DHT server
    {ok, DhtPid} = start_mock_dht(),

    ProviderInfo = #{
        node_id => <<"node-789">>,
        endpoint => <<"https://localhost:9443">>,
        metadata => #{}
    },

    %% Publish then remove
    ok = macula_service_registry:publish_to_dht(
        DhtPid,
        <<"temp.service">>,
        ProviderInfo,
        300,
        20
    ),

    ok = macula_service_registry:remove_from_dht(
        DhtPid,
        <<"temp.service">>,
        <<"node-789">>
    ),

    %% Verify removed
    {ok, Providers} = macula_service_registry:query_dht_for_service(
        DhtPid,
        <<"temp.service">>,
        20
    ),

    ?assertEqual([], Providers),

    stop_mock_dht(DhtPid).

dht_not_available_test() ->
    %% DHT not registered/started
    ProviderInfo = #{
        node_id => <<"node-xyz">>,
        endpoint => <<"https://localhost:9443">>,
        metadata => #{}
    },

    %% Publish should return error
    {error, dht_not_available} = macula_service_registry:publish_to_dht(
        nonexistent_dht_server,
        <<"service.test">>,
        ProviderInfo,
        300,
        20
    ),

    %% Query should return error
    {error, dht_not_available} = macula_service_registry:query_dht_for_service(
        nonexistent_dht_server,
        <<"service.test">>,
        20
    ),

    %% Remove should succeed (gracefully ignore)
    ok = macula_service_registry:remove_from_dht(
        nonexistent_dht_server,
        <<"service.test">>,
        <<"node-xyz">>
    ).

%%%===================================================================
%%% Mock DHT Server (for testing)
%%%===================================================================

%% Simple gen_server that mimics macula_routing_server storage operations
start_mock_dht() ->
    gen_server:start_link(?MODULE, [], []).

stop_mock_dht(Pid) ->
    gen_server:stop(Pid).

%% Helper function to find existing provider in list
find_existing_provider(_NodeId, []) ->
    not_found;
find_existing_provider(undefined, _ProviderList) ->
    not_found;
find_existing_provider(NodeId, ProviderList) ->
    find_existing_provider_index(NodeId, ProviderList, 1).

find_existing_provider_index(_NodeId, [], _Index) ->
    not_found;
find_existing_provider_index(NodeId, [Provider | Rest], Index) ->
    case maps:get(node_id, Provider, undefined) of
        NodeId -> {found, Index};
        _ -> find_existing_provider_index(NodeId, Rest, Index + 1)
    end.

%% gen_server callbacks for mock DHT
init([]) ->
    {ok, #{}}.  % State is just a map of keys to values

handle_call({store_local, Key, Value}, _From, State) ->
    %% Store providers as a list to support multiple providers
    ExistingProviders = maps:get(Key, State, []),
    ProviderList = case is_list(ExistingProviders) of
        true -> ExistingProviders;
        false -> [ExistingProviders]
    end,

    %% Get node_id from the new provider
    NodeId = maps:get(node_id, Value, undefined),

    %% Update or append provider
    UpdatedProviders = case find_existing_provider(NodeId, ProviderList) of
        {found, Index} ->
            %% Update existing provider
            lists:sublist(ProviderList, Index - 1) ++
                [Value] ++
                lists:nthtail(Index, ProviderList);
        not_found ->
            %% Append new provider
            [Value | ProviderList]
    end,

    {reply, ok, State#{Key => UpdatedProviders}};

handle_call({get_local, Key}, _From, State) ->
    case maps:get(Key, State, undefined) of
        undefined ->
            {reply, not_found, State};
        Value when is_list(Value) ->
            {reply, {ok, Value}, State};
        Value ->
            %% Legacy: single value, convert to list
            {reply, {ok, [Value]}, State}
    end;

handle_call({delete_local, Key, NodeId}, _From, State) ->
    %% Remove specific provider by node_id from the list
    NewState = case maps:get(Key, State, undefined) of
        undefined ->
            State;
        Providers when is_list(Providers) ->
            UpdatedProviders = lists:filter(fun(P) ->
                maps:get(node_id, P, undefined) =/= NodeId
            end, Providers),
            case UpdatedProviders of
                [] -> maps:remove(Key, State);
                _ -> State#{Key => UpdatedProviders}
            end;
        _SingleValue ->
            maps:remove(Key, State)
    end,
    {reply, ok, NewState}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
