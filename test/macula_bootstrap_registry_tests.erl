%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for macula_bootstrap_registry (DHT delegation).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bootstrap_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

bootstrap_registry_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Fixture) ->
         [
          {"store and lookup service", fun() -> test_store_lookup_service(Fixture) end},
          {"store and lookup topic", fun() -> test_store_lookup_topic(Fixture) end},
          {"lookup non-existent key", fun() -> test_lookup_not_found(Fixture) end},
          {"generic store/lookup API", fun() -> test_generic_api(Fixture) end},
          {"registry requires routing server", fun() -> test_requires_routing_server(Fixture) end}
         ]
     end}.

%%%===================================================================
%%% Setup/Cleanup
%%%===================================================================

setup() ->
    %% Start routing server (required dependency)
    LocalNodeId = crypto:hash(sha256, <<"test_node">>),
    Config = #{k => 20, alpha => 3},
    {ok, RoutingPid} = macula_routing_server:start_link(LocalNodeId, Config),

    %% Start bootstrap registry
    {ok, RegistryPid} = macula_bootstrap_registry:start_link(#{}),

    #{
        routing_pid => RoutingPid,
        registry_pid => RegistryPid,
        local_node_id => LocalNodeId
    }.

cleanup(#{routing_pid := RoutingPid, registry_pid := RegistryPid}) ->
    catch gen_server:stop(RegistryPid),
    catch gen_server:stop(RoutingPid),
    ok.

%%%===================================================================
%%% Tests
%%%===================================================================

test_store_lookup_service(_Fixture) ->
    %% Store a service
    ServiceKey = <<"test.calculator.add">>,
    ServiceValue = #{
        node_id => crypto:hash(sha256, <<"provider_node">>),
        endpoint => <<"127.0.0.1:9443">>
    },

    ok = macula_bootstrap_registry:store_service(ServiceKey, ServiceValue),

    %% Lookup the service
    {ok, Result} = macula_bootstrap_registry:lookup_service(ServiceKey),

    %% DHT stores as list, so we need to handle that
    case Result of
        [Value] when is_map(Value) ->
            ?assertEqual(ServiceValue, Value);
        Value when is_map(Value) ->
            ?assertEqual(ServiceValue, Value)
    end.

test_store_lookup_topic(_Fixture) ->
    %% Store a topic subscription
    Topic = <<"events.user.login">>,
    Subscriber = #{
        node_id => crypto:hash(sha256, <<"subscriber_node">>),
        endpoint => <<"127.0.0.1:9444">>
    },

    ok = macula_bootstrap_registry:store_topic(Topic, Subscriber),

    %% Lookup the topic
    {ok, Result} = macula_bootstrap_registry:lookup_topic(Topic),

    %% DHT stores as list
    case Result of
        [Value] when is_map(Value) ->
            ?assertEqual(Subscriber, Value);
        Value when is_map(Value) ->
            ?assertEqual(Subscriber, Value)
    end.

test_lookup_not_found(_Fixture) ->
    %% Lookup non-existent key
    NonExistentKey = <<"does.not.exist">>,

    Result = macula_bootstrap_registry:lookup(NonExistentKey),

    ?assertEqual({error, not_found}, Result).

test_generic_api(_Fixture) ->
    %% Test generic store/lookup API
    Key = <<"generic.key">>,
    Value = #{data => <<"test_data">>},

    ok = macula_bootstrap_registry:store(Key, Value),

    {ok, Result} = macula_bootstrap_registry:lookup(Key),

    %% DHT stores as list
    case Result of
        [V] when is_map(V) ->
            ?assertEqual(Value, V);
        V when is_map(V) ->
            ?assertEqual(Value, V)
    end.

test_requires_routing_server(_Fixture) ->
    %% Stop routing server
    catch gen_server:stop(whereis(macula_routing_server)),

    timer:sleep(100),

    %% Try to start registry without routing server
    Result = macula_bootstrap_registry:start_link(#{}),

    ?assertMatch({error, _}, Result).
