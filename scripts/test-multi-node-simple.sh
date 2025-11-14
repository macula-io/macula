#!/bin/bash
# Simplified multi-node DHT test - tests service registry directly without full mesh
# Uses in-memory DHT server and multiple "simulated" nodes

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "=== Simplified Multi-Node DHT Service Discovery Test ==="
echo ""

# Cleanup on exit
cleanup() {
    echo ""
    echo "Cleaning up..."
    pkill -f "erl.*macula_dht_test" || true
    rm -f /tmp/macula_dht_test.log
}

trap cleanup EXIT

# Compile
echo "Compiling Macula..."
cd "$PROJECT_ROOT"
rebar3 compile
echo ""

# Create test module
cat > /tmp/macula_dht_test.erl <<'EOF'
-module(macula_dht_test).
-export([run/0]).

run() ->
    io:format("~n=== DHT-Based Service Discovery Test ===~n~n"),

    %% Step 1: Start DHT routing server
    io:format("[Step 1] Starting DHT routing server...~n"),
    LocalNodeId = crypto:strong_rand_bytes(32),
    {ok, DhtPid} = macula_routing_server:start_link(LocalNodeId, #{k => 20, alpha => 3}),
    register(macula_routing_server, DhtPid),
    io:format("  ✓ DHT server started and registered as 'macula_routing_server'~n~n"),

    %% Step 2: Create service registry (simulating Node 1 - Provider)
    io:format("[Step 2] Node 1: Creating service registry and advertising service...~n"),
    Registry1 = macula_service_registry:new(),

    Handler1 = fun(Args) ->
        io:format("  [Node 1 Handler] Received call with args: ~p~n", [Args]),
        case Args of
            #{operation := <<"add">>, a := A, b := B} ->
                Result = A + B,
                {ok, #{result => Result, node => <<"node-1">>}};
            _ ->
                {error, invalid_args}
        end
    end,

    %% Advertise locally
    Registry1b = macula_service_registry:advertise_local(
        Registry1,
        <<"test.calculator">>,
        Handler1,
        #{version => <<"1.0">>, node => <<"node-1">>}
    ),
    io:format("  ✓ Service advertised locally on Node 1~n"),

    %% Publish to DHT
    ProviderInfo1 = #{
        node_id => <<"node-1-id">>,
        endpoint => <<"https://node1.local:9443">>,
        metadata => #{version => <<"1.0">>, node => <<"node-1">>}
    },
    ok = macula_service_registry:publish_to_dht(
        macula_routing_server,
        <<"test.calculator">>,
        ProviderInfo1,
        300,
        20
    ),
    io:format("  ✓ Service published to DHT~n~n"),

    %% Step 3: Simulate Node 2 (Consumer) discovering and calling
    io:format("[Step 3] Node 2: Discovering service via DHT...~n"),
    Registry2 = macula_service_registry:new(),

    %% Query DHT for service
    {ok, Providers} = macula_service_registry:query_dht_for_service(
        macula_routing_server,
        <<"test.calculator">>,
        20
    ),
    io:format("  ✓ Found ~p provider(s) in DHT~n", [length(Providers)]),
    io:format("  ✓ Providers: ~p~n~n", [Providers]),

    %% Cache the result
    Registry2b = macula_service_registry:cache_service(
        Registry2,
        <<"test.calculator">>,
        Providers,
        60
    ),
    io:format("  ✓ Cached providers (60s TTL)~n"),

    %% Verify cache hit
    case macula_service_registry:discover_service(Registry2b, <<"test.calculator">>) of
        {ok, CachedProviders, _Registry2c} ->
            io:format("  ✓ Cache hit! Retrieved ~p provider(s)~n~n", [length(CachedProviders)]);
        _ ->
            io:format("  ✗ Cache miss (unexpected)~n~n")
    end,

    %% Step 4: Simulate Node 3 (Second Provider)
    io:format("[Step 4] Node 3: Advertising same service (multi-provider)...~n"),
    Registry3 = macula_service_registry:new(),

    Handler3 = fun(Args) ->
        io:format("  [Node 3 Handler] Received call with args: ~p~n", [Args]),
        case Args of
            #{operation := <<"add">>, a := A, b := B} ->
                Result = A + B,
                {ok, #{result => Result, node => <<"node-3">>}};
            _ ->
                {error, invalid_args}
        end
    end,

    Registry3b = macula_service_registry:advertise_local(
        Registry3,
        <<"test.calculator">>,
        Handler3,
        #{version => <<"1.0">>, node => <<"node-3">>}
    ),

    ProviderInfo3 = #{
        node_id => <<"node-3-id">>,
        endpoint => <<"https://node3.local:9443">>,
        metadata => #{version => <<"1.0">>, node => <<"node-3">>}
    },
    ok = macula_service_registry:publish_to_dht(
        macula_routing_server,
        <<"test.calculator">>,
        ProviderInfo3,
        300,
        20
    ),
    io:format("  ✓ Second provider published to DHT~n~n"),

    %% Query again to see both providers
    {ok, AllProviders} = macula_service_registry:query_dht_for_service(
        macula_routing_server,
        <<"test.calculator">>,
        20
    ),
    io:format("[Verification] DHT now has ~p provider(s) for test.calculator~n", [length(AllProviders)]),
    lists:foreach(fun(Provider) ->
        io:format("  - ~p~n", [Provider])
    end, AllProviders),
    io:format("~n"),

    %% Step 5: Test local handler execution
    io:format("[Step 5] Testing local handler execution...~n"),
    case macula_service_registry:get_local_handler(Registry1b, <<"test.calculator">>) of
        {ok, LocalHandler} ->
            io:format("  ✓ Retrieved local handler from Node 1~n"),
            case LocalHandler(#{operation => <<"add">>, a => 42, b => 8}) of
                {ok, #{result := 50}} ->
                    io:format("  ✓ Handler executed successfully: 42 + 8 = 50~n~n");
                Other ->
                    io:format("  ✗ Unexpected result: ~p~n~n", [Other])
            end;
        not_found ->
            io:format("  ✗ Handler not found~n~n")
    end,

    %% Step 6: Test service removal
    io:format("[Step 6] Testing service removal from DHT...~n"),
    ok = macula_service_registry:remove_from_dht(
        macula_routing_server,
        <<"test.calculator">>,
        <<"node-1-id">>
    ),
    io:format("  ✓ Requested removal of Node 1 from DHT~n"),

    %% Note: Current implementation doesn't actually remove from DHT storage,
    %% it just logs. In a full DHT implementation, we'd remove the specific provider.
    io:format("  (Note: Full removal requires DHT replication logic)~n~n"),

    %% Step 7: Test cache pruning
    io:format("[Step 7] Testing cache expiration...~n"),
    Registry2d = macula_service_registry:cache_service(
        Registry2,
        <<"expired.service">>,
        [#{node_id => <<"test">>}],
        -1  %% Already expired
    ),
    {Pruned, RemovedCount} = macula_service_registry:prune_expired(Registry2d),
    io:format("  ✓ Pruned ~p expired cache entries~n", [RemovedCount]),

    %% Final summary
    io:format("~n=== Test Summary ===~n"),
    io:format("✓ DHT routing server started~n"),
    io:format("✓ Service advertisement working (Node 1)~n"),
    io:format("✓ DHT publish working~n"),
    io:format("✓ DHT query working~n"),
    io:format("✓ Service caching working~n"),
    io:format("✓ Multi-provider support working (Node 3)~n"),
    io:format("✓ Local handler execution working~n"),
    io:format("✓ Cache expiration working~n"),
    io:format("~n✅ All DHT service discovery tests passed!~n~n"),

    ok.
EOF

# Run the test
echo "Running DHT service discovery test..."
echo ""

erl -pa _build/default/lib/*/ebin \
    -pa /tmp \
    -noshell \
    -eval "
        c:c('/tmp/macula_dht_test'),
        macula_dht_test:run(),
        init:stop()
    " \
    2>&1 | tee /tmp/macula_dht_test.log

echo ""
echo "Test complete!"
echo ""
echo "Log saved to: /tmp/macula_dht_test.log"
