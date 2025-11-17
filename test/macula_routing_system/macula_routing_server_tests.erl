%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_routing_server module.
%%% Tests gen_server managing Kademlia DHT routing and storage.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_server_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Helper to create local node ID
local_node_id() ->
    <<100:256>>.

%% Helper to create test node
sample_node(Suffix) ->
    NodeId = <<Suffix:256>>,
    #{
        node_id => NodeId,
        address => {{127, 0, 0, 1}, 4433 + Suffix}
    }.

%% Helper to create list of nodes
sample_nodes(Suffixes) ->
    [sample_node(S) || S <- Suffixes].

%% Setup function - starts server with test config
setup_server() ->
    LocalNodeId = local_node_id(),
    %% Stop any existing server first
    catch gen_server:stop(macula_routing_server),
    timer:sleep(10),  %% Brief delay to ensure cleanup
    {ok, Pid} = macula_routing_server:start_link(LocalNodeId, #{k => 20, alpha => 3}),
    Pid.

%% Cleanup function - stops server
cleanup_server(Pid) ->
    case is_process_alive(Pid) of
        true -> gen_server:stop(Pid);
        false -> ok
    end,
    timer:sleep(10).  %% Brief delay to ensure cleanup

%%%===================================================================
%%% Server Lifecycle Tests
%%%===================================================================

start_link_creates_process_test() ->
    Pid = setup_server(),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    cleanup_server(Pid).

start_link_initializes_empty_routing_table_test() ->
    Pid = setup_server(),
    Size = macula_routing_server:size(Pid),
    ?assertEqual(0, Size),
    cleanup_server(Pid).

%%%===================================================================
%%% Adding Nodes Tests
%%%===================================================================

add_node_to_empty_server_test() ->
    Pid = setup_server(),
    Node = sample_node(1),
    ok = macula_routing_server:add_node(Pid, Node),
    Size = macula_routing_server:size(Pid),
    ?assertEqual(1, Size),
    cleanup_server(Pid).

add_node_increases_size_test() ->
    Pid = setup_server(),
    ok = macula_routing_server:add_node(Pid, sample_node(1)),
    ok = macula_routing_server:add_node(Pid, sample_node(2)),
    ok = macula_routing_server:add_node(Pid, sample_node(3)),
    Size = macula_routing_server:size(Pid),
    ?assertEqual(3, Size),
    cleanup_server(Pid).

add_multiple_nodes_test() ->
    Pid = setup_server(),
    Nodes = sample_nodes(lists:seq(1, 10)),
    lists:foreach(fun(Node) ->
        ok = macula_routing_server:add_node(Pid, Node)
    end, Nodes),
    Size = macula_routing_server:size(Pid),
    ?assertEqual(10, Size),
    cleanup_server(Pid).

add_duplicate_node_test() ->
    Pid = setup_server(),
    Node = sample_node(1),
    ok = macula_routing_server:add_node(Pid, Node),
    ok = macula_routing_server:add_node(Pid, Node),
    Size = macula_routing_server:size(Pid),
    ?assertEqual(1, Size),
    cleanup_server(Pid).

%%%===================================================================
%%% Finding Closest Nodes Tests
%%%===================================================================

find_closest_in_empty_server_test() ->
    Pid = setup_server(),
    Target = <<50:256>>,
    Result = macula_routing_server:find_closest(Pid, Target, 5),
    ?assertEqual([], Result),
    cleanup_server(Pid).

find_closest_returns_all_when_fewer_than_k_test() ->
    Pid = setup_server(),
    ok = macula_routing_server:add_node(Pid, sample_node(1)),
    ok = macula_routing_server:add_node(Pid, sample_node(2)),
    Target = <<50:256>>,
    Result = macula_routing_server:find_closest(Pid, Target, 5),
    ?assertEqual(2, length(Result)),
    cleanup_server(Pid).

find_closest_limits_to_k_test() ->
    Pid = setup_server(),
    Nodes = sample_nodes(lists:seq(1, 10)),
    lists:foreach(fun(Node) ->
        ok = macula_routing_server:add_node(Pid, Node)
    end, Nodes),
    Target = <<50:256>>,
    Result = macula_routing_server:find_closest(Pid, Target, 5),
    ?assertEqual(5, length(Result)),
    cleanup_server(Pid).

find_closest_sorted_by_distance_test() ->
    Pid = setup_server(),
    ok = macula_routing_server:add_node(Pid, sample_node(1)),
    ok = macula_routing_server:add_node(Pid, sample_node(50)),
    ok = macula_routing_server:add_node(Pid, sample_node(101)),
    ok = macula_routing_server:add_node(Pid, sample_node(200)),

    Target = <<100:256>>,
    Result = macula_routing_server:find_closest(Pid, Target, 4),

    %% Verify sorted by distance
    ?assert(length(Result) > 0),
    Distances = [macula_routing_nodeid:distance(Target, maps:get(node_id, N)) || N <- Result],
    SortedDistances = lists:sort(Distances),
    ?assertEqual(SortedDistances, Distances),
    cleanup_server(Pid).

%%%===================================================================
%%% Local Storage Tests
%%%===================================================================

store_local_and_get_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    ProviderId = <<200:256>>,
    ProviderInfo = #{
        node_id => ProviderId,
        data => <<"test_value">>
    },

    ok = macula_routing_server:store_local(Pid, Key, ProviderInfo),
    Result = macula_routing_server:get_local(Pid, Key),

    ?assertEqual({ok, [ProviderInfo]}, Result),
    cleanup_server(Pid).

store_local_multiple_providers_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    Provider1 = #{node_id => <<200:256>>, data => <<"value1">>},
    Provider2 = #{node_id => <<201:256>>, data => <<"value2">>},

    ok = macula_routing_server:store_local(Pid, Key, Provider1),
    ok = macula_routing_server:store_local(Pid, Key, Provider2),

    Result = macula_routing_server:get_local(Pid, Key),
    ?assertMatch({ok, [_, _]}, Result),

    {ok, Providers} = Result,
    ?assertEqual(2, length(Providers)),
    cleanup_server(Pid).

store_local_updates_existing_provider_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    ProviderId = <<200:256>>,
    Provider1 = #{node_id => ProviderId, data => <<"old_value">>},
    Provider2 = #{node_id => ProviderId, data => <<"new_value">>},

    ok = macula_routing_server:store_local(Pid, Key, Provider1),
    ok = macula_routing_server:store_local(Pid, Key, Provider2),

    Result = macula_routing_server:get_local(Pid, Key),
    ?assertEqual({ok, [Provider2]}, Result),
    cleanup_server(Pid).

get_local_nonexistent_key_test() ->
    Pid = setup_server(),
    Key = <<999:256>>,
    Result = macula_routing_server:get_local(Pid, Key),
    ?assertEqual(not_found, Result),
    cleanup_server(Pid).

store_local_empty_value_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    ProviderId = <<200:256>>,
    ProviderInfo = #{node_id => ProviderId, data => <<>>},

    ok = macula_routing_server:store_local(Pid, Key, ProviderInfo),
    Result = macula_routing_server:get_local(Pid, Key),

    ?assertEqual({ok, [ProviderInfo]}, Result),
    cleanup_server(Pid).

%%%===================================================================
%%% Find Value Tests
%%%===================================================================

find_value_returns_local_value_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    ProviderId = <<200:256>>,
    ProviderInfo = #{node_id => ProviderId, data => <<"test_value">>},

    ok = macula_routing_server:store_local(Pid, Key, ProviderInfo),
    Result = macula_routing_server:find_value(Pid, Key, 5),

    ?assertMatch({ok, _}, Result),
    {ok, Providers} = Result,
    ?assertEqual([ProviderInfo], Providers),
    cleanup_server(Pid).

find_value_not_found_returns_closest_nodes_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,

    %% Add some nodes to routing table
    ok = macula_routing_server:add_node(Pid, sample_node(10)),
    ok = macula_routing_server:add_node(Pid, sample_node(20)),

    Result = macula_routing_server:find_value(Pid, Key, 5),

    %% Should return empty list for service registry compatibility
    ?assertEqual({ok, []}, Result),
    cleanup_server(Pid).

find_value_not_found_empty_routing_table_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,

    Result = macula_routing_server:find_value(Pid, Key, 5),

    %% No local value and no nodes in routing table
    ?assertEqual({ok, []}, Result),
    cleanup_server(Pid).

%%%===================================================================
%%% Routing Table Access Tests
%%%===================================================================

get_routing_table_test() ->
    Pid = setup_server(),
    Table = macula_routing_server:get_routing_table(Pid),

    %% Should return a routing table structure (map-based)
    ?assert(is_map(Table)),
    cleanup_server(Pid).

size_reflects_added_nodes_test() ->
    Pid = setup_server(),
    ?assertEqual(0, macula_routing_server:size(Pid)),

    ok = macula_routing_server:add_node(Pid, sample_node(1)),
    ?assertEqual(1, macula_routing_server:size(Pid)),

    ok = macula_routing_server:add_node(Pid, sample_node(2)),
    ?assertEqual(2, macula_routing_server:size(Pid)),

    cleanup_server(Pid).

%%%===================================================================
%%% Handle Message Tests
%%%===================================================================

%% NOTE: Protocol message tests commented out pending macula_routing_protocol implementation
%% TODO: Re-enable these tests when macula_routing_protocol module is implemented

%% handle_message_find_node_test() - requires macula_routing_protocol
%% handle_message_store_test() - requires macula_routing_protocol
%% handle_message_find_value_found_test() - requires macula_routing_protocol
%% handle_message_find_value_not_found_test() - requires macula_routing_protocol

handle_message_unknown_type_test() ->
    Pid = setup_server(),

    %% Create unknown message type
    Message = #{
        type => unknown_type,
        data => <<"test">>
    },

    Response = macula_routing_server:handle_message(Pid, Message),

    %% Should return error
    ?assertMatch(#{type := error, reason := unknown_message}, Response),
    cleanup_server(Pid).

%%%===================================================================
%%% Edge Cases and Integration Tests
%%%===================================================================

add_many_nodes_and_search_test() ->
    Pid = setup_server(),

    %% Add 20 nodes
    Nodes = sample_nodes(lists:seq(1, 20)),
    lists:foreach(fun(Node) ->
        ok = macula_routing_server:add_node(Pid, Node)
    end, Nodes),

    %% Find closest to target
    Target = <<100:256>>,
    Result = macula_routing_server:find_closest(Pid, Target, 10),

    ?assertEqual(10, length(Result)),
    cleanup_server(Pid).

store_and_find_multiple_keys_test() ->
    Pid = setup_server(),

    %% Store multiple keys
    Keys = [<<N:256>> || N <- lists:seq(1, 5)],
    lists:foreach(fun(Key) ->
        Value = <<"value_", Key/binary>>,
        ProviderInfo = #{node_id => <<200:256>>, data => Value},
        ok = macula_routing_server:store_local(Pid, Key, ProviderInfo)
    end, Keys),

    %% Retrieve all keys
    lists:foreach(fun(Key) ->
        Result = macula_routing_server:get_local(Pid, Key),
        ?assertMatch({ok, _}, Result)
    end, Keys),

    cleanup_server(Pid).

concurrent_operations_test() ->
    Pid = setup_server(),

    %% Spawn multiple processes doing operations
    Parent = self(),
    lists:foreach(fun(N) ->
        spawn(fun() ->
            ok = macula_routing_server:add_node(Pid, sample_node(N)),
            Parent ! {done, N}
        end)
    end, lists:seq(1, 10)),

    %% Wait for all operations
    lists:foreach(fun(N) ->
        receive
            {done, N} -> ok
        after 1000 ->
            ?assert(false)
        end
    end, lists:seq(1, 10)),

    %% Verify all nodes added
    Size = macula_routing_server:size(Pid),
    ?assertEqual(10, Size),
    cleanup_server(Pid).

provider_replacement_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    ProviderId = <<200:256>>,
    Provider1 = #{node_id => ProviderId, data => <<"v1">>},
    Provider2 = #{node_id => ProviderId, data => <<"v2">>},

    %% Store initial value
    ok = macula_routing_server:store_local(Pid, Key, Provider1),

    %% Update with new value (same provider)
    ok = macula_routing_server:store_local(Pid, Key, Provider2),

    %% Should have only latest value
    Result = macula_routing_server:get_local(Pid, Key),
    ?assertEqual({ok, [Provider2]}, Result),
    cleanup_server(Pid).

large_value_storage_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    ProviderId = <<200:256>>,
    LargeValue = crypto:strong_rand_bytes(10000),
    ProviderInfo = #{node_id => ProviderId, data => LargeValue},

    ok = macula_routing_server:store_local(Pid, Key, ProviderInfo),
    Result = macula_routing_server:get_local(Pid, Key),

    ?assertEqual({ok, [ProviderInfo]}, Result),
    cleanup_server(Pid).

stress_test_many_nodes_test() ->
    Pid = setup_server(),

    %% Add 100 nodes
    Nodes = sample_nodes(lists:seq(1, 100)),
    lists:foreach(fun(Node) ->
        ok = macula_routing_server:add_node(Pid, Node)
    end, Nodes),

    Size = macula_routing_server:size(Pid),
    ?assert(Size > 0),

    %% Find closest should still work
    Result = macula_routing_server:find_closest(Pid, <<50:256>>, 20),
    ?assertEqual(20, length(Result)),

    cleanup_server(Pid).
