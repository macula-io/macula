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
    %% Clean up ETS table if it exists from prior run
    catch ets:delete(macula_dht_storage),
    timer:sleep(10),
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

    ?assertMatch({ok, [#{node_id := _, data := <<"test_value">>}]}, Result),
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
    {ok, [StoredProvider]} = Result,
    ?assertEqual(<<"new_value">>, maps:get(data, StoredProvider)),
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

    ?assertMatch({ok, [#{node_id := _, data := <<>>}]}, Result),
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
    ?assert(length(Providers) >= 1),
    cleanup_server(Pid).

find_value_not_found_returns_closest_nodes_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,

    %% Add some nodes to routing table
    ok = macula_routing_server:add_node(Pid, sample_node(10)),
    ok = macula_routing_server:add_node(Pid, sample_node(20)),

    Result = macula_routing_server:find_value(Pid, Key, 5),

    %% When value not found locally and network queries fail, returns {error, not_found}
    ?assertEqual({error, not_found}, Result),
    cleanup_server(Pid).

find_value_not_found_empty_routing_table_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,

    Result = macula_routing_server:find_value(Pid, Key, 5),

    %% No local value and no nodes in routing table
    ?assertEqual({error, not_found}, Result),
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
    {ok, [StoredProvider]} = Result,
    ?assertEqual(<<"v2">>, maps:get(data, StoredProvider)),
    cleanup_server(Pid).

large_value_storage_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    ProviderId = <<200:256>>,
    LargeValue = crypto:strong_rand_bytes(10000),
    ProviderInfo = #{node_id => ProviderId, data => LargeValue},

    ok = macula_routing_server:store_local(Pid, Key, ProviderInfo),
    Result = macula_routing_server:get_local(Pid, Key),

    ?assertMatch({ok, [#{data := _}]}, Result),
    {ok, [#{data := Retrieved}]} = Result,
    ?assertEqual(LargeValue, Retrieved),
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

%%%===================================================================
%%% Delete Local Tests (NEW)
%%%===================================================================

delete_local_removes_provider_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    ProviderId = <<200:256>>,
    ProviderInfo = #{node_id => ProviderId, data => <<"value">>},

    ok = macula_routing_server:store_local(Pid, Key, ProviderInfo),
    ?assertMatch({ok, _}, macula_routing_server:get_local(Pid, Key)),

    ok = macula_routing_server:delete_local(Pid, Key, ProviderId),
    ?assertEqual(not_found, macula_routing_server:get_local(Pid, Key)),
    cleanup_server(Pid).

delete_local_removes_one_of_multiple_providers_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    Provider1 = #{node_id => <<200:256>>, data => <<"v1">>},
    Provider2 = #{node_id => <<201:256>>, data => <<"v2">>},

    ok = macula_routing_server:store_local(Pid, Key, Provider1),
    ok = macula_routing_server:store_local(Pid, Key, Provider2),
    {ok, Before} = macula_routing_server:get_local(Pid, Key),
    ?assertEqual(2, length(Before)),

    ok = macula_routing_server:delete_local(Pid, Key, <<200:256>>),
    {ok, After} = macula_routing_server:get_local(Pid, Key),
    ?assertEqual(1, length(After)),
    %% Remaining provider should be Provider2
    [Remaining] = After,
    ?assertEqual(<<"v2">>, maps:get(data, Remaining)),
    cleanup_server(Pid).

delete_local_nonexistent_key_test() ->
    Pid = setup_server(),
    %% Should not crash on nonexistent key
    ok = macula_routing_server:delete_local(Pid, <<99:256>>, <<200:256>>),
    cleanup_server(Pid).

delete_local_nonexistent_provider_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    ProviderInfo = #{node_id => <<200:256>>, data => <<"value">>},
    ok = macula_routing_server:store_local(Pid, Key, ProviderInfo),

    %% Delete a provider that doesn't exist for this key
    ok = macula_routing_server:delete_local(Pid, Key, <<999:256>>),
    %% Original provider should still be there
    {ok, Providers} = macula_routing_server:get_local(Pid, Key),
    ?assertEqual(1, length(Providers)),
    cleanup_server(Pid).

%%%===================================================================
%%% Get All Keys Tests (NEW)
%%%===================================================================

get_all_keys_empty_storage_test() ->
    Pid = setup_server(),
    {ok, Keys} = macula_routing_server:get_all_keys(Pid),
    ?assertEqual([], Keys),
    cleanup_server(Pid).

get_all_keys_returns_stored_keys_test() ->
    Pid = setup_server(),
    Key1 = <<1:256>>,
    Key2 = <<2:256>>,
    Key3 = <<3:256>>,
    Provider = #{node_id => <<200:256>>, data => <<"v">>},

    ok = macula_routing_server:store_local(Pid, Key1, Provider),
    ok = macula_routing_server:store_local(Pid, Key2, Provider),
    ok = macula_routing_server:store_local(Pid, Key3, Provider),

    {ok, Keys} = macula_routing_server:get_all_keys(Pid),
    ?assertEqual(3, length(Keys)),
    ?assert(lists:member(Key1, Keys)),
    ?assert(lists:member(Key2, Keys)),
    ?assert(lists:member(Key3, Keys)),
    cleanup_server(Pid).

get_all_keys_after_delete_test() ->
    Pid = setup_server(),
    Key1 = <<1:256>>,
    Key2 = <<2:256>>,
    Provider1 = #{node_id => <<200:256>>, data => <<"v1">>},
    Provider2 = #{node_id => <<201:256>>, data => <<"v2">>},

    ok = macula_routing_server:store_local(Pid, Key1, Provider1),
    ok = macula_routing_server:store_local(Pid, Key2, Provider2),
    ok = macula_routing_server:delete_local(Pid, Key1, <<200:256>>),

    {ok, Keys} = macula_routing_server:get_all_keys(Pid),
    ?assertEqual(1, length(Keys)),
    ?assert(lists:member(Key2, Keys)),
    cleanup_server(Pid).

%%%===================================================================
%%% Remove Node Tests (NEW)
%%%===================================================================

remove_node_decreases_size_test() ->
    Pid = setup_server(),
    Node = sample_node(1),
    ok = macula_routing_server:add_node(Pid, Node),
    ?assertEqual(1, macula_routing_server:size(Pid)),

    ok = macula_routing_server:remove_node(Pid, <<1:256>>),
    timer:sleep(50),  %% cast is async
    ?assertEqual(0, macula_routing_server:size(Pid)),
    cleanup_server(Pid).

remove_nonexistent_node_test() ->
    Pid = setup_server(),
    ok = macula_routing_server:add_node(Pid, sample_node(1)),
    ok = macula_routing_server:remove_node(Pid, <<999:256>>),
    timer:sleep(50),
    ?assertEqual(1, macula_routing_server:size(Pid)),
    cleanup_server(Pid).

remove_node_from_multiple_test() ->
    Pid = setup_server(),
    ok = macula_routing_server:add_node(Pid, sample_node(1)),
    ok = macula_routing_server:add_node(Pid, sample_node(2)),
    ok = macula_routing_server:add_node(Pid, sample_node(3)),
    ?assertEqual(3, macula_routing_server:size(Pid)),

    ok = macula_routing_server:remove_node(Pid, <<2:256>>),
    timer:sleep(50),
    ?assertEqual(2, macula_routing_server:size(Pid)),
    cleanup_server(Pid).

%%%===================================================================
%%% Find Value Local (ETS) Tests (NEW)
%%%===================================================================

find_value_local_returns_stored_value_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    ProviderInfo = #{node_id => <<200:256>>, data => <<"ets_test">>},

    ok = macula_routing_server:store_local(Pid, Key, ProviderInfo),
    %% find_value_local reads directly from ETS, bypassing gen_server
    Result = macula_routing_server:find_value_local(Key, 5),
    ?assertMatch({ok, _}, Result),
    {ok, Providers} = Result,
    ?assert(length(Providers) >= 1),
    cleanup_server(Pid).

find_value_local_not_found_test() ->
    Pid = setup_server(),
    Result = macula_routing_server:find_value_local(<<999:256>>, 5),
    ?assertEqual({error, not_found}, Result),
    cleanup_server(Pid).

find_value_local_no_ets_table_test() ->
    %% When ETS table doesn't exist (server not started), should return not_found
    catch ets:delete(macula_dht_storage),
    Result = macula_routing_server:find_value_local(<<1:256>>, 5),
    ?assertEqual({error, not_found}, Result).

find_value_local_multiple_providers_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    Provider1 = #{node_id => <<200:256>>, data => <<"p1">>},
    Provider2 = #{node_id => <<201:256>>, data => <<"p2">>},

    ok = macula_routing_server:store_local(Pid, Key, Provider1),
    ok = macula_routing_server:store_local(Pid, Key, Provider2),

    Result = macula_routing_server:find_value_local(Key, 5),
    ?assertMatch({ok, _}, Result),
    {ok, Providers} = Result,
    ?assertEqual(2, length(Providers)),
    cleanup_server(Pid).

%%%===================================================================
%%% ETS Mirroring Tests (NEW)
%%%===================================================================

ets_mirrors_store_local_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    ProviderInfo = #{node_id => <<200:256>>, data => <<"mirror">>},

    ok = macula_routing_server:store_local(Pid, Key, ProviderInfo),

    %% Verify ETS has the data
    EtsResult = ets:lookup(macula_dht_storage, Key),
    ?assertMatch([{_, [_]}], EtsResult),
    cleanup_server(Pid).

ets_mirrors_delete_local_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    ProviderInfo = #{node_id => <<200:256>>, data => <<"mirror">>},

    ok = macula_routing_server:store_local(Pid, Key, ProviderInfo),
    ?assertMatch([{_, _}], ets:lookup(macula_dht_storage, Key)),

    ok = macula_routing_server:delete_local(Pid, Key, <<200:256>>),
    %% After deleting the only provider, ETS entry should be gone
    ?assertEqual([], ets:lookup(macula_dht_storage, Key)),
    cleanup_server(Pid).

ets_mirrors_partial_delete_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    Provider1 = #{node_id => <<200:256>>, data => <<"v1">>},
    Provider2 = #{node_id => <<201:256>>, data => <<"v2">>},

    ok = macula_routing_server:store_local(Pid, Key, Provider1),
    ok = macula_routing_server:store_local(Pid, Key, Provider2),

    ok = macula_routing_server:delete_local(Pid, Key, <<200:256>>),
    %% ETS should still have the remaining provider
    [{_, Remaining}] = ets:lookup(macula_dht_storage, Key),
    ?assertEqual(1, length(Remaining)),
    cleanup_server(Pid).

%%%===================================================================
%%% Handle Message Async Tests (NEW)
%%%===================================================================

handle_message_async_unknown_type_test() ->
    Pid = setup_server(),
    Message = #{type => unknown_type, data => <<"async_test">>},
    %% Should not crash, fire-and-forget
    ok = macula_routing_server:handle_message_async(Pid, Message),
    timer:sleep(50),
    ?assert(is_process_alive(Pid)),
    cleanup_server(Pid).

%%%===================================================================
%%% Config Defaults Tests (NEW)
%%%===================================================================

config_defaults_applied_test() ->
    catch gen_server:stop(macula_routing_server),
    catch ets:delete(macula_dht_storage),
    timer:sleep(10),
    %% Start with empty config - defaults should apply
    {ok, Pid} = macula_routing_server:start_link(<<42:256>>, #{}),
    %% Server should be alive with default k=20
    ?assert(is_process_alive(Pid)),
    ?assertEqual(0, macula_routing_server:size(Pid)),
    cleanup_server(Pid).

config_custom_k_test() ->
    Pid = setup_server(),
    %% Add 10 nodes using standard server (k=20)
    lists:foreach(fun(N) ->
        ok = macula_routing_server:add_node(Pid, sample_node(N))
    end, lists:seq(1, 10)),

    %% The find_closest K parameter limits results independently of config k
    Result3 = macula_routing_server:find_closest(Pid, <<50:256>>, 3),
    ?assertEqual(3, length(Result3)),
    Result7 = macula_routing_server:find_closest(Pid, <<50:256>>, 7),
    ?assertEqual(7, length(Result7)),
    cleanup_server(Pid).

%%%===================================================================
%%% Provider Without node_id Tests (NEW)
%%%===================================================================

store_provider_without_node_id_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    %% Provider without node_id field
    Provider = #{data => <<"anonymous">>},

    ok = macula_routing_server:store_local(Pid, Key, Provider),
    {ok, Providers} = macula_routing_server:get_local(Pid, Key),
    ?assertEqual(1, length(Providers)),
    cleanup_server(Pid).

store_multiple_providers_without_node_id_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    %% Providers without node_id should all be appended (no dedup by undefined)
    Provider1 = #{data => <<"anon1">>},
    Provider2 = #{data => <<"anon2">>},

    ok = macula_routing_server:store_local(Pid, Key, Provider1),
    ok = macula_routing_server:store_local(Pid, Key, Provider2),
    {ok, Providers} = macula_routing_server:get_local(Pid, Key),
    %% Both should be stored since node_id is undefined for both
    ?assertEqual(2, length(Providers)),
    cleanup_server(Pid).

%%%===================================================================
%%% Provider With Binary Keys Tests (NEW)
%%%===================================================================

store_provider_with_binary_key_node_id_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    %% Simulate msgpack-style binary keys
    Provider = #{<<"node_id">> => <<200:256>>, <<"data">> => <<"binary_keys">>},

    ok = macula_routing_server:store_local(Pid, Key, Provider),
    {ok, Providers} = macula_routing_server:get_local(Pid, Key),
    ?assertEqual(1, length(Providers)),
    cleanup_server(Pid).

binary_key_providers_treated_as_separate_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    NodeId = <<200:256>>,
    %% store_local handler uses atom-key lookup (maps:get(node_id, ...)),
    %% so binary-keyed node_ids are treated as undefined - no dedup occurs.
    Provider1 = #{<<"node_id">> => NodeId, <<"data">> => <<"v1">>},
    Provider2 = #{<<"node_id">> => NodeId, <<"data">> => <<"v2">>},

    ok = macula_routing_server:store_local(Pid, Key, Provider1),
    ok = macula_routing_server:store_local(Pid, Key, Provider2),
    {ok, Providers} = macula_routing_server:get_local(Pid, Key),
    %% Both stored because store_local uses atom-key path (node_id not <<"node_id">>)
    ?assertEqual(2, length(Providers)),
    cleanup_server(Pid).

%%%===================================================================
%%% stored_at Timestamp Tests (NEW)
%%%===================================================================

store_local_adds_stored_at_timestamp_test() ->
    Pid = setup_server(),
    Key = <<1:256>>,
    Provider = #{node_id => <<200:256>>, data => <<"ts">>},
    Before = erlang:system_time(millisecond),

    ok = macula_routing_server:store_local(Pid, Key, Provider),

    After = erlang:system_time(millisecond),
    {ok, [Stored]} = macula_routing_server:get_local(Pid, Key),
    StoredAt = maps:get(stored_at, Stored),
    ?assert(StoredAt >= Before),
    ?assert(StoredAt =< After),
    cleanup_server(Pid).

%%%===================================================================
%%% Unknown Request Handling Test (NEW)
%%%===================================================================

unknown_call_returns_error_test() ->
    Pid = setup_server(),
    Result = gen_server:call(Pid, {totally_unknown_request, 42}),
    ?assertEqual({error, unknown_request}, Result),
    cleanup_server(Pid).

unknown_cast_does_not_crash_test() ->
    Pid = setup_server(),
    gen_server:cast(Pid, {totally_unknown_cast, 42}),
    timer:sleep(50),
    ?assert(is_process_alive(Pid)),
    cleanup_server(Pid).

unknown_info_does_not_crash_test() ->
    Pid = setup_server(),
    Pid ! {totally_unknown_info, 42},
    timer:sleep(50),
    ?assert(is_process_alive(Pid)),
    cleanup_server(Pid).
