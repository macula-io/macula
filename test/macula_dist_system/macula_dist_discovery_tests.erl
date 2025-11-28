%%%-------------------------------------------------------------------
%%% @doc Unit tests for macula_dist_discovery module.
%%%
%%% Tests the decentralized node discovery functionality.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_dist_discovery_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    %% Start the discovery server
    {ok, Pid} = macula_dist_discovery:start_link(#{
        discovery_type => both
    }),
    Pid.

cleanup(Pid) ->
    %% Stop the discovery server
    catch gen_server:stop(Pid),
    %% Clean up ETS table if it exists
    catch ets:delete(macula_dist_discovery_cache),
    ok.

%%%===================================================================
%%% Test Generator
%%%===================================================================

discovery_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(Pid) ->
            {inorder, [
                {"Register node", register_node_test(Pid)},
                {"Lookup registered node", lookup_node_test(Pid)},
                {"List nodes", list_nodes_test(Pid)},
                {"Subscribe to events", subscribe_test(Pid)},
                {"Unregister node", unregister_node_test(Pid)},
                {"Lookup unregistered node returns not_found", lookup_not_found_test(Pid)}
            ]}
        end
    }.

%%%===================================================================
%%% Individual Tests
%%%===================================================================

register_node_test(_Pid) ->
    ?_test(begin
        NodeName = '4433@127.0.0.1',
        Port = 4433,
        Result = macula_dist_discovery:register_node(NodeName, Port),
        ?assertEqual(ok, Result)
    end).

lookup_node_test(_Pid) ->
    ?_test(begin
        NodeName = '4433@127.0.0.1',
        case macula_dist_discovery:lookup_node(NodeName) of
            {ok, NodeInfo} ->
                ?assertMatch(#{port := 4433}, NodeInfo);
            {error, not_found} ->
                %% This is acceptable if DHT isn't running
                ok
        end
    end).

list_nodes_test(_Pid) ->
    ?_test(begin
        Nodes = macula_dist_discovery:list_nodes(),
        ?assert(is_list(Nodes))
    end).

subscribe_test(_Pid) ->
    ?_test(begin
        Result = macula_dist_discovery:subscribe(self()),
        ?assertEqual(ok, Result),
        %% Cleanup
        macula_dist_discovery:unsubscribe(self())
    end).

unregister_node_test(_Pid) ->
    ?_test(begin
        NodeName = '4433@127.0.0.1',
        Result = macula_dist_discovery:unregister_node(NodeName),
        ?assertEqual(ok, Result)
    end).

lookup_not_found_test(_Pid) ->
    ?_test(begin
        NodeName = 'nonexistent@nowhere',
        Result = macula_dist_discovery:lookup_node(NodeName, 100),
        ?assertEqual({error, not_found}, Result)
    end).

%%%===================================================================
%%% Standalone Tests (no fixture needed)
%%%===================================================================

local_cache_test_() ->
    {"Local cache fallback tests", [
        ?_test(begin
            %% Ensure cache exists
            case ets:info(macula_dist_discovery_cache) of
                undefined ->
                    ets:new(macula_dist_discovery_cache, [
                        named_table, public, set, {read_concurrency, true}
                    ]);
                _ -> ok
            end,

            %% Store a value
            NodeName = 'test_node@localhost',
            NodeInfo = #{port => 9999, host => "localhost"},
            ets:insert(macula_dist_discovery_cache, {NodeName, NodeInfo}),

            %% Verify we can read it
            ?assertEqual([{NodeName, NodeInfo}], ets:lookup(macula_dist_discovery_cache, NodeName)),

            %% Cleanup
            ets:delete(macula_dist_discovery_cache, NodeName)
        end)
    ]}.
