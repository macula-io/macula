-module(macula_platform_api_tests).
-include_lib("eunit/eunit.hrl").

-export([test_register_workload/0, test_get_leader/0, test_crdt_operations/0]).

%%==============================================================================
%% Platform Layer API Tests (v0.10.0)
%%==============================================================================

platform_layer_api_test_() ->
    [
         fun test_register_workload/0,
         fun test_get_leader/0,
         fun test_crdt_operations/0
     ].

%%==============================================================================
%% Individual Tests
%%==============================================================================

test_register_workload() ->
    %% Connect to local platform
    {ok, Client} = macula:connect_local(#{realm => <<"test.platform.api">>}),

    %% Register workload
    {ok, Info} = macula:register_workload(Client, #{
        workload_name => <<"test_workload">>,
        capabilities => [coordinator, game_server]
    }),

    %% Verify info structure
    ?assertMatch(#{
        leader_node := _,
        cluster_size := _,
        platform_version := <<"0.10.0">>
    }, Info),

    %% Cleanup
    macula:disconnect(Client),
    ok.

test_get_leader() ->
    %% Connect to local platform
    {ok, Client} = macula:connect_local(#{realm => <<"test.platform.api">>}),

    %% Get leader (may be undefined if not in cluster)
    Result = macula:get_leader(Client),

    %% Verify result format
    case Result of
        {ok, Leader} when is_binary(Leader) ->
            ?assert(byte_size(Leader) > 0);
        {error, no_leader} ->
            %% Acceptable if not in a cluster
            ok
    end,

    %% Cleanup
    macula:disconnect(Client),
    ok.

test_crdt_operations() ->
    %% Connect to local platform
    {ok, Client} = macula:connect_local(#{realm => <<"test.platform.api">>}),

    Key = <<"test.crdt.value">>,

    %% Initially, key should not exist
    ?assertEqual({error, not_found}, macula:read_crdt(Client, Key)),

    %% Write a value using default CRDT type (LWW-Register)
    ?assertEqual(ok, macula:propose_crdt_update(Client, Key, <<"test_value">>)),

    %% Read back the value
    ?assertEqual({ok, <<"test_value">>}, macula:read_crdt(Client, Key)),

    %% Update the value
    ?assertEqual(ok, macula:propose_crdt_update(Client, Key, <<"updated_value">>)),

    %% Verify update
    ?assertEqual({ok, <<"updated_value">>}, macula:read_crdt(Client, Key)),

    %% Test with explicit CRDT type
    Key2 = <<"test.crdt.value2">>,
    ?assertEqual(ok, macula:propose_crdt_update(Client, Key2, 42, #{crdt_type => lww_register})),
    ?assertEqual({ok, 42}, macula:read_crdt(Client, Key2)),

    %% Test unsupported CRDT type
    Key3 = <<"test.crdt.value3">>,
    ?assertMatch({error, {unsupported_crdt_type, _}},
                 macula:propose_crdt_update(Client, Key3, value, #{crdt_type => g_counter})),

    %% Cleanup
    macula:disconnect(Client),
    ok.
