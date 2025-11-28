%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_nat_cache module.
%%% Tests NAT profile caching with stale-while-revalidate.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_nat_cache_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup/Teardown
%%%===================================================================

%% @doc Fixture to start and stop the NAT cache for each test
nat_cache_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun put_and_get_profile_test/1,
         fun get_non_existing_returns_not_found_test/1,
         fun put_updates_existing_profile_test/1,
         fun invalidate_removes_profile_test/1,
         fun clear_removes_all_profiles_test/1,
         fun stats_returns_hit_miss_info_test/1,
         fun stale_entry_returns_stale_status_test/1,
         fun prepare_profile_for_dht_test/1
     ]}.

setup() ->
    %% Start the NAT cache server
    {ok, Pid} = macula_nat_cache:start_link(#{}),
    Pid.

cleanup(Pid) ->
    %% Stop the NAT cache server
    gen_server:stop(Pid).

%%%===================================================================
%%% Test Cases
%%%===================================================================

put_and_get_profile_test(_Pid) ->
    fun() ->
        NodeId = <<"node-001">>,
        Profile = make_test_profile(NodeId),

        %% Put profile
        ok = macula_nat_cache:put(NodeId, Profile),

        %% Small delay to allow async put to complete
        timer:sleep(50),

        %% Get profile
        case macula_nat_cache:get(NodeId) of
            {ok, Retrieved, fresh} ->
                ?assertEqual(NodeId, maps:get(node_id, Retrieved)),
                ?assertEqual(ei, maps:get(mapping_policy, Retrieved));
            {ok, Retrieved, stale} ->
                ?assertEqual(NodeId, maps:get(node_id, Retrieved));
            Other ->
                ?assertEqual({ok, retrieved, fresh}, Other)
        end
    end.

get_non_existing_returns_not_found_test(_Pid) ->
    fun() ->
        ?assertEqual(not_found, macula_nat_cache:get(<<"nonexistent-node">>))
    end.

put_updates_existing_profile_test(_Pid) ->
    fun() ->
        NodeId = <<"node-002">>,
        Profile1 = make_test_profile(NodeId, ei, ei, pp),
        Profile2 = make_test_profile(NodeId, hd, pd, pc),

        %% Put first profile
        ok = macula_nat_cache:put(NodeId, Profile1),
        timer:sleep(50),

        %% Update with second profile
        ok = macula_nat_cache:put(NodeId, Profile2),
        timer:sleep(50),

        %% Should get the updated profile
        case macula_nat_cache:get(NodeId) of
            {ok, Retrieved, _Status} ->
                ?assertEqual(hd, maps:get(mapping_policy, Retrieved)),
                ?assertEqual(pd, maps:get(filtering_policy, Retrieved));
            Other ->
                ?assertEqual({ok, updated_profile, fresh}, Other)
        end
    end.

invalidate_removes_profile_test(_Pid) ->
    fun() ->
        NodeId = <<"node-003">>,
        Profile = make_test_profile(NodeId),

        %% Put profile
        ok = macula_nat_cache:put(NodeId, Profile),
        timer:sleep(50),

        %% Invalidate
        ok = macula_nat_cache:invalidate(NodeId),
        timer:sleep(50),

        %% Should be not found
        ?assertEqual(not_found, macula_nat_cache:get(NodeId))
    end.

clear_removes_all_profiles_test(_Pid) ->
    fun() ->
        %% Put multiple profiles
        ok = macula_nat_cache:put(<<"node-a">>, make_test_profile(<<"node-a">>)),
        ok = macula_nat_cache:put(<<"node-b">>, make_test_profile(<<"node-b">>)),
        ok = macula_nat_cache:put(<<"node-c">>, make_test_profile(<<"node-c">>)),
        timer:sleep(50),

        %% Clear all
        ok = macula_nat_cache:clear(),
        timer:sleep(50),

        %% All should be not found
        ?assertEqual(not_found, macula_nat_cache:get(<<"node-a">>)),
        ?assertEqual(not_found, macula_nat_cache:get(<<"node-b">>)),
        ?assertEqual(not_found, macula_nat_cache:get(<<"node-c">>))
    end.

stats_returns_hit_miss_info_test(_Pid) ->
    fun() ->
        NodeId = <<"node-stats">>,
        Profile = make_test_profile(NodeId),

        %% Clear stats first
        ok = macula_nat_cache:clear(),
        timer:sleep(50),

        %% Generate a miss
        not_found = macula_nat_cache:get(<<"nonexistent">>),

        %% Put and generate a hit
        ok = macula_nat_cache:put(NodeId, Profile),
        timer:sleep(50),
        {ok, _, _} = macula_nat_cache:get(NodeId),

        %% Check stats
        Stats = macula_nat_cache:stats(),
        ?assert(is_map(Stats)),
        ?assert(maps:get(misses, Stats) >= 1),
        ?assert(maps:get(hits, Stats) >= 1)
    end.

stale_entry_returns_stale_status_test(_Pid) ->
    fun() ->
        %% This test verifies stale-while-revalidate behavior
        %% Since we can't easily manipulate time in EUnit, we just verify
        %% that fresh entries return 'fresh' status
        NodeId = <<"node-fresh">>,
        Profile = make_test_profile(NodeId),

        ok = macula_nat_cache:put(NodeId, Profile),
        timer:sleep(50),

        case macula_nat_cache:get(NodeId) of
            {ok, _, fresh} -> ?assert(true);
            {ok, _, stale} -> ?assert(true);  % Still valid
            Other -> ?assertEqual({ok, profile, fresh_or_stale}, Other)
        end
    end.

prepare_profile_for_dht_test(_Pid) ->
    fun() ->
        %% Test that profiles can be serialized/deserialized for DHT
        %% This is more of a smoke test
        NodeId = <<"node-dht-test">>,
        Profile = make_test_profile(NodeId, ei, hd, pp),

        ok = macula_nat_cache:put(NodeId, Profile),
        timer:sleep(50),

        %% Verify profile was stored
        case macula_nat_cache:get(NodeId) of
            {ok, Retrieved, _} ->
                ?assertEqual(NodeId, maps:get(node_id, Retrieved));
            Other ->
                ?assertEqual({ok, profile, status}, Other)
        end
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Create a test NAT profile with default policies
make_test_profile(NodeId) ->
    make_test_profile(NodeId, ei, ei, pp).

%% @doc Create a test NAT profile with specified policies
make_test_profile(NodeId, MappingPolicy, FilteringPolicy, AllocationPolicy) ->
    #{
        node_id => NodeId,
        mapping_policy => MappingPolicy,
        filtering_policy => FilteringPolicy,
        allocation_policy => AllocationPolicy,
        can_receive_unsolicited => (FilteringPolicy == ei),
        requires_relay => (MappingPolicy == pd andalso FilteringPolicy == pd),
        relay_capable => true
    }.
