%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_membership_detector module.
%%%
%%% Tests SWIM failure detector orchestration.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_membership_detector_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% new/2 Tests
%%%===================================================================

new_creates_detector_with_defaults_test() ->
    %% GIVEN: A local member
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),

    %% WHEN: Creating detector with default config
    State = macula_membership_detector:new(LocalMember, #{}),

    %% THEN: Should have default values
    ?assertEqual(<<"local">>, macula_membership_detector:local_node_id(State)),
    ?assertEqual(1000, macula_membership_detector:protocol_period(State)).

new_creates_detector_with_custom_config_test() ->
    %% GIVEN: A local member and custom config
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    Config = #{
        protocol_period => 2000,
        indirect_count => 5,
        suspect_timeout => 10000
    },

    %% WHEN: Creating detector with custom config
    State = macula_membership_detector:new(LocalMember, Config),

    %% THEN: Should use custom values
    ?assertEqual(2000, macula_membership_detector:protocol_period(State)).

new_includes_local_member_in_list_test() ->
    %% GIVEN: A local member
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),

    %% WHEN: Creating detector
    State = macula_membership_detector:new(LocalMember, #{}),

    %% THEN: Local member should be in the list
    ?assertMatch({ok, _}, macula_membership_detector:get_member(State, <<"local">>)).

%%%===================================================================
%%% add_member/2 Tests
%%%===================================================================

add_member_adds_to_list_test() ->
    %% GIVEN: A detector with local member
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    State = macula_membership_detector:new(LocalMember, #{}),

    %% WHEN: Adding a remote member
    RemoteMember = macula_membership_member:new(<<"remote">>, {{10,0,0,1}, 4000}),
    Updated = macula_membership_detector:add_member(State, RemoteMember),

    %% THEN: Remote member should be in the list
    ?assertMatch({ok, _}, macula_membership_detector:get_member(Updated, <<"remote">>)).

add_member_multiple_members_test() ->
    %% GIVEN: A detector with local member
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    State = macula_membership_detector:new(LocalMember, #{}),

    %% WHEN: Adding multiple remote members
    Remote1 = macula_membership_member:new(<<"remote1">>, {{10,0,0,1}, 4000}),
    Remote2 = macula_membership_member:new(<<"remote2">>, {{10,0,0,2}, 4000}),
    State1 = macula_membership_detector:add_member(State, Remote1),
    State2 = macula_membership_detector:add_member(State1, Remote2),

    %% THEN: All members should be in the list
    Alive = macula_membership_detector:get_alive_members(State2),
    ?assertEqual(3, length(Alive)).

%%%===================================================================
%%% get_member/2 Tests
%%%===================================================================

get_member_returns_existing_test() ->
    %% GIVEN: A detector with members
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    State = macula_membership_detector:new(LocalMember, #{}),

    %% WHEN: Getting existing member
    Result = macula_membership_detector:get_member(State, <<"local">>),

    %% THEN: Should return the member
    ?assertMatch({ok, _}, Result).

get_member_returns_not_found_test() ->
    %% GIVEN: A detector with members
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    State = macula_membership_detector:new(LocalMember, #{}),

    %% WHEN: Getting non-existent member
    Result = macula_membership_detector:get_member(State, <<"missing">>),

    %% THEN: Should return not_found
    ?assertEqual(not_found, Result).

%%%===================================================================
%%% get_alive_members/1 Tests
%%%===================================================================

get_alive_members_returns_only_alive_test() ->
    %% GIVEN: A detector with alive and suspect members
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    Remote = macula_membership_member:new(<<"remote">>, {{10,0,0,1}, 4000}),

    State0 = macula_membership_detector:new(LocalMember, #{}),
    State1 = macula_membership_detector:add_member(State0, Remote),
    State2 = macula_membership_detector:mark_suspect(State1, <<"remote">>),

    %% WHEN: Getting alive members
    Alive = macula_membership_detector:get_alive_members(State2),

    %% THEN: Should only return alive members
    ?assertEqual(1, length(Alive)),
    [Member] = Alive,
    ?assertEqual(<<"local">>, macula_membership_member:node_id(Member)).

%%%===================================================================
%%% select_probe_target/1 Tests
%%%===================================================================

select_probe_target_returns_member_test() ->
    %% GIVEN: A detector with multiple members
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    Remote = macula_membership_member:new(<<"remote">>, {{10,0,0,1}, 4000}),

    State0 = macula_membership_detector:new(LocalMember, #{}),
    State1 = macula_membership_detector:add_member(State0, Remote),

    %% WHEN: Selecting probe target
    Result = macula_membership_detector:select_probe_target(State1),

    %% THEN: Should return a target (not self)
    ?assertMatch({ok, _, _}, Result),
    {ok, Target, _NewState} = Result,
    ?assertEqual(<<"remote">>, macula_membership_member:node_id(Target)).

select_probe_target_excludes_self_test() ->
    %% GIVEN: A detector with only local member
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    State = macula_membership_detector:new(LocalMember, #{}),

    %% WHEN: Selecting probe target
    Result = macula_membership_detector:select_probe_target(State),

    %% THEN: Should return none (only self in list)
    ?assertEqual(none, Result).

%%%===================================================================
%%% mark_suspect/2 Tests
%%%===================================================================

mark_suspect_updates_member_status_test() ->
    %% GIVEN: A detector with a member
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    Remote = macula_membership_member:new(<<"remote">>, {{10,0,0,1}, 4000}),

    State0 = macula_membership_detector:new(LocalMember, #{}),
    State1 = macula_membership_detector:add_member(State0, Remote),

    %% WHEN: Marking as suspect
    Updated = macula_membership_detector:mark_suspect(State1, <<"remote">>),

    %% THEN: Member should be suspect
    {ok, Member} = macula_membership_detector:get_member(Updated, <<"remote">>),
    ?assertEqual(suspect, macula_membership_member:status(Member)).

mark_suspect_adds_to_gossip_test() ->
    %% GIVEN: A detector with a member
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    Remote = macula_membership_member:new(<<"remote">>, {{10,0,0,1}, 4000}),

    State0 = macula_membership_detector:new(LocalMember, #{}),
    State1 = macula_membership_detector:add_member(State0, Remote),

    %% WHEN: Marking as suspect
    Updated = macula_membership_detector:mark_suspect(State1, <<"remote">>),

    %% THEN: Gossip should have the update
    Updates = macula_membership_detector:get_gossip_updates(Updated, 10),
    ?assert(length(Updates) > 0),
    %% Find the suspect update
    SuspectUpdates = [U || {NodeId, Status, _, _} = U <- Updates,
                          NodeId =:= <<"remote">>, Status =:= suspect],
    ?assertEqual(1, length(SuspectUpdates)).

mark_suspect_nonexistent_is_noop_test() ->
    %% GIVEN: A detector
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    State = macula_membership_detector:new(LocalMember, #{}),

    %% WHEN: Marking non-existent member as suspect
    Updated = macula_membership_detector:mark_suspect(State, <<"missing">>),

    %% THEN: State should be unchanged (only local in list)
    Alive = macula_membership_detector:get_alive_members(Updated),
    ?assertEqual(1, length(Alive)).

%%%===================================================================
%%% mark_dead/2 Tests
%%%===================================================================

mark_dead_updates_member_status_test() ->
    %% GIVEN: A detector with a suspect member
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    Remote = macula_membership_member:new(<<"remote">>, {{10,0,0,1}, 4000}),

    State0 = macula_membership_detector:new(LocalMember, #{}),
    State1 = macula_membership_detector:add_member(State0, Remote),
    State2 = macula_membership_detector:mark_suspect(State1, <<"remote">>),

    %% WHEN: Marking as dead
    Updated = macula_membership_detector:mark_dead(State2, <<"remote">>),

    %% THEN: Member should be dead
    {ok, Member} = macula_membership_detector:get_member(Updated, <<"remote">>),
    ?assertEqual(dead, macula_membership_member:status(Member)).

mark_dead_adds_to_gossip_test() ->
    %% GIVEN: A detector with a member
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    Remote = macula_membership_member:new(<<"remote">>, {{10,0,0,1}, 4000}),

    State0 = macula_membership_detector:new(LocalMember, #{}),
    State1 = macula_membership_detector:add_member(State0, Remote),

    %% WHEN: Marking as dead
    Updated = macula_membership_detector:mark_dead(State1, <<"remote">>),

    %% THEN: Gossip should have the dead update
    Updates = macula_membership_detector:get_gossip_updates(Updated, 10),
    DeadUpdates = [U || {NodeId, Status, _, _} = U <- Updates,
                       NodeId =:= <<"remote">>, Status =:= dead],
    ?assertEqual(1, length(DeadUpdates)).

%%%===================================================================
%%% refute_suspicion/1 Tests
%%%===================================================================

refute_suspicion_increments_incarnation_test() ->
    %% GIVEN: A detector where local is suspected
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    State = macula_membership_detector:new(LocalMember, #{}),

    %% Get initial incarnation
    {ok, InitialMember} = macula_membership_detector:get_member(State, <<"local">>),
    InitialInc = macula_membership_member:incarnation(InitialMember),

    %% WHEN: Refuting suspicion
    Updated = macula_membership_detector:refute_suspicion(State),

    %% THEN: Incarnation should be incremented
    {ok, RefutedMember} = macula_membership_detector:get_member(Updated, <<"local">>),
    ?assertEqual(InitialInc + 1, macula_membership_member:incarnation(RefutedMember)).

refute_suspicion_broadcasts_alive_test() ->
    %% GIVEN: A detector
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    State = macula_membership_detector:new(LocalMember, #{}),

    %% WHEN: Refuting suspicion
    Updated = macula_membership_detector:refute_suspicion(State),

    %% THEN: Gossip should have alive update for local
    Updates = macula_membership_detector:get_gossip_updates(Updated, 10),
    AliveUpdates = [U || {NodeId, Status, _, _} = U <- Updates,
                        NodeId =:= <<"local">>, Status =:= alive],
    ?assertEqual(1, length(AliveUpdates)).

%%%===================================================================
%%% get_gossip_updates/2 Tests
%%%===================================================================

get_gossip_updates_returns_updates_test() ->
    %% GIVEN: A detector with gossip updates
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    Remote = macula_membership_member:new(<<"remote">>, {{10,0,0,1}, 4000}),

    State0 = macula_membership_detector:new(LocalMember, #{}),
    State1 = macula_membership_detector:add_member(State0, Remote),
    State2 = macula_membership_detector:mark_suspect(State1, <<"remote">>),

    %% WHEN: Getting gossip updates
    Updates = macula_membership_detector:get_gossip_updates(State2, 10),

    %% THEN: Should return the updates
    ?assert(length(Updates) > 0).

get_gossip_updates_respects_max_test() ->
    %% GIVEN: A detector with multiple gossip updates
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    State0 = macula_membership_detector:new(LocalMember, #{}),

    %% Add multiple members and mark them suspect
    State = lists:foldl(
        fun(I, Acc) ->
            NodeId = list_to_binary("node" ++ integer_to_list(I)),
            M = macula_membership_member:new(NodeId, {{I,I,I,I}, 4000}),
            Acc1 = macula_membership_detector:add_member(Acc, M),
            macula_membership_detector:mark_suspect(Acc1, NodeId)
        end,
        State0,
        lists:seq(1, 5)
    ),

    %% WHEN: Getting max 2 updates
    Updates = macula_membership_detector:get_gossip_updates(State, 2),

    %% THEN: Should return at most 2
    ?assert(length(Updates) =< 2).

%%%===================================================================
%%% apply_gossip_updates/2 Tests
%%%===================================================================

apply_gossip_updates_adds_new_members_test() ->
    %% GIVEN: A detector with only local member
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    State = macula_membership_detector:new(LocalMember, #{}),

    %% WHEN: Applying gossip with new member
    GossipUpdates = [
        {<<"new_node">>, alive, 0}
    ],
    Updated = macula_membership_detector:apply_gossip_updates(State, GossipUpdates),

    %% THEN: New member should be in the list
    ?assertMatch({ok, _}, macula_membership_detector:get_member(Updated, <<"new_node">>)).

apply_gossip_updates_updates_existing_test() ->
    %% GIVEN: A detector with a remote member
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    Remote = macula_membership_member:new(<<"remote">>, {{10,0,0,1}, 4000}),

    State0 = macula_membership_detector:new(LocalMember, #{}),
    State1 = macula_membership_detector:add_member(State0, Remote),

    %% WHEN: Applying gossip with suspect status
    GossipUpdates = [
        {<<"remote">>, suspect, 0}
    ],
    Updated = macula_membership_detector:apply_gossip_updates(State1, GossipUpdates),

    %% THEN: Member should be suspect
    {ok, Member} = macula_membership_detector:get_member(Updated, <<"remote">>),
    ?assertEqual(suspect, macula_membership_member:status(Member)).

apply_gossip_updates_does_not_add_dead_members_test() ->
    %% GIVEN: A detector with only local member
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    State = macula_membership_detector:new(LocalMember, #{}),

    %% WHEN: Applying gossip with dead member (unknown)
    GossipUpdates = [
        {<<"dead_node">>, dead, 5}
    ],
    Updated = macula_membership_detector:apply_gossip_updates(State, GossipUpdates),

    %% THEN: Dead member should NOT be added (don't add already-dead)
    ?assertEqual(not_found, macula_membership_detector:get_member(Updated, <<"dead_node">>)).

apply_gossip_updates_merges_into_gossip_state_test() ->
    %% GIVEN: A detector
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    State = macula_membership_detector:new(LocalMember, #{}),

    %% WHEN: Applying gossip updates
    GossipUpdates = [
        {<<"new_node">>, alive, 0}
    ],
    Updated = macula_membership_detector:apply_gossip_updates(State, GossipUpdates),

    %% THEN: Gossip state should have the update for retransmission
    Updates = macula_membership_detector:get_gossip_updates(Updated, 10),
    NodeIds = [NodeId || {NodeId, _, _, _} <- Updates],
    ?assert(lists:member(<<"new_node">>, NodeIds)).
