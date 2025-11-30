%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_membership_gossip module.
%%%
%%% Tests gossip dissemination for SWIM protocol.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_membership_gossip_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% new/0 Tests
%%%===================================================================

new_creates_empty_state_test() ->
    %% WHEN: Creating a new gossip state
    State = macula_membership_gossip:new(),

    %% THEN: Should have empty updates
    ?assertEqual(#{updates => #{}}, State).

%%%===================================================================
%%% add_update/4 Tests
%%%===================================================================

add_update_adds_new_entry_test() ->
    %% GIVEN: Empty gossip state
    State = macula_membership_gossip:new(),

    %% WHEN: Adding a new update
    Updated = macula_membership_gossip:add_update(State, <<"node1">>, alive, 0),

    %% THEN: Should have one update with transmit count 0
    Updates = macula_membership_gossip:get_updates(Updated, 10),
    ?assertEqual(1, length(Updates)),
    [{NodeId, Status, Inc, TransmitCount}] = Updates,
    ?assertEqual(<<"node1">>, NodeId),
    ?assertEqual(alive, Status),
    ?assertEqual(0, Inc),
    ?assertEqual(0, TransmitCount).

add_update_replaces_with_higher_incarnation_test() ->
    %% GIVEN: State with an update at incarnation 5
    State0 = macula_membership_gossip:new(),
    State1 = macula_membership_gossip:add_update(State0, <<"node1">>, alive, 5),

    %% WHEN: Adding update with higher incarnation
    Updated = macula_membership_gossip:add_update(State1, <<"node1">>, suspect, 10),

    %% THEN: Should have the higher incarnation
    [{_, Status, Inc, _}] = macula_membership_gossip:get_updates(Updated, 10),
    ?assertEqual(suspect, Status),
    ?assertEqual(10, Inc).

add_update_ignores_lower_incarnation_test() ->
    %% GIVEN: State with an update at incarnation 10
    State0 = macula_membership_gossip:new(),
    State1 = macula_membership_gossip:add_update(State0, <<"node1">>, alive, 10),

    %% WHEN: Adding update with lower incarnation
    Updated = macula_membership_gossip:add_update(State1, <<"node1">>, dead, 5),

    %% THEN: Should keep original incarnation and status
    [{_, Status, Inc, _}] = macula_membership_gossip:get_updates(Updated, 10),
    ?assertEqual(alive, Status),
    ?assertEqual(10, Inc).

add_update_replaces_same_incarnation_different_status_test() ->
    %% GIVEN: State with alive at incarnation 5
    State0 = macula_membership_gossip:new(),
    State1 = macula_membership_gossip:add_update(State0, <<"node1">>, alive, 5),

    %% WHEN: Adding suspect at same incarnation
    Updated = macula_membership_gossip:add_update(State1, <<"node1">>, suspect, 5),

    %% THEN: Should have suspect status (more severe)
    [{_, Status, Inc, _}] = macula_membership_gossip:get_updates(Updated, 10),
    ?assertEqual(suspect, Status),
    ?assertEqual(5, Inc).

add_update_keeps_same_incarnation_same_status_test() ->
    %% GIVEN: State with alive at incarnation 5
    State0 = macula_membership_gossip:new(),
    State1 = macula_membership_gossip:add_update(State0, <<"node1">>, alive, 5),

    %% Increment transmit count
    State2 = macula_membership_gossip:mark_transmitted(State1, <<"node1">>),

    %% WHEN: Adding same status at same incarnation
    Updated = macula_membership_gossip:add_update(State2, <<"node1">>, alive, 5),

    %% THEN: Should keep existing (with transmit count preserved)
    [{_, _, _, TransmitCount}] = macula_membership_gossip:get_updates(Updated, 10),
    ?assertEqual(1, TransmitCount).

%%%===================================================================
%%% get_updates/2 Tests
%%%===================================================================

get_updates_returns_most_recent_first_test() ->
    %% GIVEN: State with multiple updates
    State0 = macula_membership_gossip:new(),
    State1 = macula_membership_gossip:add_update(State0, <<"node1">>, alive, 0),
    timer:sleep(10),  % Ensure different timestamps
    State2 = macula_membership_gossip:add_update(State1, <<"node2">>, alive, 0),
    timer:sleep(10),
    State3 = macula_membership_gossip:add_update(State2, <<"node3">>, alive, 0),

    %% WHEN: Getting updates
    Updates = macula_membership_gossip:get_updates(State3, 10),

    %% THEN: Most recent should be first
    [First | _] = Updates,
    {FirstNodeId, _, _, _} = First,
    ?assertEqual(<<"node3">>, FirstNodeId).

get_updates_respects_max_limit_test() ->
    %% GIVEN: State with 5 updates
    State = lists:foldl(
        fun(I, Acc) ->
            NodeId = list_to_binary("node" ++ integer_to_list(I)),
            timer:sleep(5),
            macula_membership_gossip:add_update(Acc, NodeId, alive, 0)
        end,
        macula_membership_gossip:new(),
        lists:seq(1, 5)
    ),

    %% WHEN: Getting max 3 updates
    Updates = macula_membership_gossip:get_updates(State, 3),

    %% THEN: Should return only 3
    ?assertEqual(3, length(Updates)).

get_updates_returns_empty_for_empty_state_test() ->
    %% GIVEN: Empty state
    State = macula_membership_gossip:new(),

    %% WHEN: Getting updates
    Updates = macula_membership_gossip:get_updates(State, 10),

    %% THEN: Should be empty
    ?assertEqual([], Updates).

%%%===================================================================
%%% mark_transmitted/2 Tests
%%%===================================================================

mark_transmitted_increments_count_test() ->
    %% GIVEN: State with an update
    State0 = macula_membership_gossip:new(),
    State1 = macula_membership_gossip:add_update(State0, <<"node1">>, alive, 0),

    %% WHEN: Marking as transmitted multiple times
    State2 = macula_membership_gossip:mark_transmitted(State1, <<"node1">>),
    State3 = macula_membership_gossip:mark_transmitted(State2, <<"node1">>),
    State4 = macula_membership_gossip:mark_transmitted(State3, <<"node1">>),

    %% THEN: Transmit count should be 3
    [{_, _, _, TransmitCount}] = macula_membership_gossip:get_updates(State4, 10),
    ?assertEqual(3, TransmitCount).

mark_transmitted_nonexistent_is_noop_test() ->
    %% GIVEN: State with one update
    State0 = macula_membership_gossip:new(),
    State1 = macula_membership_gossip:add_update(State0, <<"node1">>, alive, 0),

    %% WHEN: Marking non-existent node
    Updated = macula_membership_gossip:mark_transmitted(State1, <<"missing">>),

    %% THEN: State should be unchanged (still 1 update)
    Updates = macula_membership_gossip:get_updates(Updated, 10),
    ?assertEqual(1, length(Updates)).

%%%===================================================================
%%% merge_updates/2 Tests
%%%===================================================================

merge_updates_adds_new_entries_test() ->
    %% GIVEN: Empty state
    State = macula_membership_gossip:new(),

    %% WHEN: Merging received updates
    ReceivedUpdates = [
        {<<"node1">>, alive, 0},
        {<<"node2">>, suspect, 1},
        {<<"node3">>, dead, 2}
    ],
    Updated = macula_membership_gossip:merge_updates(State, ReceivedUpdates),

    %% THEN: Should have all 3 updates
    Updates = macula_membership_gossip:get_updates(Updated, 10),
    ?assertEqual(3, length(Updates)).

merge_updates_uses_swim_semantics_test() ->
    %% GIVEN: State with existing update
    State0 = macula_membership_gossip:new(),
    State1 = macula_membership_gossip:add_update(State0, <<"node1">>, alive, 5),

    %% WHEN: Merging with higher incarnation
    ReceivedUpdates = [{<<"node1">>, suspect, 10}],
    Updated = macula_membership_gossip:merge_updates(State1, ReceivedUpdates),

    %% THEN: Higher incarnation should win
    [{_, Status, Inc, _}] = macula_membership_gossip:get_updates(Updated, 10),
    ?assertEqual(suspect, Status),
    ?assertEqual(10, Inc).

merge_updates_empty_list_is_noop_test() ->
    %% GIVEN: State with an update
    State0 = macula_membership_gossip:new(),
    State1 = macula_membership_gossip:add_update(State0, <<"node1">>, alive, 0),

    %% WHEN: Merging empty list
    Updated = macula_membership_gossip:merge_updates(State1, []),

    %% THEN: State should be unchanged
    Updates = macula_membership_gossip:get_updates(Updated, 10),
    ?assertEqual(1, length(Updates)).

%%%===================================================================
%%% prune/2 Tests
%%%===================================================================

prune_removes_fully_transmitted_test() ->
    %% GIVEN: State with updates at different transmit counts
    State0 = macula_membership_gossip:new(),
    State1 = macula_membership_gossip:add_update(State0, <<"node1">>, alive, 0),
    State2 = macula_membership_gossip:add_update(State1, <<"node2">>, alive, 0),

    %% Transmit node1 5 times, node2 2 times
    State3 = lists:foldl(
        fun(_, Acc) -> macula_membership_gossip:mark_transmitted(Acc, <<"node1">>) end,
        State2,
        lists:seq(1, 5)
    ),
    State4 = lists:foldl(
        fun(_, Acc) -> macula_membership_gossip:mark_transmitted(Acc, <<"node2">>) end,
        State3,
        lists:seq(1, 2)
    ),

    %% WHEN: Pruning with target of 3
    Pruned = macula_membership_gossip:prune(State4, 3),

    %% THEN: node1 (5 transmits) should be removed, node2 (2 transmits) kept
    Updates = macula_membership_gossip:get_updates(Pruned, 10),
    ?assertEqual(1, length(Updates)),
    [{NodeId, _, _, _}] = Updates,
    ?assertEqual(<<"node2">>, NodeId).

prune_keeps_under_target_test() ->
    %% GIVEN: State with updates all under target
    State0 = macula_membership_gossip:new(),
    State1 = macula_membership_gossip:add_update(State0, <<"node1">>, alive, 0),
    State2 = macula_membership_gossip:add_update(State1, <<"node2">>, alive, 0),

    %% Transmit both once
    State3 = macula_membership_gossip:mark_transmitted(State2, <<"node1">>),
    State4 = macula_membership_gossip:mark_transmitted(State3, <<"node2">>),

    %% WHEN: Pruning with target of 5
    Pruned = macula_membership_gossip:prune(State4, 5),

    %% THEN: Both should remain
    Updates = macula_membership_gossip:get_updates(Pruned, 10),
    ?assertEqual(2, length(Updates)).

prune_empty_state_test() ->
    %% GIVEN: Empty state
    State = macula_membership_gossip:new(),

    %% WHEN: Pruning
    Pruned = macula_membership_gossip:prune(State, 3),

    %% THEN: Should remain empty
    Updates = macula_membership_gossip:get_updates(Pruned, 10),
    ?assertEqual([], Updates).

