%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_membership_gossip module.
%%% Tests written FIRST (TDD red phase).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_membership_gossip_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Gossip Creation Tests
%%%===================================================================

%% Test: new creates empty gossip state
new_gossip_is_empty_test() ->
    Gossip = macula_membership_gossip:new(),
    Updates = macula_membership_gossip:get_updates(Gossip, 10),
    ?assertEqual([], Updates).

%%%===================================================================
%%% Add Update Tests
%%%===================================================================

%% Test: add_update stores update
add_update_stores_test() ->
    Gossip = macula_membership_gossip:new(),
    NodeId = <<1:256>>,

    Gossip2 = macula_membership_gossip:add_update(Gossip, NodeId, alive, 0),
    Updates = macula_membership_gossip:get_updates(Gossip2, 10),

    ?assertEqual(1, length(Updates)),
    [{UpdateNodeId, Status, Inc, _Count}] = Updates,
    ?assertEqual(NodeId, UpdateNodeId),
    ?assertEqual(alive, Status),
    ?assertEqual(0, Inc).

%% Test: add_update increments transmit count on duplicate
add_update_increments_count_test() ->
    Gossip = macula_membership_gossip:new(),
    NodeId = <<1:256>>,

    Gossip2 = macula_membership_gossip:add_update(Gossip, NodeId, alive, 0),
    [{_, _, _, Count1}] = macula_membership_gossip:get_updates(Gossip2, 10),
    ?assertEqual(0, Count1),

    %% Get updates (simulates transmission)
    _Updates = macula_membership_gossip:get_updates(Gossip2, 10),

    %% Add same update again (should increment count)
    Gossip3 = macula_membership_gossip:add_update(Gossip2, NodeId, alive, 0),
    [{_, _, _, Count2}] = macula_membership_gossip:get_updates(Gossip3, 10),
    ?assertEqual(0, Count2).  % Still 0 because it's the same state

%% Test: higher incarnation replaces old update
higher_incarnation_replaces_test() ->
    Gossip = macula_membership_gossip:new(),
    NodeId = <<1:256>>,

    Gossip2 = macula_membership_gossip:add_update(Gossip, NodeId, alive, 0),
    Gossip3 = macula_membership_gossip:add_update(Gossip2, NodeId, alive, 5),

    Updates = macula_membership_gossip:get_updates(Gossip3, 10),
    ?assertEqual(1, length(Updates)),
    [{_, _, Inc, _}] = Updates,
    ?assertEqual(5, Inc).

%%%===================================================================
%%% Get Updates Tests
%%%===================================================================

%% Test: get_updates respects max_updates limit
get_updates_respects_limit_test() ->
    Gossip = macula_membership_gossip:new(),

    %% Add 5 updates
    Gossip2 = lists:foldl(
        fun(I, Acc) ->
            macula_membership_gossip:add_update(Acc, <<I:256>>, alive, 0)
        end,
        Gossip,
        lists:seq(1, 5)
    ),

    %% Request only 3
    Updates = macula_membership_gossip:get_updates(Gossip2, 3),
    ?assertEqual(3, length(Updates)).

%% Test: get_updates returns most recent first
get_updates_most_recent_first_test() ->
    Gossip = macula_membership_gossip:new(),

    %% Add updates with different timestamps
    Gossip2 = macula_membership_gossip:add_update(Gossip, <<1:256>>, alive, 0),
    timer:sleep(10),
    Gossip3 = macula_membership_gossip:add_update(Gossip2, <<2:256>>, alive, 0),
    timer:sleep(10),
    Gossip4 = macula_membership_gossip:add_update(Gossip3, <<3:256>>, alive, 0),

    Updates = macula_membership_gossip:get_updates(Gossip4, 10),
    ?assertEqual(3, length(Updates)),

    %% Most recent should be first (node 3)
    [{FirstNodeId, _, _, _} | _] = Updates,
    ?assertEqual(<<3:256>>, FirstNodeId).

%%%===================================================================
%%% Mark Transmitted Tests
%%%===================================================================

%% Test: mark_transmitted increments transmit count
mark_transmitted_increments_test() ->
    Gossip = macula_membership_gossip:new(),
    NodeId = <<1:256>>,

    Gossip2 = macula_membership_gossip:add_update(Gossip, NodeId, alive, 0),
    [{_, _, _, Count1}] = macula_membership_gossip:get_updates(Gossip2, 10),
    ?assertEqual(0, Count1),

    %% Mark as transmitted
    Gossip3 = macula_membership_gossip:mark_transmitted(Gossip2, NodeId),
    [{_, _, _, Count2}] = macula_membership_gossip:get_updates(Gossip3, 10),
    ?assertEqual(1, Count2).

%% Test: mark_transmitted multiple times increments count
mark_transmitted_multiple_test() ->
    Gossip = macula_membership_gossip:new(),
    NodeId = <<1:256>>,

    Gossip2 = macula_membership_gossip:add_update(Gossip, NodeId, alive, 0),
    Gossip3 = macula_membership_gossip:mark_transmitted(Gossip2, NodeId),
    Gossip4 = macula_membership_gossip:mark_transmitted(Gossip3, NodeId),
    Gossip5 = macula_membership_gossip:mark_transmitted(Gossip4, NodeId),

    [{_, _, _, Count}] = macula_membership_gossip:get_updates(Gossip5, 10),
    ?assertEqual(3, Count).

%%%===================================================================
%%% Prune Tests
%%%===================================================================

%% Test: prune removes updates exceeding target transmit count
prune_removes_old_updates_test() ->
    Gossip = macula_membership_gossip:new(),

    %% Add updates
    Gossip2 = macula_membership_gossip:add_update(Gossip, <<1:256>>, alive, 0),
    Gossip3 = macula_membership_gossip:add_update(Gossip2, <<2:256>>, alive, 0),

    %% Mark first update transmitted many times
    Gossip4 = lists:foldl(
        fun(_, Acc) ->
            macula_membership_gossip:mark_transmitted(Acc, <<1:256>>)
        end,
        Gossip3,
        lists:seq(1, 10)
    ),

    %% Prune with target=5 (first update has 10 transmits, should be removed)
    Gossip5 = macula_membership_gossip:prune(Gossip4, 5),
    Updates = macula_membership_gossip:get_updates(Gossip5, 10),

    %% Should only have node 2
    ?assertEqual(1, length(Updates)),
    [{NodeId, _, _, _}] = Updates,
    ?assertEqual(<<2:256>>, NodeId).

%% Test: prune keeps updates below target count
prune_keeps_recent_test() ->
    Gossip = macula_membership_gossip:new(),

    %% Add update and transmit a few times
    Gossip2 = macula_membership_gossip:add_update(Gossip, <<1:256>>, alive, 0),
    Gossip3 = macula_membership_gossip:mark_transmitted(Gossip2, <<1:256>>),
    Gossip4 = macula_membership_gossip:mark_transmitted(Gossip3, <<1:256>>),

    %% Prune with target=5 (update has 2 transmits, should be kept)
    Gossip5 = macula_membership_gossip:prune(Gossip4, 5),
    Updates = macula_membership_gossip:get_updates(Gossip5, 10),

    ?assertEqual(1, length(Updates)).

%%%===================================================================
%%% Merge Updates Tests
%%%===================================================================

%% Test: merge_updates applies received updates
merge_updates_applies_test() ->
    Gossip = macula_membership_gossip:new(),

    ReceivedUpdates = [
        {<<1:256>>, alive, 0},
        {<<2:256>>, suspect, 1},
        {<<3:256>>, dead, 0}
    ],

    Gossip2 = macula_membership_gossip:merge_updates(Gossip, ReceivedUpdates),
    Updates = macula_membership_gossip:get_updates(Gossip2, 10),

    ?assertEqual(3, length(Updates)).

%% Test: merge_updates ignores stale information
merge_updates_ignores_stale_test() ->
    Gossip = macula_membership_gossip:new(),

    %% Add update with incarnation 5
    Gossip2 = macula_membership_gossip:add_update(Gossip, <<1:256>>, alive, 5),

    %% Try to merge older incarnation
    ReceivedUpdates = [{<<1:256>>, suspect, 2}],
    Gossip3 = macula_membership_gossip:merge_updates(Gossip2, ReceivedUpdates),

    %% Should still have incarnation 5, status alive
    Updates = macula_membership_gossip:get_updates(Gossip3, 10),
    [{_, Status, Inc, _}] = Updates,
    ?assertEqual(alive, Status),
    ?assertEqual(5, Inc).
