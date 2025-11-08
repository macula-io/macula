%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_membership_member module.
%%% Tests written FIRST (TDD red phase).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_membership_member_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Member Creation Tests
%%%===================================================================

%% Test: new creates member with alive status
new_member_is_alive_test() ->
    NodeId = <<1:256>>,
    Address = {{127,0,0,1}, 8080},
    Member = macula_membership_member:new(NodeId, Address),

    ?assertEqual(alive, macula_membership_member:status(Member)),
    ?assertEqual(NodeId, macula_membership_member:node_id(Member)),
    ?assertEqual(Address, macula_membership_member:address(Member)),
    ?assertEqual(0, macula_membership_member:incarnation(Member)).

%% Test: new with metadata stores custom data
new_member_with_metadata_test() ->
    NodeId = <<2:256>>,
    Address = {{127,0,0,1}, 8081},
    Metadata = #{realm => <<"test">>, capabilities => [pubsub, rpc]},
    Member = macula_membership_member:new(NodeId, Address, Metadata),

    ?assertEqual(Metadata, macula_membership_member:metadata(Member)).

%%%===================================================================
%%% Status Transition Tests
%%%===================================================================

%% Test: alive → suspect transition
alive_to_suspect_test() ->
    Member = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    Suspected = macula_membership_member:mark_suspect(Member),

    ?assertEqual(suspect, macula_membership_member:status(Suspected)),
    ?assertEqual(0, macula_membership_member:incarnation(Suspected)).

%% Test: suspect → dead transition
suspect_to_dead_test() ->
    Member = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    Suspected = macula_membership_member:mark_suspect(Member),
    Dead = macula_membership_member:mark_dead(Suspected),

    ?assertEqual(dead, macula_membership_member:status(Dead)).

%% Test: suspect → alive transition (refutation)
suspect_to_alive_refutation_test() ->
    Member = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    Suspected = macula_membership_member:mark_suspect(Member),
    Refuted = macula_membership_member:mark_alive(Suspected, 1),

    ?assertEqual(alive, macula_membership_member:status(Refuted)),
    ?assertEqual(1, macula_membership_member:incarnation(Refuted)).

%% Test: cannot go from dead to alive
dead_stays_dead_test() ->
    Member = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    Dead = macula_membership_member:mark_dead(Member),
    StillDead = macula_membership_member:mark_alive(Dead, 2),

    ?assertEqual(dead, macula_membership_member:status(StillDead)).

%%%===================================================================
%%% Incarnation Tests
%%%===================================================================

%% Test: incarnation increments on refutation
incarnation_increments_test() ->
    Member = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    ?assertEqual(0, macula_membership_member:incarnation(Member)),

    Refuted = macula_membership_member:mark_alive(Member, 1),
    ?assertEqual(1, macula_membership_member:incarnation(Refuted)),

    Refuted2 = macula_membership_member:mark_alive(Refuted, 2),
    ?assertEqual(2, macula_membership_member:incarnation(Refuted2)).

%% Test: higher incarnation wins in merge
higher_incarnation_wins_test() ->
    Member1 = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    Member2 = macula_membership_member:mark_alive(Member1, 5),

    %% Try to merge with lower incarnation (should be ignored)
    SuspectLow = macula_membership_member:mark_suspect(Member1), % inc=0
    Merged = macula_membership_member:merge(Member2, SuspectLow),

    ?assertEqual(alive, macula_membership_member:status(Merged)),
    ?assertEqual(5, macula_membership_member:incarnation(Merged)).

%% Test: same incarnation, suspect beats alive
same_incarnation_suspect_wins_test() ->
    Member1 = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    Suspect = macula_membership_member:mark_suspect(Member1),

    Merged = macula_membership_member:merge(Member1, Suspect),
    ?assertEqual(suspect, macula_membership_member:status(Merged)).

%% Test: dead always wins
dead_always_wins_test() ->
    Member = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    Dead = macula_membership_member:mark_dead(Member),
    AliveHighInc = macula_membership_member:mark_alive(Member, 10),

    Merged = macula_membership_member:merge(AliveHighInc, Dead),
    ?assertEqual(dead, macula_membership_member:status(Merged)).

%%%===================================================================
%%% Comparison Tests
%%%===================================================================

%% Test: compare members by incarnation and status
compare_members_test() ->
    Base = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    Higher = macula_membership_member:mark_alive(Base, 5),
    Suspect = macula_membership_member:mark_suspect(Base),
    Dead = macula_membership_member:mark_dead(Base),

    %% Higher incarnation is newer
    ?assertEqual(gt, macula_membership_member:compare(Higher, Base)),
    ?assertEqual(lt, macula_membership_member:compare(Base, Higher)),

    %% Same incarnation: suspect > alive
    ?assertEqual(gt, macula_membership_member:compare(Suspect, Base)),

    %% Dead always wins
    ?assertEqual(gt, macula_membership_member:compare(Dead, Higher)),
    ?assertEqual(gt, macula_membership_member:compare(Dead, Suspect)).

%%%===================================================================
%%% Accessor Tests
%%%===================================================================

%% Test: all accessors return correct values
accessors_test() ->
    NodeId = <<42:256>>,
    Address = {{192,168,1,1}, 9000},
    Metadata = #{test => true},
    Member = macula_membership_member:new(NodeId, Address, Metadata),
    Suspect = macula_membership_member:mark_suspect(Member),

    ?assertEqual(NodeId, macula_membership_member:node_id(Suspect)),
    ?assertEqual(Address, macula_membership_member:address(Suspect)),
    ?assertEqual(suspect, macula_membership_member:status(Suspect)),
    ?assertEqual(0, macula_membership_member:incarnation(Suspect)),
    ?assertEqual(Metadata, macula_membership_member:metadata(Suspect)).
