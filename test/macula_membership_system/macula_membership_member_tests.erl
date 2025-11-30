%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_membership_member module.
%%%
%%% Tests member creation, status transitions, and merge semantics.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_membership_member_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% new/2,3 Tests
%%%===================================================================

new_creates_alive_member_test() ->
    %% WHEN: Creating a new member
    Member = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),

    %% THEN: Should be alive with incarnation 0
    ?assertEqual(<<"node1">>, macula_membership_member:node_id(Member)),
    ?assertEqual({{127,0,0,1}, 4000}, macula_membership_member:address(Member)),
    ?assertEqual(alive, macula_membership_member:status(Member)),
    ?assertEqual(0, macula_membership_member:incarnation(Member)),
    ?assertEqual(#{}, macula_membership_member:metadata(Member)).

new_with_metadata_test() ->
    %% WHEN: Creating a member with metadata
    Meta = #{role => gateway, region => <<"us-east">>},
    Member = macula_membership_member:new(<<"node1">>, {{10,0,0,1}, 9443}, Meta),

    %% THEN: Should have the metadata
    ?assertEqual(Meta, macula_membership_member:metadata(Member)).

%%%===================================================================
%%% mark_alive/2 Tests
%%%===================================================================

mark_alive_updates_incarnation_test() ->
    %% GIVEN: An alive member
    Member = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),

    %% WHEN: Marking alive with higher incarnation
    Updated = macula_membership_member:mark_alive(Member, 5),

    %% THEN: Incarnation should be updated
    ?assertEqual(alive, macula_membership_member:status(Updated)),
    ?assertEqual(5, macula_membership_member:incarnation(Updated)).

mark_alive_revives_suspect_test() ->
    %% GIVEN: A suspect member
    Member = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),
    Suspect = macula_membership_member:mark_suspect(Member),
    ?assertEqual(suspect, macula_membership_member:status(Suspect)),

    %% WHEN: Marking alive
    Revived = macula_membership_member:mark_alive(Suspect, 1),

    %% THEN: Should be alive
    ?assertEqual(alive, macula_membership_member:status(Revived)),
    ?assertEqual(1, macula_membership_member:incarnation(Revived)).

mark_alive_does_not_revive_dead_test() ->
    %% GIVEN: A dead member
    Member = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),
    Dead = macula_membership_member:mark_dead(Member),
    ?assertEqual(dead, macula_membership_member:status(Dead)),

    %% WHEN: Trying to mark alive
    StillDead = macula_membership_member:mark_alive(Dead, 100),

    %% THEN: Should remain dead (dead is permanent)
    ?assertEqual(dead, macula_membership_member:status(StillDead)).

%%%===================================================================
%%% mark_suspect/1 Tests
%%%===================================================================

mark_suspect_changes_status_test() ->
    %% GIVEN: An alive member
    Member = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),

    %% WHEN: Marking suspect
    Suspect = macula_membership_member:mark_suspect(Member),

    %% THEN: Status should be suspect
    ?assertEqual(suspect, macula_membership_member:status(Suspect)),
    %% Other fields unchanged
    ?assertEqual(<<"node1">>, macula_membership_member:node_id(Suspect)),
    ?assertEqual(0, macula_membership_member:incarnation(Suspect)).

%%%===================================================================
%%% mark_dead/1 Tests
%%%===================================================================

mark_dead_changes_status_test() ->
    %% GIVEN: A suspect member
    Member = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),
    Suspect = macula_membership_member:mark_suspect(Member),

    %% WHEN: Marking dead
    Dead = macula_membership_member:mark_dead(Suspect),

    %% THEN: Status should be dead
    ?assertEqual(dead, macula_membership_member:status(Dead)).

mark_dead_from_alive_test() ->
    %% GIVEN: An alive member
    Member = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),

    %% WHEN: Marking dead directly
    Dead = macula_membership_member:mark_dead(Member),

    %% THEN: Status should be dead
    ?assertEqual(dead, macula_membership_member:status(Dead)).

%%%===================================================================
%%% merge/2 Tests - SWIM Merge Semantics
%%%===================================================================

merge_dead_always_wins_test() ->
    %% GIVEN: One dead, one alive member
    Alive = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),
    Dead = macula_membership_member:mark_dead(Alive),

    %% WHEN: Merging in either order
    Result1 = macula_membership_member:merge(Dead, Alive),
    Result2 = macula_membership_member:merge(Alive, Dead),

    %% THEN: Dead always wins
    ?assertEqual(dead, macula_membership_member:status(Result1)),
    ?assertEqual(dead, macula_membership_member:status(Result2)).

merge_higher_incarnation_wins_test() ->
    %% GIVEN: Two members with different incarnations
    M1 = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),
    M1Updated = macula_membership_member:mark_alive(M1, 5),
    M2 = macula_membership_member:mark_alive(M1, 2),

    %% WHEN: Merging
    Result = macula_membership_member:merge(M1Updated, M2),

    %% THEN: Higher incarnation wins
    ?assertEqual(5, macula_membership_member:incarnation(Result)).

merge_same_incarnation_suspect_wins_test() ->
    %% GIVEN: Same incarnation, different status
    M1 = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),
    Alive = macula_membership_member:mark_alive(M1, 3),
    Suspect = macula_membership_member:mark_suspect(macula_membership_member:mark_alive(M1, 3)),

    %% WHEN: Merging
    Result = macula_membership_member:merge(Alive, Suspect),

    %% THEN: Suspect wins over alive at same incarnation
    ?assertEqual(suspect, macula_membership_member:status(Result)).

merge_same_status_keeps_first_test() ->
    %% GIVEN: Two alive members with same incarnation
    M1 = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),
    M2 = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),

    %% WHEN: Merging
    Result = macula_membership_member:merge(M1, M2),

    %% THEN: First is kept (they're equal)
    ?assertEqual(alive, macula_membership_member:status(Result)).

%%%===================================================================
%%% compare/2 Tests
%%%===================================================================

compare_dead_is_greater_test() ->
    %% GIVEN: Dead and alive members
    Alive = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),
    Dead = macula_membership_member:mark_dead(Alive),

    %% WHEN: Comparing
    ?assertEqual(gt, macula_membership_member:compare(Dead, Alive)),
    ?assertEqual(lt, macula_membership_member:compare(Alive, Dead)).

compare_both_dead_is_equal_test() ->
    %% GIVEN: Two dead members
    M1 = macula_membership_member:mark_dead(macula_membership_member:new(<<"n1">>, {{1,1,1,1}, 1})),
    M2 = macula_membership_member:mark_dead(macula_membership_member:new(<<"n2">>, {{2,2,2,2}, 2})),

    %% WHEN: Comparing
    ?assertEqual(eq, macula_membership_member:compare(M1, M2)).

compare_higher_incarnation_is_greater_test() ->
    %% GIVEN: Different incarnations
    M1 = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),
    Higher = macula_membership_member:mark_alive(M1, 10),
    Lower = macula_membership_member:mark_alive(M1, 5),

    %% WHEN: Comparing
    ?assertEqual(gt, macula_membership_member:compare(Higher, Lower)),
    ?assertEqual(lt, macula_membership_member:compare(Lower, Higher)).

compare_same_incarnation_suspect_greater_test() ->
    %% GIVEN: Same incarnation, different status
    M1 = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),
    Alive = macula_membership_member:mark_alive(M1, 3),
    Suspect = macula_membership_member:mark_suspect(macula_membership_member:mark_alive(M1, 3)),

    %% WHEN: Comparing
    ?assertEqual(gt, macula_membership_member:compare(Suspect, Alive)),
    ?assertEqual(lt, macula_membership_member:compare(Alive, Suspect)).

compare_equal_test() ->
    %% GIVEN: Identical members
    M1 = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),
    M2 = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),

    %% WHEN: Comparing
    ?assertEqual(eq, macula_membership_member:compare(M1, M2)).
