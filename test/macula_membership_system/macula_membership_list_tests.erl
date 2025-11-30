%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_membership_list module.
%%%
%%% Tests membership list operations and member lookup.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_membership_list_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% new/1 Tests
%%%===================================================================

new_creates_list_with_local_member_test() ->
    %% GIVEN: A local member
    LocalMember = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),

    %% WHEN: Creating a new list
    List = macula_membership_list:new(LocalMember),

    %% THEN: Should contain the local member
    ?assertEqual(1, macula_membership_list:size(List)),
    ?assertMatch({ok, _}, macula_membership_list:get_member(List, <<"local">>)).

%%%===================================================================
%%% add_member/2 Tests
%%%===================================================================

add_member_adds_new_member_test() ->
    %% GIVEN: A list with one member
    Local = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    List = macula_membership_list:new(Local),

    %% WHEN: Adding a new member
    NewMember = macula_membership_member:new(<<"remote">>, {{192,168,1,1}, 4000}),
    Updated = macula_membership_list:add_member(List, NewMember),

    %% THEN: Both members should be present
    ?assertEqual(2, macula_membership_list:size(Updated)),
    ?assertMatch({ok, _}, macula_membership_list:get_member(Updated, <<"remote">>)).

add_member_does_not_overwrite_existing_test() ->
    %% GIVEN: A list with a member
    Local = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),
    List = macula_membership_list:new(Local),

    %% WHEN: Adding member with same ID
    Duplicate = macula_membership_member:new(<<"node1">>, {{10,0,0,1}, 5000}),
    Updated = macula_membership_list:add_member(List, Duplicate),

    %% THEN: Original address should be preserved
    {ok, Member} = macula_membership_list:get_member(Updated, <<"node1">>),
    ?assertEqual({{127,0,0,1}, 4000}, macula_membership_member:address(Member)),
    ?assertEqual(1, macula_membership_list:size(Updated)).

%%%===================================================================
%%% update_member/2 Tests
%%%===================================================================

update_member_updates_existing_test() ->
    %% GIVEN: A list with a member
    Local = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),
    List = macula_membership_list:new(Local),

    %% WHEN: Updating with new status
    Updated = macula_membership_member:mark_suspect(Local),
    NewList = macula_membership_list:update_member(List, Updated),

    %% THEN: Status should be updated
    {ok, Member} = macula_membership_list:get_member(NewList, <<"node1">>),
    ?assertEqual(suspect, macula_membership_member:status(Member)).

update_member_adds_if_not_present_test() ->
    %% GIVEN: A list without the member
    Local = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    List = macula_membership_list:new(Local),

    %% WHEN: Updating a non-existent member
    NewMember = macula_membership_member:new(<<"new">>, {{10,0,0,1}, 4000}),
    Updated = macula_membership_list:update_member(List, NewMember),

    %% THEN: Member should be added
    ?assertEqual(2, macula_membership_list:size(Updated)),
    ?assertMatch({ok, _}, macula_membership_list:get_member(Updated, <<"new">>)).

update_member_uses_merge_semantics_test() ->
    %% GIVEN: A member with high incarnation
    Local = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),
    HighInc = macula_membership_member:mark_alive(Local, 10),
    List = macula_membership_list:new(HighInc),

    %% WHEN: Updating with lower incarnation
    LowInc = macula_membership_member:mark_alive(Local, 5),
    Updated = macula_membership_list:update_member(List, LowInc),

    %% THEN: Higher incarnation should be preserved (merge semantics)
    {ok, Member} = macula_membership_list:get_member(Updated, <<"node1">>),
    ?assertEqual(10, macula_membership_member:incarnation(Member)).

%%%===================================================================
%%% remove_member/2 Tests
%%%===================================================================

remove_member_removes_existing_test() ->
    %% GIVEN: A list with multiple members
    Local = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    Remote = macula_membership_member:new(<<"remote">>, {{10,0,0,1}, 4000}),
    List = macula_membership_list:add_member(macula_membership_list:new(Local), Remote),
    ?assertEqual(2, macula_membership_list:size(List)),

    %% WHEN: Removing a member
    Updated = macula_membership_list:remove_member(List, <<"remote">>),

    %% THEN: Member should be gone
    ?assertEqual(1, macula_membership_list:size(Updated)),
    ?assertEqual(not_found, macula_membership_list:get_member(Updated, <<"remote">>)).

remove_member_nonexistent_is_noop_test() ->
    %% GIVEN: A list
    Local = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    List = macula_membership_list:new(Local),

    %% WHEN: Removing non-existent member
    Updated = macula_membership_list:remove_member(List, <<"missing">>),

    %% THEN: List should be unchanged
    ?assertEqual(1, macula_membership_list:size(Updated)).

%%%===================================================================
%%% get_member/2 Tests
%%%===================================================================

get_member_returns_member_test() ->
    %% GIVEN: A list with a member
    Local = macula_membership_member:new(<<"node1">>, {{127,0,0,1}, 4000}),
    List = macula_membership_list:new(Local),

    %% WHEN: Getting the member
    Result = macula_membership_list:get_member(List, <<"node1">>),

    %% THEN: Should return the member
    ?assertMatch({ok, _}, Result).

get_member_not_found_test() ->
    %% GIVEN: A list without the member
    Local = macula_membership_member:new(<<"local">>, {{127,0,0,1}, 4000}),
    List = macula_membership_list:new(Local),

    %% WHEN: Getting non-existent member
    Result = macula_membership_list:get_member(List, <<"missing">>),

    %% THEN: Should return not_found
    ?assertEqual(not_found, Result).

%%%===================================================================
%%% get_alive_members/1 Tests
%%%===================================================================

get_alive_members_returns_only_alive_test() ->
    %% GIVEN: A list with alive and suspect members
    Local = macula_membership_member:new(<<"alive1">>, {{1,1,1,1}, 1}),
    Remote = macula_membership_member:new(<<"alive2">>, {{2,2,2,2}, 2}),
    Suspect = macula_membership_member:mark_suspect(
        macula_membership_member:new(<<"suspect">>, {{3,3,3,3}, 3})),

    List0 = macula_membership_list:new(Local),
    List1 = macula_membership_list:add_member(List0, Remote),
    List2 = macula_membership_list:update_member(List1, Suspect),

    %% WHEN: Getting alive members
    Alive = macula_membership_list:get_alive_members(List2),

    %% THEN: Should only return alive members
    ?assertEqual(2, length(Alive)),
    NodeIds = [macula_membership_member:node_id(M) || M <- Alive],
    ?assert(lists:member(<<"alive1">>, NodeIds)),
    ?assert(lists:member(<<"alive2">>, NodeIds)).

%%%===================================================================
%%% get_suspect_members/1 Tests
%%%===================================================================

get_suspect_members_returns_only_suspect_test() ->
    %% GIVEN: A list with alive and suspect members
    Local = macula_membership_member:new(<<"alive">>, {{1,1,1,1}, 1}),
    Suspect1 = macula_membership_member:mark_suspect(
        macula_membership_member:new(<<"suspect1">>, {{2,2,2,2}, 2})),
    Suspect2 = macula_membership_member:mark_suspect(
        macula_membership_member:new(<<"suspect2">>, {{3,3,3,3}, 3})),

    List0 = macula_membership_list:new(Local),
    List1 = macula_membership_list:update_member(List0, Suspect1),
    List2 = macula_membership_list:update_member(List1, Suspect2),

    %% WHEN: Getting suspect members
    Suspects = macula_membership_list:get_suspect_members(List2),

    %% THEN: Should only return suspect members
    ?assertEqual(2, length(Suspects)),
    NodeIds = [macula_membership_member:node_id(M) || M <- Suspects],
    ?assert(lists:member(<<"suspect1">>, NodeIds)),
    ?assert(lists:member(<<"suspect2">>, NodeIds)).

%%%===================================================================
%%% get_random_members/2,3 Tests
%%%===================================================================

get_random_members_returns_requested_count_test() ->
    %% GIVEN: A list with 5 members
    Members = [macula_membership_member:new(list_to_binary("node" ++ integer_to_list(I)),
                                            {{I,I,I,I}, I}) || I <- lists:seq(1, 5)],
    List = lists:foldl(fun(M, Acc) ->
        macula_membership_list:update_member(Acc, M)
    end, macula_membership_list:new(hd(Members)), tl(Members)),

    %% WHEN: Getting 3 random members
    Random = macula_membership_list:get_random_members(List, 3),

    %% THEN: Should return 3 members
    ?assertEqual(3, length(Random)).

get_random_members_excludes_specified_node_test() ->
    %% GIVEN: A list with 3 members
    M1 = macula_membership_member:new(<<"node1">>, {{1,1,1,1}, 1}),
    M2 = macula_membership_member:new(<<"node2">>, {{2,2,2,2}, 2}),
    M3 = macula_membership_member:new(<<"node3">>, {{3,3,3,3}, 3}),

    List0 = macula_membership_list:new(M1),
    List1 = macula_membership_list:add_member(List0, M2),
    List2 = macula_membership_list:add_member(List1, M3),

    %% WHEN: Getting random members excluding node1
    Random = macula_membership_list:get_random_members(List2, 10, <<"node1">>),

    %% THEN: Should not include node1
    NodeIds = [macula_membership_member:node_id(M) || M <- Random],
    ?assertNot(lists:member(<<"node1">>, NodeIds)),
    ?assertEqual(2, length(Random)).

get_random_members_returns_less_if_not_enough_test() ->
    %% GIVEN: A list with 2 members
    M1 = macula_membership_member:new(<<"node1">>, {{1,1,1,1}, 1}),
    M2 = macula_membership_member:new(<<"node2">>, {{2,2,2,2}, 2}),
    List = macula_membership_list:add_member(macula_membership_list:new(M1), M2),

    %% WHEN: Requesting more than available
    Random = macula_membership_list:get_random_members(List, 10),

    %% THEN: Should return all available
    ?assertEqual(2, length(Random)).

%%%===================================================================
%%% get_all_members/1 Tests
%%%===================================================================

get_all_members_returns_all_test() ->
    %% GIVEN: A list with members in various states
    M1 = macula_membership_member:new(<<"alive">>, {{1,1,1,1}, 1}),
    M2 = macula_membership_member:mark_suspect(
        macula_membership_member:new(<<"suspect">>, {{2,2,2,2}, 2})),
    M3 = macula_membership_member:mark_dead(
        macula_membership_member:new(<<"dead">>, {{3,3,3,3}, 3})),

    List0 = macula_membership_list:new(M1),
    List1 = macula_membership_list:update_member(List0, M2),
    List2 = macula_membership_list:update_member(List1, M3),

    %% WHEN: Getting all members
    All = macula_membership_list:get_all_members(List2),

    %% THEN: Should return all members regardless of status
    ?assertEqual(3, length(All)).

%%%===================================================================
%%% size/1 Tests
%%%===================================================================

size_returns_correct_count_test() ->
    %% GIVEN: A list with 3 members
    M1 = macula_membership_member:new(<<"n1">>, {{1,1,1,1}, 1}),
    M2 = macula_membership_member:new(<<"n2">>, {{2,2,2,2}, 2}),
    M3 = macula_membership_member:new(<<"n3">>, {{3,3,3,3}, 3}),

    List0 = macula_membership_list:new(M1),
    List1 = macula_membership_list:add_member(List0, M2),
    List2 = macula_membership_list:add_member(List1, M3),

    %% THEN: Size should be 3
    ?assertEqual(3, macula_membership_list:size(List2)).
