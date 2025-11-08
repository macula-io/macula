%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_membership_list module.
%%% Tests written FIRST (TDD red phase).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_membership_list_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% List Creation Tests
%%%===================================================================

%% Test: new creates list with local node only
new_list_has_local_node_test() ->
    LocalNodeId = <<1:256>>,
    LocalAddress = {{127,0,0,1}, 8080},
    LocalMember = macula_membership_member:new(LocalNodeId, LocalAddress),

    List = macula_membership_list:new(LocalMember),

    ?assertEqual(1, macula_membership_list:size(List)),
    ?assertMatch({ok, _}, macula_membership_list:get_member(List, LocalNodeId)).

%%%===================================================================
%%% Member Management Tests
%%%===================================================================

%% Test: add_member increases size
add_member_increases_size_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    List = macula_membership_list:new(LocalMember),

    NewMember = macula_membership_member:new(<<2:256>>, {{127,0,0,1}, 8081}),
    List2 = macula_membership_list:add_member(List, NewMember),

    ?assertEqual(2, macula_membership_list:size(List2)).

%% Test: get_member returns member by node_id
get_member_returns_member_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    List = macula_membership_list:new(LocalMember),

    NodeId = <<2:256>>,
    NewMember = macula_membership_member:new(NodeId, {{127,0,0,1}, 8081}),
    List2 = macula_membership_list:add_member(List, NewMember),

    {ok, Retrieved} = macula_membership_list:get_member(List2, NodeId),
    ?assertEqual(NodeId, macula_membership_member:node_id(Retrieved)).

%% Test: get_member returns not_found for unknown node
get_member_not_found_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    List = macula_membership_list:new(LocalMember),

    ?assertEqual(not_found, macula_membership_list:get_member(List, <<99:256>>)).

%% Test: update_member modifies existing member
update_member_modifies_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    List = macula_membership_list:new(LocalMember),

    NodeId = <<2:256>>,
    Member = macula_membership_member:new(NodeId, {{127,0,0,1}, 8081}),
    List2 = macula_membership_list:add_member(List, Member),

    %% Mark as suspect
    Suspected = macula_membership_member:mark_suspect(Member),
    List3 = macula_membership_list:update_member(List2, Suspected),

    {ok, Retrieved} = macula_membership_list:get_member(List3, NodeId),
    ?assertEqual(suspect, macula_membership_member:status(Retrieved)).

%% Test: update_member with higher incarnation wins
update_member_higher_incarnation_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    List = macula_membership_list:new(LocalMember),

    NodeId = <<2:256>>,
    Member = macula_membership_member:new(NodeId, {{127,0,0,1}, 8081}),
    List2 = macula_membership_list:add_member(List, Member),

    %% Try to update with lower incarnation (should be ignored)
    OldSuspect = macula_membership_member:mark_suspect(Member), % inc=0
    List3 = macula_membership_list:update_member(List2, OldSuspect),

    %% Update with higher incarnation
    HigherAlive = macula_membership_member:mark_alive(Member, 5),
    List4 = macula_membership_list:update_member(List3, HigherAlive),

    {ok, Retrieved} = macula_membership_list:get_member(List4, NodeId),
    ?assertEqual(alive, macula_membership_member:status(Retrieved)),
    ?assertEqual(5, macula_membership_member:incarnation(Retrieved)).

%%%===================================================================
%%% Query Tests
%%%===================================================================

%% Test: get_alive_members returns only alive members
get_alive_members_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    List = macula_membership_list:new(LocalMember),

    Alive = macula_membership_member:new(<<2:256>>, {{127,0,0,1}, 8081}),
    Suspect = macula_membership_member:mark_suspect(
        macula_membership_member:new(<<3:256>>, {{127,0,0,1}, 8082})
    ),
    Dead = macula_membership_member:mark_dead(
        macula_membership_member:new(<<4:256>>, {{127,0,0,1}, 8083})
    ),

    List2 = macula_membership_list:add_member(List, Alive),
    List3 = macula_membership_list:add_member(List2, Suspect),
    List4 = macula_membership_list:add_member(List3, Dead),

    AliveMembers = macula_membership_list:get_alive_members(List4),
    ?assertEqual(2, length(AliveMembers)),  % Local + Alive

    Statuses = [macula_membership_member:status(M) || M <- AliveMembers],
    ?assert(lists:all(fun(S) -> S =:= alive end, Statuses)).

%% Test: get_suspect_members returns only suspect members
get_suspect_members_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    List = macula_membership_list:new(LocalMember),

    Alive = macula_membership_member:new(<<2:256>>, {{127,0,0,1}, 8081}),
    Suspect1 = macula_membership_member:mark_suspect(
        macula_membership_member:new(<<3:256>>, {{127,0,0,1}, 8082})
    ),
    Suspect2 = macula_membership_member:mark_suspect(
        macula_membership_member:new(<<4:256>>, {{127,0,0,1}, 8083})
    ),

    List2 = macula_membership_list:add_member(List, Alive),
    List3 = macula_membership_list:add_member(List2, Suspect1),
    List4 = macula_membership_list:add_member(List3, Suspect2),

    SuspectMembers = macula_membership_list:get_suspect_members(List4),
    ?assertEqual(2, length(SuspectMembers)),

    Statuses = [macula_membership_member:status(M) || M <- SuspectMembers],
    ?assert(lists:all(fun(S) -> S =:= suspect end, Statuses)).

%% Test: get_random_members returns N random members
get_random_members_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    List = macula_membership_list:new(LocalMember),

    %% Add 5 more members
    List2 = lists:foldl(
        fun(I, Acc) ->
            Member = macula_membership_member:new(<<I:256>>, {{127,0,0,1}, 8080 + I}),
            macula_membership_list:add_member(Acc, Member)
        end,
        List,
        lists:seq(2, 6)
    ),

    %% Get 3 random members
    Random = macula_membership_list:get_random_members(List2, 3),
    ?assertEqual(3, length(Random)),

    %% Should not include duplicates
    NodeIds = [macula_membership_member:node_id(M) || M <- Random],
    ?assertEqual(3, length(lists:usort(NodeIds))).

%% Test: get_random_members excludes specified node
get_random_members_excludes_test() ->
    LocalNodeId = <<1:256>>,
    LocalMember = macula_membership_member:new(LocalNodeId, {{127,0,0,1}, 8080}),
    List = macula_membership_list:new(LocalMember),

    %% Add 3 more members
    List2 = lists:foldl(
        fun(I, Acc) ->
            Member = macula_membership_member:new(<<I:256>>, {{127,0,0,1}, 8080 + I}),
            macula_membership_list:add_member(Acc, Member)
        end,
        List,
        lists:seq(2, 4)
    ),

    %% Get 2 random members excluding local node
    Random = macula_membership_list:get_random_members(List2, 2, LocalNodeId),
    ?assertEqual(2, length(Random)),

    %% Should not include local node
    NodeIds = [macula_membership_member:node_id(M) || M <- Random],
    ?assert(not lists:member(LocalNodeId, NodeIds)).

%%%===================================================================
%%% Remove Tests
%%%===================================================================

%% Test: remove_member decreases size
remove_member_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    List = macula_membership_list:new(LocalMember),

    NodeId = <<2:256>>,
    Member = macula_membership_member:new(NodeId, {{127,0,0,1}, 8081}),
    List2 = macula_membership_list:add_member(List, Member),

    ?assertEqual(2, macula_membership_list:size(List2)),

    List3 = macula_membership_list:remove_member(List2, NodeId),
    ?assertEqual(1, macula_membership_list:size(List3)),
    ?assertEqual(not_found, macula_membership_list:get_member(List3, NodeId)).

%%%===================================================================
%%% All Members Test
%%%===================================================================

%% Test: get_all_members returns complete list
get_all_members_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    List = macula_membership_list:new(LocalMember),

    List2 = lists:foldl(
        fun(I, Acc) ->
            Member = macula_membership_member:new(<<I:256>>, {{127,0,0,1}, 8080 + I}),
            macula_membership_list:add_member(Acc, Member)
        end,
        List,
        lists:seq(2, 5)
    ),

    AllMembers = macula_membership_list:get_all_members(List2),
    ?assertEqual(5, length(AllMembers)).
