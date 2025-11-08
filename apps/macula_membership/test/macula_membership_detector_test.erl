%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_membership_detector module.
%%% Tests written FIRST (TDD red phase).
%%% Note: These are unit tests for detector logic, not full SWIM integration.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_membership_detector_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Detector Creation Tests
%%%===================================================================

%% Test: new creates detector with local member
new_detector_test() ->
    LocalNodeId = <<1:256>>,
    LocalAddress = {{127,0,0,1}, 8080},
    LocalMember = macula_membership_member:new(LocalNodeId, LocalAddress),

    Config = #{
        protocol_period => 1000,
        indirect_count => 3,
        suspect_timeout => 5000
    },

    State = macula_membership_detector:new(LocalMember, Config),

    ?assertEqual(LocalNodeId, macula_membership_detector:local_node_id(State)),
    ?assertEqual(1000, macula_membership_detector:protocol_period(State)).

%%%===================================================================
%%% Member Selection Tests
%%%===================================================================

%% Test: select_probe_target returns random alive member
select_probe_target_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    State = macula_membership_detector:new(LocalMember, #{
        protocol_period => 1000,
        indirect_count => 3,
        suspect_timeout => 5000
    }),

    %% Add some members
    Member2 = macula_membership_member:new(<<2:256>>, {{127,0,0,1}, 8081}),
    Member3 = macula_membership_member:new(<<3:256>>, {{127,0,0,1}, 8082}),

    State2 = macula_membership_detector:add_member(State, Member2),
    State3 = macula_membership_detector:add_member(State2, Member3),

    %% Select target (should not be local)
    case macula_membership_detector:select_probe_target(State3) of
        {ok, Target, _State4} ->
            NodeId = macula_membership_member:node_id(Target),
            ?assert(NodeId =/= <<1:256>>);
        none ->
            ?assert(false, "Should have selected a target")
    end.

%% Test: select_probe_target returns none when no other members
select_probe_target_none_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    State = macula_membership_detector:new(LocalMember, #{
        protocol_period => 1000,
        indirect_count => 3,
        suspect_timeout => 5000
    }),

    ?assertEqual(none, macula_membership_detector:select_probe_target(State)).

%%%===================================================================
%%% Status Update Tests
%%%===================================================================

%% Test: mark_member_suspect updates status
mark_member_suspect_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    State = macula_membership_detector:new(LocalMember, #{
        protocol_period => 1000,
        indirect_count => 3,
        suspect_timeout => 5000
    }),

    Member2 = macula_membership_member:new(<<2:256>>, {{127,0,0,1}, 8081}),
    State2 = macula_membership_detector:add_member(State, Member2),

    %% Mark as suspect
    State3 = macula_membership_detector:mark_suspect(State2, <<2:256>>),

    %% Verify status changed
    {ok, Updated} = macula_membership_detector:get_member(State3, <<2:256>>),
    ?assertEqual(suspect, macula_membership_member:status(Updated)).

%% Test: mark_member_dead updates status
mark_member_dead_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    State = macula_membership_detector:new(LocalMember, #{
        protocol_period => 1000,
        indirect_count => 3,
        suspect_timeout => 5000
    }),

    Member2 = macula_membership_member:new(<<2:256>>, {{127,0,0,1}, 8081}),
    State2 = macula_membership_detector:add_member(State, Member2),

    %% Mark as dead
    State3 = macula_membership_detector:mark_dead(State2, <<2:256>>),

    %% Verify status changed
    {ok, Updated} = macula_membership_detector:get_member(State3, <<2:256>>),
    ?assertEqual(dead, macula_membership_member:status(Updated)).

%%%===================================================================
%%% Refutation Tests
%%%===================================================================

%% Test: refute_suspicion increments incarnation
refute_suspicion_test() ->
    LocalNodeId = <<1:256>>,
    LocalMember = macula_membership_member:new(LocalNodeId, {{127,0,0,1}, 8080}),
    State = macula_membership_detector:new(LocalMember, #{
        protocol_period => 1000,
        indirect_count => 3,
        suspect_timeout => 5000
    }),

    %% Refute (as if we received suspicion about ourselves)
    State2 = macula_membership_detector:refute_suspicion(State),

    %% Check incarnation increased
    {ok, LocalUpdated} = macula_membership_detector:get_member(State2, LocalNodeId),
    ?assertEqual(1, macula_membership_member:incarnation(LocalUpdated)),
    ?assertEqual(alive, macula_membership_member:status(LocalUpdated)).

%%%===================================================================
%%% Gossip Integration Tests
%%%===================================================================

%% Test: get_gossip_updates returns recent changes
get_gossip_updates_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    State = macula_membership_detector:new(LocalMember, #{
        protocol_period => 1000,
        indirect_count => 3,
        suspect_timeout => 5000
    }),

    Member2 = macula_membership_member:new(<<2:256>>, {{127,0,0,1}, 8081}),
    State2 = macula_membership_detector:add_member(State, Member2),

    %% Mark as suspect (creates gossip update)
    State3 = macula_membership_detector:mark_suspect(State2, <<2:256>>),

    %% Get gossip updates
    Updates = macula_membership_detector:get_gossip_updates(State3, 10),
    ?assert(length(Updates) > 0).

%% Test: apply_gossip_updates merges received updates
apply_gossip_updates_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    State = macula_membership_detector:new(LocalMember, #{
        protocol_period => 1000,
        indirect_count => 3,
        suspect_timeout => 5000
    }),

    %% Apply updates about node 2
    Updates = [{<<2:256>>, alive, 0}],
    State2 = macula_membership_detector:apply_gossip_updates(State, Updates),

    %% Verify member was added
    ?assertMatch({ok, _}, macula_membership_detector:get_member(State2, <<2:256>>)).

%%%===================================================================
%%% Membership Query Tests
%%%===================================================================

%% Test: get_alive_members returns only alive
get_alive_members_test() ->
    LocalMember = macula_membership_member:new(<<1:256>>, {{127,0,0,1}, 8080}),
    State = macula_membership_detector:new(LocalMember, #{
        protocol_period => 1000,
        indirect_count => 3,
        suspect_timeout => 5000
    }),

    Alive = macula_membership_member:new(<<2:256>>, {{127,0,0,1}, 8081}),
    Suspect = macula_membership_member:mark_suspect(
        macula_membership_member:new(<<3:256>>, {{127,0,0,1}, 8082})
    ),

    State2 = macula_membership_detector:add_member(State, Alive),
    State3 = macula_membership_detector:add_member(State2, Suspect),

    AliveMembers = macula_membership_detector:get_alive_members(State3),
    ?assertEqual(2, length(AliveMembers)).  % Local + node 2
