%%%-------------------------------------------------------------------
%%% @doc
%%% SWIM failure detector (pure logic, no GenServer).
%%% Orchestrates member list, gossip, and protocol timing.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_membership_detector).

%% API
-export([
    new/2,
    local_node_id/1,
    protocol_period/1,
    add_member/2,
    get_member/2,
    get_alive_members/1,
    select_probe_target/1,
    mark_suspect/2,
    mark_dead/2,
    refute_suspicion/1,
    get_gossip_updates/2,
    apply_gossip_updates/2
]).

%% Types
-type detector_state() :: #{
    local_node_id := binary(),
    protocol_period := pos_integer(),
    indirect_count := pos_integer(),
    suspect_timeout := pos_integer(),
    member_list := macula_membership_list:member_list(),
    gossip := macula_membership_gossip:gossip_state()
}.

-export_type([detector_state/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create a new detector state.
-spec new(macula_membership_member:member(), map()) -> detector_state().
new(LocalMember, Config) ->
    LocalNodeId = macula_membership_member:node_id(LocalMember),
    #{
        local_node_id => LocalNodeId,
        protocol_period => maps:get(protocol_period, Config, 1000),
        indirect_count => maps:get(indirect_count, Config, 3),
        suspect_timeout => maps:get(suspect_timeout, Config, 5000),
        member_list => macula_membership_list:new(LocalMember),
        gossip => macula_membership_gossip:new()
    }.

%% @doc Get local node ID.
-spec local_node_id(detector_state()) -> binary().
local_node_id(#{local_node_id := NodeId}) -> NodeId.

%% @doc Get protocol period.
-spec protocol_period(detector_state()) -> pos_integer().
protocol_period(#{protocol_period := Period}) -> Period.

%% @doc Add a member to the list.
-spec add_member(detector_state(), macula_membership_member:member()) -> detector_state().
add_member(#{member_list := List} = State, Member) ->
    State#{member_list => macula_membership_list:add_member(List, Member)}.

%% @doc Get a member by node ID.
-spec get_member(detector_state(), binary()) -> {ok, macula_membership_member:member()} | not_found.
get_member(#{member_list := List}, NodeId) ->
    macula_membership_list:get_member(List, NodeId).

%% @doc Get all alive members.
-spec get_alive_members(detector_state()) -> [macula_membership_member:member()].
get_alive_members(#{member_list := List}) ->
    macula_membership_list:get_alive_members(List).

%% @doc Select a random member to probe (excluding self).
-spec select_probe_target(detector_state()) -> {ok, macula_membership_member:member(), detector_state()} | none.
select_probe_target(#{local_node_id := LocalNodeId, member_list := List} = State) ->
    case macula_membership_list:get_random_members(List, 1, LocalNodeId) of
        [Target] ->
            {ok, Target, State};
        [] ->
            none
    end.

%% @doc Mark a member as suspect.
-spec mark_suspect(detector_state(), binary()) -> detector_state().
mark_suspect(#{member_list := List, gossip := Gossip} = State, NodeId) ->
    case macula_membership_list:get_member(List, NodeId) of
        {ok, Member} ->
            %% Update member status
            Suspected = macula_membership_member:mark_suspect(Member),
            List2 = macula_membership_list:update_member(List, Suspected),

            %% Add to gossip
            Status = macula_membership_member:status(Suspected),
            Inc = macula_membership_member:incarnation(Suspected),
            Gossip2 = macula_membership_gossip:add_update(Gossip, NodeId, Status, Inc),

            State#{member_list => List2, gossip => Gossip2};
        not_found ->
            State
    end.

%% @doc Mark a member as dead.
-spec mark_dead(detector_state(), binary()) -> detector_state().
mark_dead(#{member_list := List, gossip := Gossip} = State, NodeId) ->
    case macula_membership_list:get_member(List, NodeId) of
        {ok, Member} ->
            %% Update member status
            Dead = macula_membership_member:mark_dead(Member),
            List2 = macula_membership_list:update_member(List, Dead),

            %% Add to gossip
            Status = macula_membership_member:status(Dead),
            Inc = macula_membership_member:incarnation(Dead),
            Gossip2 = macula_membership_gossip:add_update(Gossip, NodeId, Status, Inc),

            State#{member_list => List2, gossip => Gossip2};
        not_found ->
            State
    end.

%% @doc Refute suspicion (increment local incarnation).
-spec refute_suspicion(detector_state()) -> detector_state().
refute_suspicion(#{local_node_id := LocalNodeId, member_list := List, gossip := Gossip} = State) ->
    case macula_membership_list:get_member(List, LocalNodeId) of
        {ok, LocalMember} ->
            %% Increment incarnation
            CurrentInc = macula_membership_member:incarnation(LocalMember),
            NewInc = CurrentInc + 1,
            Refuted = macula_membership_member:mark_alive(LocalMember, NewInc),

            %% Update in list
            List2 = macula_membership_list:update_member(List, Refuted),

            %% Add to gossip (broadcast refutation)
            Gossip2 = macula_membership_gossip:add_update(Gossip, LocalNodeId, alive, NewInc),

            State#{member_list => List2, gossip => Gossip2};
        not_found ->
            State
    end.

%% @doc Get gossip updates to piggyback on messages.
-spec get_gossip_updates(detector_state(), pos_integer()) -> [macula_membership_gossip:update()].
get_gossip_updates(#{gossip := Gossip}, MaxUpdates) ->
    macula_membership_gossip:get_updates(Gossip, MaxUpdates).

%% @doc Apply received gossip updates.
-spec apply_gossip_updates(detector_state(), [{binary(), macula_membership_member:status(), non_neg_integer()}]) ->
    detector_state().
apply_gossip_updates(#{member_list := List, gossip := Gossip} = State, Updates) ->
    %% Merge into gossip
    Gossip2 = macula_membership_gossip:merge_updates(Gossip, Updates),

    %% Apply to member list
    List2 = lists:foldl(
        fun({NodeId, Status, Incarnation}, AccList) ->
            case macula_membership_list:get_member(AccList, NodeId) of
                {ok, ExistingMember} ->
                    %% Update existing member
                    case Status of
                        alive ->
                            Updated = macula_membership_member:mark_alive(ExistingMember, Incarnation),
                            macula_membership_list:update_member(AccList, Updated);
                        suspect ->
                            Updated = macula_membership_member:mark_suspect(ExistingMember),
                            macula_membership_list:update_member(AccList, Updated);
                        dead ->
                            Updated = macula_membership_member:mark_dead(ExistingMember),
                            macula_membership_list:update_member(AccList, Updated)
                    end;
                not_found ->
                    %% Add new member (if not dead)
                    case Status of
                        dead ->
                            AccList;  % Don't add already-dead members
                        _ ->
                            %% Create member with placeholder address (should come from discovery)
                            NewMember = macula_membership_member:new(NodeId, {{0,0,0,0}, 0}),
                            Updated = case Status of
                                alive -> macula_membership_member:mark_alive(NewMember, Incarnation);
                                suspect -> macula_membership_member:mark_suspect(NewMember)
                            end,
                            macula_membership_list:add_member(AccList, Updated)
                    end
            end
        end,
        List,
        Updates
    ),

    State#{member_list => List2, gossip => Gossip2}.
