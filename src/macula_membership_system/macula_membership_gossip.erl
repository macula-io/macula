%%%-------------------------------------------------------------------
%%% @doc
%%% Gossip dissemination for SWIM protocol.
%%% Tracks membership updates and provides them for piggybacking.
%%% Uses exponential decay: log(N) messages per update.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_membership_gossip).

%% API
-export([
    new/0,
    add_update/4,
    get_updates/2,
    mark_transmitted/2,
    merge_updates/2,
    prune/2
]).

%% Types
-type update() :: {binary(), macula_membership_member:status(), non_neg_integer(), non_neg_integer()}.

-type gossip_state() :: #{
    updates => #{binary() => {macula_membership_member:status(), non_neg_integer(), non_neg_integer(), integer()}}
}.

-export_type([update/0, gossip_state/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create a new gossip state.
-spec new() -> gossip_state().
new() ->
    #{updates => #{}}.

%% @doc Add a membership update to gossip.
%% If a more recent update exists, it's replaced.
-spec add_update(gossip_state(), binary(), macula_membership_member:status(), non_neg_integer()) ->
    gossip_state().
add_update(#{updates := Updates} = State, NodeId, Status, Incarnation) ->
    Timestamp = erlang:system_time(millisecond),
    Existing = maps:get(NodeId, Updates, undefined),
    NewUpdate = merge_update(Existing, Status, Incarnation, Timestamp),
    State#{updates => Updates#{NodeId => NewUpdate}}.

%% @private New update (no existing entry)
merge_update(undefined, Status, Incarnation, Timestamp) ->
    {Status, Incarnation, 0, Timestamp};
%% @private Higher incarnation always wins
merge_update({_OldStatus, OldInc, _TransmitCount, _OldTimestamp}, Status, Incarnation, Timestamp)
  when Incarnation > OldInc ->
    {Status, Incarnation, 0, Timestamp};
%% @private Same incarnation, different status - update (suspect > alive)
merge_update({OldStatus, OldInc, _TransmitCount, _OldTimestamp}, Status, Incarnation, Timestamp)
  when Incarnation =:= OldInc, Status =/= OldStatus ->
    {Status, Incarnation, 0, Timestamp};
%% @private Same or older incarnation, keep existing
merge_update({OldStatus, OldInc, TransmitCount, OldTimestamp}, _Status, _Incarnation, _Timestamp) ->
    {OldStatus, OldInc, TransmitCount, OldTimestamp}.

%% @doc Get updates to piggyback on messages.
%% Returns most recent updates first, limited by max_updates.
-spec get_updates(gossip_state(), pos_integer()) -> [update()].
get_updates(#{updates := Updates}, MaxUpdates) ->
    %% Convert to list with timestamps for sorting
    UpdateList = [{NodeId, Status, Inc, TransmitCount, Timestamp} ||
                  {NodeId, {Status, Inc, TransmitCount, Timestamp}} <- maps:to_list(Updates)],

    %% Sort by timestamp (most recent first)
    Sorted = lists:sort(
        fun({_, _, _, _, T1}, {_, _, _, _, T2}) -> T1 >= T2 end,
        UpdateList
    ),

    %% Take max and format
    [{NodeId, Status, Inc, TransmitCount} ||
     {NodeId, Status, Inc, TransmitCount, _} <- lists:sublist(Sorted, MaxUpdates)].

%% @doc Mark an update as transmitted (increment transmit count).
-spec mark_transmitted(gossip_state(), binary()) -> gossip_state().
mark_transmitted(#{updates := Updates} = State, NodeId) ->
    case maps:get(NodeId, Updates, undefined) of
        undefined ->
            State;  % Not found
        {Status, Inc, TransmitCount, Timestamp} ->
            NewUpdate = {Status, Inc, TransmitCount + 1, Timestamp},
            State#{updates => Updates#{NodeId => NewUpdate}}
    end.

%% @doc Merge received gossip updates into local state.
%% Uses SWIM merge semantics (higher incarnation wins, etc.).
-spec merge_updates(gossip_state(), [{binary(), macula_membership_member:status(), non_neg_integer()}]) ->
    gossip_state().
merge_updates(State, ReceivedUpdates) ->
    lists:foldl(
        fun({NodeId, Status, Incarnation}, Acc) ->
            add_update(Acc, NodeId, Status, Incarnation)
        end,
        State,
        ReceivedUpdates
    ).

%% @doc Prune updates that have been transmitted enough times.
%% Target is typically log(N) where N is cluster size.
-spec prune(gossip_state(), non_neg_integer()) -> gossip_state().
prune(#{updates := Updates} = State, TargetTransmitCount) ->
    %% Remove updates with transmit count >= target
    Pruned = maps:filter(
        fun(_NodeId, {_Status, _Inc, TransmitCount, _Timestamp}) ->
            TransmitCount < TargetTransmitCount
        end,
        Updates
    ),
    State#{updates => Pruned}.
