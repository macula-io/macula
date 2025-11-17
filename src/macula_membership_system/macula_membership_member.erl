%%%-------------------------------------------------------------------
%%% @doc
%%% Member record and state transitions for SWIM protocol.
%%% Represents a single node in the membership list.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_membership_member).

%% API
-export([
    new/2,
    new/3,
    node_id/1,
    address/1,
    status/1,
    incarnation/1,
    metadata/1,
    mark_alive/2,
    mark_suspect/1,
    mark_dead/1,
    merge/2,
    compare/2
]).

%% Types
-type status() :: alive | suspect | dead.
-type member() :: #{
    node_id := binary(),
    address := {inet:ip_address(), inet:port_number()},
    status := status(),
    incarnation := non_neg_integer(),
    metadata := map()
}.

-export_type([member/0, status/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create a new member with alive status and incarnation 0.
-spec new(binary(), {inet:ip_address(), inet:port_number()}) -> member().
new(NodeId, Address) ->
    new(NodeId, Address, #{}).

%% @doc Create a new member with custom metadata.
-spec new(binary(), {inet:ip_address(), inet:port_number()}, map()) -> member().
new(NodeId, Address, Metadata) ->
    #{
        node_id => NodeId,
        address => Address,
        status => alive,
        incarnation => 0,
        metadata => Metadata
    }.

%% @doc Get node ID.
-spec node_id(member()) -> binary().
node_id(#{node_id := NodeId}) -> NodeId.

%% @doc Get address.
-spec address(member()) -> {inet:ip_address(), inet:port_number()}.
address(#{address := Address}) -> Address.

%% @doc Get status.
-spec status(member()) -> status().
status(#{status := Status}) -> Status.

%% @doc Get incarnation number.
-spec incarnation(member()) -> non_neg_integer().
incarnation(#{incarnation := Inc}) -> Inc.

%% @doc Get metadata.
-spec metadata(member()) -> map().
metadata(#{metadata := Meta}) -> Meta.

%% @doc Mark member as alive with new incarnation (refutation).
%% Dead members cannot be revived.
-spec mark_alive(member(), non_neg_integer()) -> member().
mark_alive(#{status := dead} = Member, _NewIncarnation) ->
    %% Dead members stay dead
    Member;
mark_alive(Member, NewIncarnation) ->
    Member#{
        status => alive,
        incarnation => NewIncarnation
    }.

%% @doc Mark member as suspect (failed to respond to ping).
-spec mark_suspect(member()) -> member().
mark_suspect(Member) ->
    Member#{status => suspect}.

%% @doc Mark member as dead (confirmed failure).
-spec mark_dead(member()) -> member().
mark_dead(Member) ->
    Member#{status => dead}.

%% @doc Merge two member states, keeping the most recent information.
%% Rules:
%%   1. Dead always wins
%%   2. Higher incarnation wins
%%   3. Same incarnation: suspect > alive
-spec merge(member(), member()) -> member().
merge(#{status := dead} = M1, _M2) ->
    M1;  % Dead wins
merge(_M1, #{status := dead} = M2) ->
    M2;  % Dead wins
merge(#{incarnation := Inc1} = M1, #{incarnation := Inc2}) when Inc1 > Inc2 ->
    M1;  % Higher incarnation wins
merge(#{incarnation := Inc1}, #{incarnation := Inc2} = M2) when Inc2 > Inc1 ->
    M2;  % Higher incarnation wins
merge(#{incarnation := Inc, status := Status1} = M1,
      #{incarnation := Inc, status := Status2} = M2) ->
    %% Same incarnation: suspect beats alive
    case {Status1, Status2} of
        {suspect, alive} -> M1;
        {alive, suspect} -> M2;
        _ -> M1  % Same status, keep first
    end.

%% @doc Compare two members to determine which is more recent.
%% Returns: gt (M1 is newer), lt (M1 is older), eq (same)
-spec compare(member(), member()) -> gt | lt | eq.
compare(#{status := dead}, #{status := dead}) ->
    eq;
compare(#{status := dead}, _) ->
    gt;  % Dead wins
compare(_, #{status := dead}) ->
    lt;  % Dead wins
compare(#{incarnation := Inc1}, #{incarnation := Inc2}) when Inc1 > Inc2 ->
    gt;
compare(#{incarnation := Inc1}, #{incarnation := Inc2}) when Inc2 > Inc1 ->
    lt;
compare(#{incarnation := Inc, status := Status1},
        #{incarnation := Inc, status := Status2}) ->
    %% Same incarnation: compare status
    case {Status1, Status2} of
        {suspect, alive} -> gt;
        {alive, suspect} -> lt;
        _ -> eq
    end.
