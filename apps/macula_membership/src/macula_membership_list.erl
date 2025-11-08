%%%-------------------------------------------------------------------
%%% @doc
%%% Membership list for SWIM protocol.
%%% Maintains cluster membership view with fast concurrent access.
%%% Uses map-based storage (could be ETS in production).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_membership_list).

%% API
-export([
    new/1,
    add_member/2,
    update_member/2,
    remove_member/2,
    get_member/2,
    get_alive_members/1,
    get_suspect_members/1,
    get_random_members/2,
    get_random_members/3,
    get_all_members/1,
    size/1
]).

%% Types
-type member_list() :: #{
    members := #{binary() => macula_membership_member:member()}
}.

-export_type([member_list/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create a new membership list with the local node.
-spec new(macula_membership_member:member()) -> member_list().
new(LocalMember) ->
    NodeId = macula_membership_member:node_id(LocalMember),
    #{
        members => #{NodeId => LocalMember}
    }.

%% @doc Add a new member to the list.
%% If member already exists, this is a no-op (use update_member instead).
-spec add_member(member_list(), macula_membership_member:member()) -> member_list().
add_member(#{members := Members} = List, Member) ->
    NodeId = macula_membership_member:node_id(Member),
    case maps:is_key(NodeId, Members) of
        true ->
            %% Already exists, don't overwrite
            List;
        false ->
            List#{members => Members#{NodeId => Member}}
    end.

%% @doc Update an existing member (or add if not present).
%% Uses merge semantics to resolve conflicts.
-spec update_member(member_list(), macula_membership_member:member()) -> member_list().
update_member(#{members := Members} = List, NewMember) ->
    NodeId = macula_membership_member:node_id(NewMember),
    case maps:get(NodeId, Members, undefined) of
        undefined ->
            %% Not present, add it
            List#{members => Members#{NodeId => NewMember}};
        OldMember ->
            %% Merge with existing
            Merged = macula_membership_member:merge(NewMember, OldMember),
            List#{members => Members#{NodeId => Merged}}
    end.

%% @doc Remove a member from the list.
-spec remove_member(member_list(), binary()) -> member_list().
remove_member(#{members := Members} = List, NodeId) ->
    List#{members => maps:remove(NodeId, Members)}.

%% @doc Get a member by node ID.
-spec get_member(member_list(), binary()) -> {ok, macula_membership_member:member()} | not_found.
get_member(#{members := Members}, NodeId) ->
    case maps:get(NodeId, Members, undefined) of
        undefined -> not_found;
        Member -> {ok, Member}
    end.

%% @doc Get all alive members.
-spec get_alive_members(member_list()) -> [macula_membership_member:member()].
get_alive_members(#{members := Members}) ->
    [M || M <- maps:values(Members), macula_membership_member:status(M) =:= alive].

%% @doc Get all suspect members.
-spec get_suspect_members(member_list()) -> [macula_membership_member:member()].
get_suspect_members(#{members := Members}) ->
    [M || M <- maps:values(Members), macula_membership_member:status(M) =:= suspect].

%% @doc Get N random members from the list.
-spec get_random_members(member_list(), pos_integer()) -> [macula_membership_member:member()].
get_random_members(List, N) ->
    get_random_members(List, N, undefined).

%% @doc Get N random members excluding specified node ID.
-spec get_random_members(member_list(), pos_integer(), binary() | undefined) ->
    [macula_membership_member:member()].
get_random_members(#{members := Members}, N, ExcludeNodeId) ->
    %% Get candidate members
    Candidates = case ExcludeNodeId of
        undefined ->
            maps:values(Members);
        NodeId ->
            [M || {K, M} <- maps:to_list(Members), K =/= NodeId]
    end,

    %% Shuffle and take N
    Shuffled = shuffle(Candidates),
    lists:sublist(Shuffled, N).

%% @doc Get all members.
-spec get_all_members(member_list()) -> [macula_membership_member:member()].
get_all_members(#{members := Members}) ->
    maps:values(Members).

%% @doc Get the number of members in the list.
-spec size(member_list()) -> non_neg_integer().
size(#{members := Members}) ->
    maps:size(Members).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Shuffle a list (Fisher-Yates algorithm).
-spec shuffle(list()) -> list().
shuffle(List) ->
    List1 = lists:zip(List, [rand:uniform() || _ <- List]),
    List2 = lists:sort(fun({_, R1}, {_, R2}) -> R1 =< R2 end, List1),
    [E || {E, _} <- List2].
