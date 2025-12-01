%%%-------------------------------------------------------------------
%%% @doc
%%% K-bucket for Kademlia routing table.
%%% Stores up to k nodes with LRU eviction policy.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_bucket).

%% API
-export([
    new/1,
    add_node/2,
    remove_node/2,
    get_nodes/1,
    find_node/2,
    find_closest/3,
    has_node/2,
    update_timestamp/2,
    size/1,
    capacity/1
]).

%% Types
-type node_info() :: #{
    node_id := binary(),
    address := {inet:ip_address(), inet:port_number()},
    last_seen => integer()  % Optional timestamp
}.

-type bucket() :: #{
    capacity := pos_integer(),
    nodes := [node_info()]  % Ordered: head = oldest, tail = most recent
}.

-export_type([bucket/0, node_info/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create a new bucket with capacity k.
-spec new(pos_integer()) -> bucket().
new(Capacity) ->
    #{capacity => Capacity, nodes => []}.

%% @doc Add a node to the bucket.
%% If node exists, move to tail (most recent).
%% If bucket full, return {error, bucket_full}.
-spec add_node(bucket(), node_info()) -> bucket() | {error, bucket_full}.
add_node(Bucket, NodeInfo) ->
    NodeWithTimestamp = ensure_timestamp(NodeInfo),
    NodeId = maps:get(node_id, NodeWithTimestamp),
    do_add_node(Bucket, NodeId, NodeWithTimestamp).

%% Node already exists - move to tail
do_add_node(#{nodes := Nodes} = Bucket, NodeId, NodeInfo) when is_list(Nodes) ->
    case lists:keymember(NodeId, 1, nodes_to_tuples(Nodes)) of
        true ->
            UpdatedNodes = remove_by_id(Nodes, NodeId) ++ [NodeInfo],
            Bucket#{nodes => UpdatedNodes};
        false ->
            add_new_node(Bucket, NodeInfo)
    end.

%% Bucket has space - add new node
add_new_node(#{capacity := Capacity, nodes := Nodes} = Bucket, NodeInfo)
  when length(Nodes) < Capacity ->
    Bucket#{nodes => Nodes ++ [NodeInfo]};
%% Bucket full
add_new_node(#{capacity := Capacity, nodes := Nodes}, _NodeInfo)
  when length(Nodes) >= Capacity ->
    {error, bucket_full}.

%% Add timestamp if not present
ensure_timestamp(#{last_seen := _} = NodeInfo) ->
    NodeInfo;
ensure_timestamp(NodeInfo) ->
    NodeInfo#{last_seen => erlang:system_time(millisecond)}.

%% @doc Remove a node from the bucket.
-spec remove_node(bucket(), binary()) -> bucket().
remove_node(#{nodes := Nodes} = Bucket, NodeId) ->
    Bucket#{nodes => remove_by_id(Nodes, NodeId)}.

%% @doc Get all nodes in the bucket (ordered: oldest first).
-spec get_nodes(bucket()) -> [node_info()].
get_nodes(#{nodes := Nodes}) ->
    Nodes.

%% @doc Find a node by ID.
-spec find_node(bucket(), binary()) -> {ok, node_info()} | not_found.
find_node(#{nodes := Nodes}, NodeId) ->
    find_by_id(Nodes, NodeId).

%% @doc Find n closest nodes to target (sorted by XOR distance).
-spec find_closest(bucket(), binary(), pos_integer()) -> [node_info()].
find_closest(#{nodes := Nodes}, Target, N) ->
    WithDistance = [{distance_to(Target, Node), Node} || Node <- Nodes],
    Sorted = lists:keysort(1, WithDistance),
    [Node || {_Dist, Node} <- lists:sublist(Sorted, N)].

%% @doc Check if bucket contains node.
-spec has_node(bucket(), binary()) -> boolean().
has_node(#{nodes := Nodes}, NodeId) ->
    lists:keymember(NodeId, 1, nodes_to_tuples(Nodes)).

%% @doc Update node's last_seen timestamp (moves to tail).
-spec update_timestamp(bucket(), binary()) -> bucket().
update_timestamp(#{nodes := Nodes} = Bucket, NodeId) ->
    do_update_timestamp(Bucket, Nodes, NodeId).

do_update_timestamp(Bucket, Nodes, NodeId) ->
    case find_by_id(Nodes, NodeId) of
        {ok, Node} ->
            UpdatedNode = Node#{last_seen => erlang:system_time(millisecond)},
            Bucket#{nodes => remove_by_id(Nodes, NodeId) ++ [UpdatedNode]};
        not_found ->
            Bucket
    end.

%% @doc Get number of nodes in bucket.
-spec size(bucket()) -> non_neg_integer().
size(#{nodes := Nodes}) ->
    length(Nodes).

%% @doc Get bucket capacity.
-spec capacity(bucket()) -> pos_integer().
capacity(#{capacity := Capacity}) ->
    Capacity.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Convert nodes to tuples for efficient key-based lookups.
nodes_to_tuples(Nodes) ->
    [{maps:get(node_id, N), N} || N <- Nodes].

%% @doc Find node by ID using list comprehension.
-spec find_by_id([node_info()], binary()) -> {ok, node_info()} | not_found.
find_by_id(Nodes, NodeId) ->
    case [N || #{node_id := Id} = N <- Nodes, Id =:= NodeId] of
        [Node | _] -> {ok, Node};
        [] -> not_found
    end.

%% @doc Remove node by ID using list comprehension.
-spec remove_by_id([node_info()], binary()) -> [node_info()].
remove_by_id(Nodes, NodeId) ->
    [N || #{node_id := Id} = N <- Nodes, Id =/= NodeId].

%% @doc Calculate XOR distance between target and node.
%% Returns raw XOR binary (32 bytes) for Kademlia comparison.
-spec distance_to(binary(), node_info()) -> binary().
distance_to(Target, #{node_id := NodeId}) ->
    macula_routing_nodeid:distance(Target, NodeId).
