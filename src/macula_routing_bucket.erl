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
    #{
        capacity => Capacity,
        nodes => []
    }.

%% @doc Add a node to the bucket.
%% If node exists, move to tail (most recent).
%% If bucket full, return {error, bucket_full}.
-spec add_node(bucket(), node_info()) -> bucket() | {error, bucket_full}.
add_node(#{capacity := Capacity, nodes := Nodes} = Bucket, NodeInfo) ->
    NodeId = maps:get(node_id, NodeInfo),

    %% Add timestamp if not present
    NodeWithTimestamp = case maps:is_key(last_seen, NodeInfo) of
        true -> NodeInfo;
        false -> NodeInfo#{last_seen => erlang:system_time(millisecond)}
    end,

    case find_node_in_list(Nodes, NodeId) of
        {found, _ExistingNode} ->
            %% Node exists, move to tail (remove and re-add)
            NodesWithoutNode = remove_node_from_list(Nodes, NodeId),
            Bucket#{nodes => NodesWithoutNode ++ [NodeWithTimestamp]};

        not_found ->
            %% New node
            case length(Nodes) < Capacity of
                true ->
                    %% Space available, add to tail
                    Bucket#{nodes => Nodes ++ [NodeWithTimestamp]};
                false ->
                    %% Bucket full
                    {error, bucket_full}
            end
    end.

%% @doc Remove a node from the bucket.
-spec remove_node(bucket(), binary()) -> bucket().
remove_node(#{nodes := Nodes} = Bucket, NodeId) ->
    Bucket#{nodes => remove_node_from_list(Nodes, NodeId)}.

%% @doc Get all nodes in the bucket (ordered: oldest first).
-spec get_nodes(bucket()) -> [node_info()].
get_nodes(#{nodes := Nodes}) ->
    Nodes.

%% @doc Find a node by ID.
-spec find_node(bucket(), binary()) -> {ok, node_info()} | not_found.
find_node(#{nodes := Nodes}, NodeId) ->
    case find_node_in_list(Nodes, NodeId) of
        {found, Node} -> {ok, Node};
        not_found -> not_found
    end.

%% @doc Find n closest nodes to target (sorted by XOR distance).
-spec find_closest(bucket(), binary(), pos_integer()) -> [node_info()].
find_closest(#{nodes := Nodes}, Target, N) ->
    %% Sort nodes by distance to target
    Sorted = lists:sort(
        fun(A, B) ->
            DistA = macula_routing_nodeid:distance(Target, maps:get(node_id, A)),
            DistB = macula_routing_nodeid:distance(Target, maps:get(node_id, B)),
            DistA =< DistB
        end,
        Nodes
    ),
    lists:sublist(Sorted, N).

%% @doc Check if bucket contains node.
-spec has_node(bucket(), binary()) -> boolean().
has_node(#{nodes := Nodes}, NodeId) ->
    case find_node_in_list(Nodes, NodeId) of
        {found, _} -> true;
        not_found -> false
    end.

%% @doc Update node's last_seen timestamp (moves to tail).
-spec update_timestamp(bucket(), binary()) -> bucket().
update_timestamp(#{nodes := Nodes} = Bucket, NodeId) ->
    case find_node_in_list(Nodes, NodeId) of
        {found, Node} ->
            %% Remove and re-add with updated timestamp
            NodesWithoutNode = remove_node_from_list(Nodes, NodeId),
            UpdatedNode = Node#{last_seen => erlang:system_time(millisecond)},
            Bucket#{nodes => NodesWithoutNode ++ [UpdatedNode]};
        not_found ->
            Bucket  % No change
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

%% @doc Find node in list by ID.
-spec find_node_in_list([node_info()], binary()) -> {found, node_info()} | not_found.
find_node_in_list([], _NodeId) ->
    not_found;
find_node_in_list([Node | Rest], NodeId) ->
    case maps:get(node_id, Node) of
        NodeId -> {found, Node};
        _ -> find_node_in_list(Rest, NodeId)
    end.

%% @doc Remove node from list by ID.
-spec remove_node_from_list([node_info()], binary()) -> [node_info()].
remove_node_from_list(Nodes, NodeId) ->
    lists:filter(
        fun(Node) ->
            maps:get(node_id, Node) =/= NodeId
        end,
        Nodes
    ).
