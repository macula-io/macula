%%%-------------------------------------------------------------------
%%% @doc
%%% Routing table for Kademlia DHT.
%%% Manages 256 k-buckets organized by XOR distance.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_table).

%% API
-export([
    new/2,
    add_node/2,
    remove_node/2,
    find_closest/3,
    get_bucket/2,
    bucket_size/2,
    get_all_nodes/1,
    update_timestamp/2,
    size/1,
    local_node_id/1,
    k/1
]).

%% Types
-type routing_table() :: #{
    local_node_id := binary(),
    k := pos_integer(),
    buckets := #{0..255 => macula_routing_bucket:bucket()}
}.

-export_type([routing_table/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create a new routing table.
-spec new(binary(), pos_integer()) -> routing_table().
new(LocalNodeId, K) ->
    #{
        local_node_id => LocalNodeId,
        k => K,
        buckets => #{}
    }.

%% @doc Add a node to the routing table.
%% Calculates bucket index and adds to appropriate bucket.
-spec add_node(routing_table(), macula_routing_bucket:node_info()) -> routing_table().
add_node(#{local_node_id := LocalNodeId, k := K, buckets := Buckets} = Table, NodeInfo) ->
    NodeId = maps:get(node_id, NodeInfo),

    %% Calculate bucket index
    BucketIndex = macula_routing_nodeid:bucket_index(LocalNodeId, NodeId),

    %% Don't add self (bucket index 256)
    case BucketIndex of
        256 ->
            Table;  % Same node, ignore
        _ ->
            %% Get or create bucket
            Bucket = case maps:get(BucketIndex, Buckets, undefined) of
                undefined -> macula_routing_bucket:new(K);
                ExistingBucket -> ExistingBucket
            end,

            %% Add node to bucket
            case macula_routing_bucket:add_node(Bucket, NodeInfo) of
                {error, bucket_full} ->
                    %% TODO: Implement bucket splitting or ping oldest node
                    Table;  % For now, just ignore
                UpdatedBucket ->
                    Table#{buckets => Buckets#{BucketIndex => UpdatedBucket}}
            end
    end.

%% @doc Remove a node from the routing table.
-spec remove_node(routing_table(), binary()) -> routing_table().
remove_node(#{local_node_id := LocalNodeId, buckets := Buckets} = Table, NodeId) ->
    %% Calculate bucket index
    BucketIndex = macula_routing_nodeid:bucket_index(LocalNodeId, NodeId),

    case maps:get(BucketIndex, Buckets, undefined) of
        undefined ->
            Table;  % Bucket doesn't exist
        Bucket ->
            UpdatedBucket = macula_routing_bucket:remove_node(Bucket, NodeId),
            Table#{buckets => Buckets#{BucketIndex => UpdatedBucket}}
    end.

%% @doc Find k closest nodes to target.
-spec find_closest(routing_table(), binary(), pos_integer()) -> [macula_routing_bucket:node_info()].
find_closest(#{local_node_id := LocalNodeId, buckets := Buckets}, Target, K) ->
    %% Start with target's bucket
    TargetBucketIndex = macula_routing_nodeid:bucket_index(LocalNodeId, Target),

    %% Collect nodes from nearby buckets (expanding outward)
    AllNodes = collect_nodes_near_bucket(Buckets, TargetBucketIndex),

    %% Sort by distance to target
    Sorted = lists:sort(
        fun(A, B) ->
            DistA = macula_routing_nodeid:distance(Target, maps:get(node_id, A)),
            DistB = macula_routing_nodeid:distance(Target, maps:get(node_id, B)),
            DistA =< DistB
        end,
        AllNodes
    ),

    %% Return k closest
    lists:sublist(Sorted, K).

%% @doc Get bucket by index.
-spec get_bucket(routing_table(), 0..255) -> macula_routing_bucket:bucket().
get_bucket(#{k := K, buckets := Buckets}, BucketIndex) ->
    case maps:get(BucketIndex, Buckets, undefined) of
        undefined -> macula_routing_bucket:new(K);
        Bucket -> Bucket
    end.

%% @doc Get size of a specific bucket.
-spec bucket_size(routing_table(), 0..255) -> non_neg_integer().
bucket_size(Table, BucketIndex) ->
    Bucket = get_bucket(Table, BucketIndex),
    macula_routing_bucket:size(Bucket).

%% @doc Get all nodes from all buckets.
-spec get_all_nodes(routing_table()) -> [macula_routing_bucket:node_info()].
get_all_nodes(#{buckets := Buckets}) ->
    lists:flatten([
        macula_routing_bucket:get_nodes(Bucket)
        || Bucket <- maps:values(Buckets)
    ]).

%% @doc Update timestamp for a node (moves to tail in its bucket).
-spec update_timestamp(routing_table(), binary()) -> routing_table().
update_timestamp(#{local_node_id := LocalNodeId, buckets := Buckets} = Table, NodeId) ->
    %% Calculate bucket index
    BucketIndex = macula_routing_nodeid:bucket_index(LocalNodeId, NodeId),

    case maps:get(BucketIndex, Buckets, undefined) of
        undefined ->
            Table;  % Node not in table
        Bucket ->
            UpdatedBucket = macula_routing_bucket:update_timestamp(Bucket, NodeId),
            Table#{buckets => Buckets#{BucketIndex => UpdatedBucket}}
    end.

%% @doc Get total number of nodes in routing table.
-spec size(routing_table()) -> non_neg_integer().
size(#{buckets := Buckets}) ->
    lists:sum([
        macula_routing_bucket:size(Bucket)
        || Bucket <- maps:values(Buckets)
    ]).

%% @doc Get local node ID.
-spec local_node_id(routing_table()) -> binary().
local_node_id(#{local_node_id := NodeId}) ->
    NodeId.

%% @doc Get k (bucket capacity).
-spec k(routing_table()) -> pos_integer().
k(#{k := K}) ->
    K.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Collect nodes from buckets near target bucket (expanding outward).
-spec collect_nodes_near_bucket(#{0..255 => macula_routing_bucket:bucket()}, 0..256) -> [macula_routing_bucket:node_info()].
collect_nodes_near_bucket(Buckets, StartIndex) when StartIndex =:= 256 ->
    %% Special case: target is local node, collect from all buckets
    lists:flatten([
        macula_routing_bucket:get_nodes(Bucket)
        || Bucket <- maps:values(Buckets)
    ]);
collect_nodes_near_bucket(Buckets, StartIndex) ->
    %% Expand outward from start index
    Indices = expand_indices(StartIndex, 0, 255, []),
    lists:flatten([
        case maps:get(Index, Buckets, undefined) of
            undefined -> [];
            Bucket -> macula_routing_bucket:get_nodes(Bucket)
        end
        || Index <- Indices
    ]).

%% @doc Generate list of bucket indices expanding outward from start.
-spec expand_indices(non_neg_integer(), non_neg_integer(), non_neg_integer(), [non_neg_integer()]) -> [non_neg_integer()].
expand_indices(Start, Min, Max, Acc) ->
    %% Start with start index, then alternate +1, -1, +2, -2, etc.
    expand_indices(Start, Start, 1, Min, Max, [Start | Acc]).

expand_indices(_Start, _Current, Offset, Min, Max, Acc) when Offset > Max - Min ->
    lists:reverse(Acc);
expand_indices(Start, Current, Offset, Min, Max, Acc) ->
    %% Try adding offset
    Acc2 = case Current + Offset of
        Next when Next =< Max ->
            [Next | Acc];
        _ ->
            Acc
    end,

    %% Try subtracting offset
    Acc3 = case Current - Offset of
        Prev when Prev >= Min ->
            [Prev | Acc2];
        _ ->
            Acc2
    end,

    expand_indices(Start, Current, Offset + 1, Min, Max, Acc3).
