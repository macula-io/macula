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
add_node(#{local_node_id := LocalNodeId} = Table, NodeInfo) ->
    NodeId = maps:get(node_id, NodeInfo),
    BucketIndex = macula_routing_nodeid:bucket_index(LocalNodeId, NodeId),
    do_add_node(Table, BucketIndex, NodeInfo).

%% Self node (bucket index 256) - ignore
do_add_node(Table, 256, _NodeInfo) ->
    Table;
%% Add to appropriate bucket
do_add_node(#{k := K, buckets := Buckets} = Table, BucketIndex, NodeInfo) ->
    Bucket = maps:get(BucketIndex, Buckets, macula_routing_bucket:new(K)),
    add_to_bucket(Table, Buckets, BucketIndex, Bucket, NodeInfo).

%% Add node to bucket. If bucket is full, the node is ignored.
%% Standard Kademlia behavior - full buckets indicate well-known nodes.
add_to_bucket(Table, _Buckets, _BucketIndex, Bucket, NodeInfo) ->
    case macula_routing_bucket:add_node(Bucket, NodeInfo) of
        {error, bucket_full} -> Table;
        UpdatedBucket -> update_bucket(Table, _BucketIndex, UpdatedBucket)
    end.

update_bucket(#{buckets := Buckets} = Table, BucketIndex, UpdatedBucket) ->
    Table#{buckets => Buckets#{BucketIndex => UpdatedBucket}}.

%% @doc Remove a node from the routing table.
-spec remove_node(routing_table(), binary()) -> routing_table().
remove_node(#{local_node_id := LocalNodeId} = Table, NodeId) ->
    BucketIndex = macula_routing_nodeid:bucket_index(LocalNodeId, NodeId),
    do_remove_node(Table, BucketIndex, NodeId).

do_remove_node(#{buckets := Buckets} = Table, BucketIndex, NodeId) ->
    case maps:find(BucketIndex, Buckets) of
        {ok, Bucket} ->
            UpdatedBucket = macula_routing_bucket:remove_node(Bucket, NodeId),
            update_bucket(Table, BucketIndex, UpdatedBucket);
        error ->
            Table
    end.

%% @doc Find k closest nodes to target.
-spec find_closest(routing_table(), binary(), pos_integer()) -> [macula_routing_bucket:node_info()].
find_closest(#{local_node_id := LocalNodeId, buckets := Buckets}, Target, K) ->
    TargetBucketIndex = macula_routing_nodeid:bucket_index(LocalNodeId, Target),
    AllNodes = collect_nodes_near_bucket(Buckets, TargetBucketIndex),
    sort_by_distance_and_take(AllNodes, Target, K).

sort_by_distance_and_take(Nodes, Target, K) ->
    WithDistance = [{distance_to(Target, N), N} || N <- Nodes],
    Sorted = lists:keysort(1, WithDistance),
    [Node || {_Dist, Node} <- lists:sublist(Sorted, K)].

distance_to(Target, #{node_id := NodeId}) ->
    macula_routing_nodeid:distance(Target, NodeId).

%% @doc Get bucket by index.
-spec get_bucket(routing_table(), 0..255) -> macula_routing_bucket:bucket().
get_bucket(#{k := K, buckets := Buckets}, BucketIndex) ->
    maps:get(BucketIndex, Buckets, macula_routing_bucket:new(K)).

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
update_timestamp(#{local_node_id := LocalNodeId} = Table, NodeId) ->
    BucketIndex = macula_routing_nodeid:bucket_index(LocalNodeId, NodeId),
    do_update_timestamp(Table, BucketIndex, NodeId).

do_update_timestamp(#{buckets := Buckets} = Table, BucketIndex, NodeId) ->
    case maps:find(BucketIndex, Buckets) of
        {ok, Bucket} ->
            UpdatedBucket = macula_routing_bucket:update_timestamp(Bucket, NodeId),
            update_bucket(Table, BucketIndex, UpdatedBucket);
        error ->
            Table
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
collect_nodes_near_bucket(Buckets, 256) ->
    %% Target is local node - collect from all buckets
    lists:flatmap(fun macula_routing_bucket:get_nodes/1, maps:values(Buckets));
collect_nodes_near_bucket(Buckets, StartIndex) ->
    Indices = expand_indices(StartIndex, 0, 255),
    lists:flatmap(fun(Index) -> get_bucket_nodes(Buckets, Index) end, Indices).

get_bucket_nodes(Buckets, Index) ->
    case maps:find(Index, Buckets) of
        {ok, Bucket} -> macula_routing_bucket:get_nodes(Bucket);
        error -> []
    end.

%% @doc Generate list of bucket indices expanding outward from start.
-spec expand_indices(non_neg_integer(), non_neg_integer(), non_neg_integer()) -> [non_neg_integer()].
expand_indices(Start, Min, Max) ->
    expand_from_center(Start, 1, Min, Max, [Start]).

expand_from_center(_Start, Offset, Min, Max, Acc) when Offset > Max - Min ->
    lists:reverse(Acc);
expand_from_center(Start, Offset, Min, Max, Acc) ->
    Acc2 = maybe_add_index(Start + Offset, Max, Acc),
    Acc3 = maybe_add_index(Start - Offset, Min, Acc2),
    expand_from_center(Start, Offset + 1, Min, Max, Acc3).

maybe_add_index(Index, Max, Acc) when Index =< Max, Index >= 0 ->
    [Index | Acc];
maybe_add_index(_Index, _Max, Acc) ->
    Acc.
