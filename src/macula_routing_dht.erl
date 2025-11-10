%%%-------------------------------------------------------------------
%%% @doc
%%% Core DHT algorithms for Kademlia routing.
%%% Implements iterative lookup, store, and find operations.
%%% Pure functions - no GenServer, designed to be called by macula_routing_server.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_dht).

%% API
-export([
    iterative_find_node/4,
    store_value/6,
    find_value/4,
    update_closest/4,
    select_alpha/3
]).

%% Types
-type query_fn() :: fun((macula_routing_bucket:node_info(), binary()) ->
    {ok, [macula_routing_bucket:node_info()]} |
    {value, term()} |
    {nodes, [macula_routing_bucket:node_info()]} |
    {error, term()}).

-type store_fn() :: fun((macula_routing_bucket:node_info(), binary(), term()) -> ok | {error, term()}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Iterative lookup to find k closest nodes to target.
%% Uses alpha concurrent queries (default: 3).
-spec iterative_find_node(
    macula_routing_table:routing_table(),
    binary(),
    pos_integer(),
    query_fn()
) -> {ok, [macula_routing_bucket:node_info()]}.
iterative_find_node(RoutingTable, Target, K, QueryFn) ->
    %% 1. Get k closest from local routing table
    InitialClosest = macula_routing_table:find_closest(RoutingTable, Target, K),

    %% 2. Perform iterative lookup with alpha=3 concurrency
    Alpha = 3,
    FinalClosest = iterative_lookup(InitialClosest, Target, K, Alpha, [], QueryFn),

    {ok, FinalClosest}.

%% @doc Store value at k closest nodes to key.
-spec store_value(
    macula_routing_table:routing_table(),
    binary(),
    term(),
    pos_integer(),
    query_fn(),
    store_fn()
) -> ok.
store_value(RoutingTable, Key, Value, K, QueryFn, StoreFn) ->
    %% 1. Find k closest nodes to key
    {ok, ClosestNodes} = iterative_find_node(RoutingTable, Key, K, QueryFn),

    %% 2. Store value at each of the k closest
    lists:foreach(
        fun(Node) ->
            StoreFn(Node, Key, Value)
        end,
        ClosestNodes
    ),

    ok.

%% @doc Find value in DHT.
%% Returns {ok, Value} if found, {nodes, [NodeInfo]} if not found.
-spec find_value(
    macula_routing_table:routing_table(),
    binary(),
    pos_integer(),
    query_fn()
) -> {ok, term()} | {nodes, [macula_routing_bucket:node_info()]}.
find_value(RoutingTable, Key, K, QueryFn) ->
    %% Get initial closest nodes
    InitialClosest = macula_routing_table:find_closest(RoutingTable, Key, K),

    %% Perform iterative lookup, but stop if value found
    Alpha = 3,
    iterative_find_value(InitialClosest, Key, K, Alpha, [], QueryFn).

%% @doc Update closest set with new nodes, maintaining k closest and removing duplicates.
-spec update_closest(
    [macula_routing_bucket:node_info()],
    [macula_routing_bucket:node_info()],
    binary(),
    pos_integer()
) -> [macula_routing_bucket:node_info()].
update_closest(CurrentClosest, NewNodes, Target, K) ->
    %% Combine current and new
    Combined = CurrentClosest ++ NewNodes,

    %% Remove duplicates by node_id
    Deduplicated = deduplicate_nodes(Combined),

    %% Sort by distance to target
    Sorted = lists:sort(
        fun(A, B) ->
            DistA = macula_routing_nodeid:distance(Target, maps:get(node_id, A)),
            DistB = macula_routing_nodeid:distance(Target, maps:get(node_id, B)),
            DistA =< DistB
        end,
        Deduplicated
    ),

    %% Take k closest
    lists:sublist(Sorted, K).

%% @doc Select up to alpha unqueried nodes from closest set.
-spec select_alpha(
    [macula_routing_bucket:node_info()],
    [binary()],
    pos_integer()
) -> [macula_routing_bucket:node_info()].
select_alpha(Closest, Queried, Alpha) ->
    %% Filter out already queried nodes
    Unqueried = lists:filter(
        fun(Node) ->
            NodeId = maps:get(node_id, Node),
            not lists:member(NodeId, Queried)
        end,
        Closest
    ),

    %% Take up to alpha
    lists:sublist(Unqueried, Alpha).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Iterative lookup algorithm for FIND_NODE.
-spec iterative_lookup(
    [macula_routing_bucket:node_info()],
    binary(),
    pos_integer(),
    pos_integer(),
    [binary()],
    query_fn()
) -> [macula_routing_bucket:node_info()].
iterative_lookup(Closest, Target, K, Alpha, Queried, QueryFn) ->
    %% Select alpha nodes to query
    ToQuery = select_alpha(Closest, Queried, Alpha),

    case ToQuery of
        [] ->
            %% No more nodes to query, return current closest
            Closest;

        _ ->
            %% Query nodes in parallel (simplified: sequential for now)
            {NewNodes, NewQueried} = query_nodes(ToQuery, Target, Queried, QueryFn),

            %% Update closest set with responses
            UpdatedClosest = update_closest(Closest, NewNodes, Target, K),

            %% Check if we found closer nodes
            case found_closer_nodes(Closest, UpdatedClosest, K) of
                true ->
                    %% Continue iterating
                    iterative_lookup(UpdatedClosest, Target, K, Alpha, NewQueried, QueryFn);
                false ->
                    %% No closer nodes found, we're done
                    UpdatedClosest
            end
    end.

%% @doc Iterative lookup for FIND_VALUE (stops when value found).
-spec iterative_find_value(
    [macula_routing_bucket:node_info()],
    binary(),
    pos_integer(),
    pos_integer(),
    [binary()],
    query_fn()
) -> {ok, term()} | {nodes, [macula_routing_bucket:node_info()]}.
iterative_find_value(Closest, Key, K, Alpha, Queried, QueryFn) ->
    %% Select alpha nodes to query
    ToQuery = select_alpha(Closest, Queried, Alpha),

    case ToQuery of
        [] ->
            %% No more nodes to query, value not found
            {nodes, Closest};

        _ ->
            %% Query nodes for value
            case query_nodes_for_value(ToQuery, Key, Queried, QueryFn) of
                {value, Value} ->
                    %% Found the value!
                    {ok, Value};

                {nodes, NewNodes, NewQueried} ->
                    %% Update closest set
                    UpdatedClosest = update_closest(Closest, NewNodes, Key, K),

                    %% Continue searching
                    case found_closer_nodes(Closest, UpdatedClosest, K) of
                        true ->
                            iterative_find_value(UpdatedClosest, Key, K, Alpha, NewQueried, QueryFn);
                        false ->
                            {nodes, UpdatedClosest}
                    end
            end
    end.

%% @doc Query nodes and collect responses.
-spec query_nodes(
    [macula_routing_bucket:node_info()],
    binary(),
    [binary()],
    query_fn()
) -> {[macula_routing_bucket:node_info()], [binary()]}.
query_nodes(Nodes, Target, Queried, QueryFn) ->
    lists:foldl(
        fun(Node, {AccNodes, AccQueried}) ->
            NodeId = maps:get(node_id, Node),
            case QueryFn(Node, Target) of
                {ok, ResponseNodes} ->
                    {AccNodes ++ ResponseNodes, [NodeId | AccQueried]};
                {error, _Reason} ->
                    %% Query failed, just mark as queried
                    {AccNodes, [NodeId | AccQueried]}
            end
        end,
        {[], Queried},
        Nodes
    ).

%% @doc Query nodes for value (FIND_VALUE).
-spec query_nodes_for_value(
    [macula_routing_bucket:node_info()],
    binary(),
    [binary()],
    query_fn()
) -> {value, term()} | {nodes, [macula_routing_bucket:node_info()], [binary()]}.
query_nodes_for_value([], _Key, Queried, _QueryFn) ->
    {nodes, [], Queried};
query_nodes_for_value([Node | Rest], Key, Queried, QueryFn) ->
    NodeId = maps:get(node_id, Node),

    case QueryFn(Node, Key) of
        {value, Value} ->
            %% Found the value!
            {value, Value};

        {nodes, ResponseNodes} ->
            %% Continue querying
            case query_nodes_for_value(Rest, Key, [NodeId | Queried], QueryFn) of
                {value, Value} ->
                    {value, Value};
                {nodes, AccNodes, AccQueried} ->
                    {nodes, ResponseNodes ++ AccNodes, AccQueried}
            end;

        {error, _Reason} ->
            %% Query failed, try next node
            query_nodes_for_value(Rest, Key, [NodeId | Queried], QueryFn)
    end.

%% @doc Check if we found closer nodes (convergence check).
-spec found_closer_nodes(
    [macula_routing_bucket:node_info()],
    [macula_routing_bucket:node_info()],
    pos_integer()
) -> boolean().
found_closer_nodes(OldClosest, NewClosest, K) ->
    %% Take k from each
    OldK = lists:sublist(OldClosest, K),
    NewK = lists:sublist(NewClosest, K),

    %% Extract node IDs
    OldIds = lists:sort([maps:get(node_id, N) || N <- OldK]),
    NewIds = lists:sort([maps:get(node_id, N) || N <- NewK]),

    %% If node IDs changed, we found closer nodes
    OldIds =/= NewIds.

%% @doc Remove duplicate nodes (by node_id).
-spec deduplicate_nodes([macula_routing_bucket:node_info()]) -> [macula_routing_bucket:node_info()].
deduplicate_nodes(Nodes) ->
    %% Use map to track seen node IDs
    {_, Deduplicated} = lists:foldl(
        fun(Node, {Seen, Acc}) ->
            NodeId = maps:get(node_id, Node),
            case maps:is_key(NodeId, Seen) of
                true ->
                    %% Already seen, skip
                    {Seen, Acc};
                false ->
                    %% New node, add to result
                    {Seen#{NodeId => true}, [Node | Acc]}
            end
        end,
        {#{}, []},
        Nodes
    ),

    lists:reverse(Deduplicated).
