%%%-------------------------------------------------------------------
%%% @doc
%%% Core DHT algorithms for Kademlia routing.
%%% Implements iterative lookup, store, and find operations.
%%% Pure functions - no GenServer, designed to be called by macula_routing_server.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_routing_dht).

%% API - Core DHT algorithms
-export([
    iterative_find_node/4,
    store_value/6,
    find_value/4,
    update_closest/4,
    select_alpha/3
]).

%% API - Simple key-value interface (used by macula_dist_discovery)
-export([
    store/2,
    delete/1,
    find/1,
    subscribe/2,
    notify_store/2,
    notify_delete/1
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
    Combined = CurrentClosest ++ NewNodes,
    Deduplicated = deduplicate_nodes(Combined),
    sort_by_distance_and_take(Deduplicated, Target, K).

sort_by_distance_and_take(Nodes, Target, K) ->
    WithDistance = [{distance_to(Target, N), N} || N <- Nodes],
    Sorted = lists:keysort(1, WithDistance),
    [Node || {_Dist, Node} <- lists:sublist(Sorted, K)].

distance_to(Target, #{node_id := NodeId}) ->
    macula_routing_nodeid:distance(Target, NodeId).

%% @doc Select up to alpha unqueried nodes from closest set.
-spec select_alpha(
    [macula_routing_bucket:node_info()],
    [binary()],
    pos_integer()
) -> [macula_routing_bucket:node_info()].
select_alpha(Closest, Queried, Alpha) ->
    QueriedSet = sets:from_list(Queried),
    Unqueried = [N || #{node_id := Id} = N <- Closest, not sets:is_element(Id, QueriedSet)],
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
    ToQuery = select_alpha(Closest, Queried, Alpha),
    do_iterative_lookup(ToQuery, Closest, Target, K, Alpha, Queried, QueryFn).

%% No more nodes to query - return current closest
do_iterative_lookup([], Closest, _Target, _K, _Alpha, _Queried, _QueryFn) ->
    Closest;
%% Query nodes and continue if closer found
do_iterative_lookup(ToQuery, Closest, Target, K, Alpha, Queried, QueryFn) ->
    {NewNodes, NewQueried} = query_nodes(ToQuery, Target, Queried, QueryFn),
    UpdatedClosest = update_closest(Closest, NewNodes, Target, K),
    continue_if_closer(Closest, UpdatedClosest, Target, K, Alpha, NewQueried, QueryFn).

continue_if_closer(OldClosest, NewClosest, Target, K, Alpha, Queried, QueryFn) ->
    case found_closer_nodes(OldClosest, NewClosest, K) of
        true -> iterative_lookup(NewClosest, Target, K, Alpha, Queried, QueryFn);
        false -> NewClosest
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
    ToQuery = select_alpha(Closest, Queried, Alpha),
    do_iterative_find_value(ToQuery, Closest, Key, K, Alpha, Queried, QueryFn).

%% No more nodes to query
do_iterative_find_value([], Closest, _Key, _K, _Alpha, _Queried, _QueryFn) ->
    {nodes, Closest};
%% Query nodes for value
do_iterative_find_value(ToQuery, Closest, Key, K, Alpha, Queried, QueryFn) ->
    handle_value_query_result(
        query_nodes_for_value(ToQuery, Key, Queried, QueryFn),
        Closest, Key, K, Alpha, QueryFn
    ).

handle_value_query_result({value, Value}, _Closest, _Key, _K, _Alpha, _QueryFn) ->
    {ok, Value};
handle_value_query_result({nodes, NewNodes, NewQueried}, Closest, Key, K, Alpha, QueryFn) ->
    UpdatedClosest = update_closest(Closest, NewNodes, Key, K),
    case found_closer_nodes(Closest, UpdatedClosest, K) of
        true -> iterative_find_value(UpdatedClosest, Key, K, Alpha, NewQueried, QueryFn);
        false -> {nodes, UpdatedClosest}
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
    OldIds = extract_node_ids(lists:sublist(OldClosest, K)),
    NewIds = extract_node_ids(lists:sublist(NewClosest, K)),
    OldIds =/= NewIds.

extract_node_ids(Nodes) ->
    lists:sort([Id || #{node_id := Id} <- Nodes]).

%% @doc Remove duplicate nodes (by node_id).
-spec deduplicate_nodes([macula_routing_bucket:node_info()]) -> [macula_routing_bucket:node_info()].
deduplicate_nodes(Nodes) ->
    {_, Result} = lists:foldl(fun dedupe_node/2, {#{}, []}, Nodes),
    lists:reverse(Result).

dedupe_node(#{node_id := Id} = Node, {Seen, Acc}) ->
    case maps:is_key(Id, Seen) of
        true -> {Seen, Acc};
        false -> {Seen#{Id => true}, [Node | Acc]}
    end.

%%%===================================================================
%%% Simple Key-Value Interface
%%% Used by macula_dist_discovery for QUIC distribution (deferred v1.1.0+)
%%%===================================================================

%% @doc Store a key-value pair in the DHT.
%% Delegates to macula_routing_server if running.
-spec store(binary(), binary()) -> ok | {error, term()}.
store(Key, Value) ->
    case whereis(macula_routing_server) of
        undefined ->
            {error, not_started};
        Pid ->
            try
                macula_routing_server:store(Pid, Key, Value)
            catch
                exit:{noproc, _} -> {error, not_started};
                _:Reason -> {error, Reason}
            end
    end.

%% @doc Delete a key from the DHT.
%% Delegates to macula_routing_server if running.
-spec delete(binary()) -> ok | {error, term()}.
delete(Key) ->
    case whereis(macula_routing_server) of
        undefined ->
            {error, not_started};
        Pid ->
            try
                macula_routing_server:delete_local(Pid, Key, user_requested),
                ok
            catch
                exit:{noproc, _} -> {error, not_started};
                _:Reason -> {error, Reason}
            end
    end.

%% @doc Find a value in the DHT.
%% Delegates to macula_routing_server if running.
-spec find(binary()) -> {ok, binary()} | {error, not_found | term()}.
find(Key) ->
    case whereis(macula_routing_server) of
        undefined ->
            {error, not_started};
        Pid ->
            try
                case macula_routing_server:find_value(Pid, Key, #{}) of
                    {ok, Value} -> {ok, Value};
                    {error, not_found} -> {error, not_found};
                    Other -> Other
                end
            catch
                exit:{noproc, _} -> {error, not_started};
                _:Reason -> {error, Reason}
            end
    end.

%% @doc Subscribe to DHT events matching a key prefix.
%% Subscribes the given Pid to receive events when keys with the given prefix
%% are stored or deleted. Uses gproc property-based subscriptions.
%%
%% Events sent to the subscriber:
%%   {dht_stored, Key, Value} - When a key is stored
%%   {dht_deleted, Key} - When a key is deleted
%%
%% To unsubscribe, the subscriber process should call:
%%   gproc:unreg({p, l, {dht_prefix_subscription, Prefix}})
-spec subscribe(binary(), pid()) -> ok.
subscribe(Prefix, Pid) when is_binary(Prefix), is_pid(Pid) ->
    %% Register subscription via gproc property
    %% The macula_routing_server will notify subscribers when keys change
    Key = {p, l, {dht_prefix_subscription, Prefix}},
    try
        %% If the caller is subscribing itself, use gproc:reg
        %% If subscribing another process, we need to send a message to that process
        case Pid =:= self() of
            true ->
                gproc:reg(Key),
                ok;
            false ->
                %% For remote subscription, send a message to the target pid
                %% asking it to register. The target process must handle this.
                Pid ! {subscribe_dht_prefix, Prefix},
                ok
        end
    catch
        error:badarg ->
            %% Already registered - this is fine
            ok;
        _:_ ->
            ok
    end;
subscribe(_, _) ->
    ok.

%% @doc Notify subscribers about a DHT store event.
%% Called by macula_routing_server when a key is stored.
-spec notify_store(binary(), term()) -> ok.
notify_store(Key, Value) ->
    notify_prefix_subscribers(Key, {dht_stored, Key, Value}).

%% @doc Notify subscribers about a DHT delete event.
%% Called by macula_routing_server when a key is deleted.
-spec notify_delete(binary()) -> ok.
notify_delete(Key) ->
    notify_prefix_subscribers(Key, {dht_deleted, Key}).

%% @doc Send notification to all subscribers whose prefix matches the key.
-spec notify_prefix_subscribers(binary(), term()) -> ok.
notify_prefix_subscribers(Key, Message) ->
    %% Find all prefix subscriptions and check if they match
    %% This iterates over all subscriptions - for production use with many
    %% subscriptions, consider a more efficient data structure
    try
        Matches = gproc:select({l, p}, [{{{'_', '_', {dht_prefix_subscription, '$1'}}, '_', '_'},
                                          [], ['$1']}]),
        lists:foreach(fun(Prefix) ->
            case binary:match(Key, Prefix) of
                {0, _} ->
                    %% Key starts with this prefix - notify subscribers
                    gproc:send({p, l, {dht_prefix_subscription, Prefix}}, Message);
                _ ->
                    ok
            end
        end, Matches)
    catch
        _:_ ->
            %% gproc not available or no subscribers - ignore
            ok
    end.
