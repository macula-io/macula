%%%-------------------------------------------------------------------
%%% @doc
%%% Distributed Relay Registry.
%%%
%%% Tracks peers that can serve as relay nodes for NAT traversal.
%%% Peers with public IPs or NATs that allow incoming connections
%%% can register as relay-capable.
%%%
%%% Relay Selection Criteria:
%%% - Peer must have public IP or full-cone NAT
%%% - Lower latency to requesting peer preferred
%%% - Load balancing across available relays
%%% - Geographic proximity (via RTT estimation)
%%%
%%% Registry Storage:
%%% - Local ETS for fast lookup
%%% - DHT for distributed discovery
%%% - TTL-based cleanup for stale entries
%%%
%%% Usage: register(NodeId, Endpoint) to register as relay-capable,
%%% find_relay(TargetNodeId) to find best relay for a target.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_relay_registry).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    register/2,
    register/3,
    unregister/1,
    find_relay/1,
    find_relay/2,
    get_relays/0,
    get_relay_count/0,
    is_relay/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(TABLE, macula_relay_registry_table).
-define(DEFAULT_MAX_RELAYS, 1000).
-define(DEFAULT_RELAY_TTL_SECONDS, 600).  % 10 minutes
-define(CLEANUP_INTERVAL_MS, 60000).      % Cleanup every 60 seconds
-define(DHT_KEY_PREFIX, <<"relay.node.">>).

%%%===================================================================
%%% Types
%%%===================================================================

-type relay_info() :: #{
    node_id := binary(),
    endpoint := {binary() | string(), inet:port_number()},
    capacity := non_neg_integer(),         % Max concurrent relays
    current_load := non_neg_integer(),     % Current relay count
    latency_ms => non_neg_integer(),       % RTT to this relay
    registered_at := integer(),
    expires_at := integer()
}.

-type find_opts() :: #{
    max_results => pos_integer(),
    exclude => [binary()],
    max_latency_ms => non_neg_integer()
}.

-export_type([relay_info/0, find_opts/0]).

-record(state, {
    table :: ets:tid(),
    max_relays :: pos_integer(),
    relay_ttl :: pos_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the relay registry server.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Register current node as relay-capable.
-spec register(binary(), {binary() | string(), inet:port_number()}) -> ok | {error, term()}.
register(NodeId, Endpoint) ->
    register(NodeId, Endpoint, #{}).

%% @doc Register as relay with options.
%% Options:
%%   capacity - Maximum concurrent relay connections (default: 100)
-spec register(binary(), {binary() | string(), inet:port_number()}, map()) -> ok | {error, term()}.
register(NodeId, Endpoint, Opts) ->
    gen_server:call(?SERVER, {register, NodeId, Endpoint, Opts}).

%% @doc Unregister as relay.
-spec unregister(binary()) -> ok.
unregister(NodeId) ->
    gen_server:cast(?SERVER, {unregister, NodeId}).

%% @doc Find best relay for connecting to a target.
-spec find_relay(binary()) -> {ok, relay_info()} | {error, no_relays_available}.
find_relay(TargetNodeId) ->
    find_relay(TargetNodeId, #{}).

%% @doc Find relay with options.
-spec find_relay(binary(), find_opts()) -> {ok, relay_info()} | {error, no_relays_available}.
find_relay(TargetNodeId, Opts) ->
    gen_server:call(?SERVER, {find_relay, TargetNodeId, Opts}).

%% @doc Get all registered relays.
-spec get_relays() -> [relay_info()].
get_relays() ->
    gen_server:call(?SERVER, get_relays).

%% @doc Get count of available relays.
-spec get_relay_count() -> non_neg_integer().
get_relay_count() ->
    gen_server:call(?SERVER, get_relay_count).

%% @doc Check if a node is registered as relay.
-spec is_relay(binary()) -> boolean().
is_relay(NodeId) ->
    gen_server:call(?SERVER, {is_relay, NodeId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    MaxRelays = maps:get(max_relays, Opts, ?DEFAULT_MAX_RELAYS),
    RelayTTL = maps:get(relay_ttl_seconds, Opts, ?DEFAULT_RELAY_TTL_SECONDS),

    %% Create ETS table for relay info
    Table = ets:new(?TABLE, [
        set,
        protected,
        {keypos, 1},
        {read_concurrency, true}
    ]),

    %% Schedule periodic cleanup
    schedule_cleanup(),

    ?LOG_INFO("Relay registry started (max_relays=~p, ttl=~p seconds)",
              [MaxRelays, RelayTTL]),

    {ok, #state{
        table = Table,
        max_relays = MaxRelays,
        relay_ttl = RelayTTL
    }}.

handle_call({register, NodeId, Endpoint, Opts}, _From, State) ->
    #state{table = Table, max_relays = MaxRelays, relay_ttl = TTL} = State,

    %% Check capacity
    CurrentCount = ets:info(Table, size),
    case CurrentCount >= MaxRelays of
        true ->
            ?LOG_WARNING("[RELAY_REGISTRY] Registration rejected: max_relays_reached (~p)", [MaxRelays]),
            {reply, {error, max_relays_reached}, State};
        false ->
            Now = erlang:system_time(second),
            Capacity = maps:get(capacity, Opts, 100),

            RelayInfo = #{
                node_id => NodeId,
                endpoint => Endpoint,
                capacity => Capacity,
                current_load => 0,
                registered_at => Now,
                expires_at => Now + TTL
            },

            ets:insert(Table, {NodeId, RelayInfo}),

            %% Publish to DHT for distributed discovery
            publish_relay_to_dht(NodeId, RelayInfo),

            ?LOG_WARNING("[RELAY_REGISTRY] Registered relay: ~s at ~p (capacity: ~p, table_size: ~p)",
                       [NodeId, Endpoint, Capacity, ets:info(Table, size)]),
            {reply, ok, State}
    end;

handle_call({find_relay, TargetNodeId, Opts}, _From, State) ->
    #state{table = Table} = State,
    Now = erlang:system_time(second),

    %% Get all valid relays
    Exclude = maps:get(exclude, Opts, []),
    MaxLatency = maps:get(max_latency_ms, Opts, infinity),

    %% Debug: log current ETS contents
    TableSize = ets:info(Table, size),
    ?LOG_WARNING("[RELAY_REGISTRY] find_relay called: target=~p, table_size=~p", [TargetNodeId, TableSize]),

    Candidates = ets:foldl(
        fun({NodeId, Info}, Acc) ->
            ExpiresAt = maps:get(expires_at, Info),
            Load = maps:get(current_load, Info),
            Capacity = maps:get(capacity, Info),
            Latency = maps:get(latency_ms, Info, 0),

            IsValid = Now < ExpiresAt,
            IsExcluded = lists:member(NodeId, Exclude),
            IsNotTarget = NodeId =/= TargetNodeId,
            HasCapacity = Load < Capacity,
            LatencyOk = MaxLatency =:= infinity orelse Latency =< MaxLatency,

            case IsValid andalso not IsExcluded andalso IsNotTarget andalso HasCapacity andalso LatencyOk of
                true -> [Info | Acc];
                false -> Acc
            end
        end,
        [],
        Table
    ),

    %% Also check DHT for remote relays
    AllCandidates = case Candidates of
        [] -> lookup_relays_from_dht(TargetNodeId);
        _ -> Candidates
    end,

    case AllCandidates of
        [] ->
            {reply, {error, no_relays_available}, State};
        _ ->
            %% Select best relay (lowest load, then lowest latency)
            BestRelay = select_best_relay(AllCandidates),
            {reply, {ok, BestRelay}, State}
    end;

handle_call(get_relays, _From, State) ->
    Now = erlang:system_time(second),
    Relays = ets:foldl(
        fun({_NodeId, Info}, Acc) ->
            ExpiresAt = maps:get(expires_at, Info),
            case Now < ExpiresAt of
                true -> [Info | Acc];
                false -> Acc
            end
        end,
        [],
        State#state.table
    ),
    {reply, Relays, State};

handle_call(get_relay_count, _From, State) ->
    Count = ets:info(State#state.table, size),
    {reply, Count, State};

handle_call({is_relay, NodeId}, _From, State) ->
    Now = erlang:system_time(second),
    Result = case ets:lookup(State#state.table, NodeId) of
        [{NodeId, #{expires_at := ExpiresAt}}] -> Now < ExpiresAt;
        [] -> false
    end,
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({unregister, NodeId}, State) ->
    ets:delete(State#state.table, NodeId),
    ?LOG_DEBUG("Unregistered relay: ~s", [NodeId]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    cleanup_expired_relays(State#state.table),
    schedule_cleanup(),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{table = Table}) ->
    ets:delete(Table),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Select best relay from candidates.
-spec select_best_relay([relay_info()]) -> relay_info().
select_best_relay([Relay]) -> Relay;
select_best_relay(Relays) ->
    %% Sort by load (ascending), then by latency (ascending)
    Sorted = lists:sort(
        fun(A, B) ->
            LoadA = maps:get(current_load, A, 0),
            LoadB = maps:get(current_load, B, 0),
            LatencyA = maps:get(latency_ms, A, 0),
            LatencyB = maps:get(latency_ms, B, 0),

            case LoadA =:= LoadB of
                true -> LatencyA =< LatencyB;
                false -> LoadA < LoadB
            end
        end,
        Relays
    ),
    hd(Sorted).

%% @private
%% @doc Publish relay registration to DHT.
-spec publish_relay_to_dht(binary(), relay_info()) -> ok.
publish_relay_to_dht(NodeId, RelayInfo) ->
    case whereis(macula_routing_server) of
        undefined ->
            ok;
        Pid ->
            DhtKey = relay_dht_key(NodeId),
            DhtValue = prepare_relay_for_dht(RelayInfo),
            try
                macula_routing_server:store(Pid, DhtKey, DhtValue),
                ?LOG_DEBUG("Published relay to DHT: ~s", [NodeId])
            catch
                _:Reason ->
                    ?LOG_WARNING("Failed to publish relay to DHT: ~p", [Reason])
            end,
            ok
    end.

%% @private
%% @doc Lookup relays from DHT.
-spec lookup_relays_from_dht(binary()) -> [relay_info()].
lookup_relays_from_dht(_TargetNodeId) ->
    case whereis(macula_routing_server) of
        undefined ->
            [];
        Pid ->
            %% Query DHT for relay nodes
            Key = crypto:hash(sha256, <<"relay.list">>),
            case macula_routing_server:find_value(Pid, Key, 20) of
                {ok, Value} when is_list(Value) ->
                    [parse_relay_from_dht(R) || R <- Value];
                _ ->
                    []
            end
    end.

%% @private
%% @doc Generate DHT key for relay.
-spec relay_dht_key(binary()) -> binary().
relay_dht_key(NodeId) ->
    crypto:hash(sha256, <<?DHT_KEY_PREFIX/binary, NodeId/binary>>).

%% @private
%% @doc Prepare relay info for DHT storage.
-spec prepare_relay_for_dht(relay_info()) -> map().
prepare_relay_for_dht(Info) ->
    {Host, Port} = maps:get(endpoint, Info),
    HostBin = case is_binary(Host) of
        true -> Host;
        false -> list_to_binary(Host)
    end,
    #{
        <<"node_id">> => maps:get(node_id, Info),
        <<"host">> => HostBin,
        <<"port">> => Port,
        <<"capacity">> => maps:get(capacity, Info),
        <<"current_load">> => maps:get(current_load, Info),
        <<"expires_at">> => maps:get(expires_at, Info)
    }.

%% @private
%% @doc Parse relay info from DHT format.
-spec parse_relay_from_dht(map()) -> relay_info().
parse_relay_from_dht(DhtValue) ->
    #{
        node_id => maps:get(<<"node_id">>, DhtValue),
        endpoint => {maps:get(<<"host">>, DhtValue), maps:get(<<"port">>, DhtValue)},
        capacity => maps:get(<<"capacity">>, DhtValue, 100),
        current_load => maps:get(<<"current_load">>, DhtValue, 0),
        registered_at => 0,
        expires_at => maps:get(<<"expires_at">>, DhtValue, 0)
    }.

%% @private
%% @doc Schedule periodic cleanup.
-spec schedule_cleanup() -> reference().
schedule_cleanup() ->
    erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup).

%% @private
%% @doc Clean up expired relay entries.
-spec cleanup_expired_relays(ets:tid()) -> ok.
cleanup_expired_relays(Table) ->
    Now = erlang:system_time(second),

    ExpiredKeys = ets:foldl(
        fun({NodeId, #{expires_at := ExpiresAt}}, Acc) ->
            case Now > ExpiresAt of
                true -> [NodeId | Acc];
                false -> Acc
            end
        end,
        [],
        Table
    ),

    lists:foreach(fun(Key) -> ets:delete(Table, Key) end, ExpiredKeys),

    case ExpiredKeys of
        [] -> ok;
        _ -> ?LOG_DEBUG("Relay cleanup: removed ~p expired relays", [length(ExpiredKeys)])
    end.
