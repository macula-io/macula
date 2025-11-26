%%%-------------------------------------------------------------------
%%% @doc
%%% Direct Routing Table - Bypass bootstrap for known subscriber endpoints.
%%%
%%% Problem: Every publish goes through bootstrap even for known subscribers.
%%% Solution: Cache {NodeId, Endpoint} mappings and route directly via QUIC.
%%%
%%% Design:
%%%   - ETS table for O(1) lookup by NodeId
%%%   - TTL-based expiration (default 5 minutes)
%%%   - Automatic cleanup of stale entries
%%%   - Thread-safe concurrent reads
%%%
%%% Expected improvement:
%%%   - 3-5x latency reduction for messages to known subscribers
%%%   - Reduced load on bootstrap gateway
%%%
%%% Configuration Options:
%%%   - ttl_ms: Route entry TTL (default: 300000ms = 5 minutes)
%%%   - cleanup_interval_ms: How often to clean stale entries (default: 60000ms)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_direct_routing).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    lookup/1,
    store/2,
    store_from_subscriber/1,
    remove/1,
    clear_all/0,
    stats/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-define(SERVER, ?MODULE).
-define(TABLE, macula_direct_routing_table).
-define(DEFAULT_TTL_MS, 300000).  % 5 minutes default TTL
-define(DEFAULT_CLEANUP_INTERVAL_MS, 60000).  % Cleanup every 1 minute

-record(state, {
    table :: ets:tid(),
    ttl_ms :: pos_integer(),
    cleanup_interval_ms :: pos_integer(),
    hits :: non_neg_integer(),
    misses :: non_neg_integer(),
    stores :: non_neg_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the direct routing table with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the direct routing table with options.
%% Options:
%%   - ttl_ms: Route entry TTL in milliseconds (default: 300000)
%%   - cleanup_interval_ms: Cleanup interval (default: 60000)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Look up endpoint for a node ID.
%% Returns {ok, Endpoint} on hit, or miss on cache miss/expired.
-spec lookup(binary()) -> {ok, binary()} | miss.
lookup(NodeId) when is_binary(NodeId) ->
    case ets:lookup(?TABLE, NodeId) of
        [{NodeId, Endpoint, ExpiresAt}] ->
            Now = erlang:system_time(millisecond),
            case ExpiresAt > Now of
                true ->
                    %% Cache hit - increment counter asynchronously
                    gen_server:cast(?SERVER, cache_hit),
                    {ok, Endpoint};
                false ->
                    %% Expired - treat as miss
                    ets:delete(?TABLE, NodeId),
                    gen_server:cast(?SERVER, cache_miss),
                    miss
            end;
        [] ->
            gen_server:cast(?SERVER, cache_miss),
            miss
    end.

%% @doc Store endpoint for a node ID.
%% Entry will expire after TTL.
-spec store(binary(), binary()) -> ok.
store(NodeId, Endpoint) when is_binary(NodeId), is_binary(Endpoint) ->
    gen_server:cast(?SERVER, {store, NodeId, Endpoint}).

%% @doc Store routing info from a subscriber map (from DHT lookup).
%% Extracts node_id and endpoint from subscriber info.
-spec store_from_subscriber(map()) -> ok.
store_from_subscriber(#{node_id := NodeId, endpoint := Endpoint}) ->
    store(NodeId, Endpoint);
store_from_subscriber(#{<<"node_id">> := NodeId, <<"endpoint">> := Endpoint}) ->
    store(NodeId, Endpoint);
store_from_subscriber(_) ->
    %% Missing required fields - skip silently
    ok.

%% @doc Remove routing entry for a node ID.
-spec remove(binary()) -> ok.
remove(NodeId) when is_binary(NodeId) ->
    gen_server:cast(?SERVER, {remove, NodeId}).

%% @doc Clear all routing entries.
-spec clear_all() -> ok.
clear_all() ->
    gen_server:call(?SERVER, clear_all).

%% @doc Get routing table statistics.
-spec stats() -> map().
stats() ->
    gen_server:call(?SERVER, stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    TtlMs = maps:get(ttl_ms, Opts, ?DEFAULT_TTL_MS),
    CleanupIntervalMs = maps:get(cleanup_interval_ms, Opts, ?DEFAULT_CLEANUP_INTERVAL_MS),
    Table = ets:new(?TABLE, [
        named_table,
        set,
        public,  % Allow direct reads from any process
        {read_concurrency, true}
    ]),
    %% Schedule periodic cleanup
    erlang:send_after(CleanupIntervalMs, self(), cleanup),
    {ok, #state{
        table = Table,
        ttl_ms = TtlMs,
        cleanup_interval_ms = CleanupIntervalMs,
        hits = 0,
        misses = 0,
        stores = 0
    }}.

handle_call(stats, _From, #state{hits = Hits, misses = Misses, stores = Stores} = State) ->
    Total = Hits + Misses,
    HitRate = case Total of
        0 -> 0.0;
        _ -> Hits / Total * 100
    end,
    TableSize = ets:info(?TABLE, size),
    Stats = #{
        hits => Hits,
        misses => Misses,
        stores => Stores,
        total_lookups => Total,
        hit_rate => HitRate,
        table_size => TableSize
    },
    {reply, Stats, State};

handle_call(clear_all, _From, State) ->
    ets:delete_all_objects(?TABLE),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(cache_hit, #state{hits = Hits} = State) ->
    {noreply, State#state{hits = Hits + 1}};

handle_cast(cache_miss, #state{misses = Misses} = State) ->
    {noreply, State#state{misses = Misses + 1}};

handle_cast({store, NodeId, Endpoint}, #state{ttl_ms = TtlMs, stores = Stores} = State) ->
    ExpiresAt = erlang:system_time(millisecond) + TtlMs,
    ets:insert(?TABLE, {NodeId, Endpoint, ExpiresAt}),
    {noreply, State#state{stores = Stores + 1}};

handle_cast({remove, NodeId}, State) ->
    ets:delete(?TABLE, NodeId),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, #state{cleanup_interval_ms = CleanupIntervalMs} = State) ->
    Now = erlang:system_time(millisecond),
    %% Remove expired entries
    MatchSpec = [{{'$1', '_', '$2'}, [{'<', '$2', Now}], [true]}],
    _ = ets:select_delete(?TABLE, MatchSpec),
    %% Schedule next cleanup
    erlang:send_after(CleanupIntervalMs, self(), cleanup),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
