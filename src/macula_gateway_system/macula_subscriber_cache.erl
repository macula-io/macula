%%%-------------------------------------------------------------------
%%% @doc
%%% DHT Subscriber Cache - Caches topic→subscribers mappings for fast pub/sub.
%%%
%%% Problem: DHT lookups on every PUBLISH cause 50-200ms latency per message.
%%% Solution: Cache topic→subscribers mappings with TTL-based expiration.
%%%
%%% Design:
%%%   - ETS table for O(1) lookup
%%%   - TTL-based expiration (default 5 seconds)
%%%   - Automatic cache invalidation on subscribe/unsubscribe
%%%   - Thread-safe concurrent reads
%%%   - Adaptive rate-limiting to prevent discovery storms (default 2 seconds)
%%%
%%% Rate-Limiting:
%%%   When cache expires and many publishes occur, rate-limiting prevents
%%%   flooding the DHT with lookup queries. Only one DHT query per topic
%%%   is allowed within the min_discovery_interval_ms window.
%%%
%%% Expected improvement:
%%%   - 5-10x latency reduction for high-frequency topics (caching)
%%%   - 2-3x improvement during high-frequency publishing (rate-limiting)
%%%
%%% Configuration Options:
%%%   - ttl_ms: Cache entry TTL (default: 5000ms)
%%%   - min_discovery_interval_ms: Minimum time between DHT queries per topic (default: 2000ms)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_subscriber_cache).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    lookup/1,
    store/2,
    invalidate/1,
    invalidate_all/0,
    stats/0,
    %% Rate-limiting API
    should_query_dht/1,
    record_dht_query/1
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
-define(TABLE, macula_subscriber_cache_table).
-define(RATE_LIMIT_TABLE, macula_discovery_rate_limit_table).
-define(DEFAULT_TTL_MS, 5000).  % 5 seconds default TTL
-define(DEFAULT_MIN_DISCOVERY_INTERVAL_MS, 2000).  % 2 seconds between DHT queries per topic
-define(CLEANUP_INTERVAL_MS, 1000).  % Cleanup every 1 second

-record(state, {
    table :: ets:tid(),
    rate_limit_table :: ets:tid(),
    ttl_ms :: pos_integer(),
    min_discovery_interval_ms :: pos_integer(),
    hits :: non_neg_integer(),
    misses :: non_neg_integer(),
    rate_limited :: non_neg_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the subscriber cache with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the subscriber cache with options.
%% Options:
%%   - ttl_ms: Cache entry TTL in milliseconds (default: 5000)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Look up subscribers for a topic from cache.
%% Returns {ok, Subscribers} on cache hit, or {miss, TopicKey} on cache miss.
%% TopicKey is the SHA256 hash of the topic binary.
-spec lookup(binary()) -> {ok, list()} | {miss, binary()}.
lookup(Topic) when is_binary(Topic) ->
    TopicKey = crypto:hash(sha256, Topic),
    lookup_by_key(TopicKey).

%% @doc Store subscribers for a topic in cache.
%% Subscribers will expire after TTL.
-spec store(binary(), list()) -> ok.
store(Topic, Subscribers) when is_binary(Topic), is_list(Subscribers) ->
    TopicKey = crypto:hash(sha256, Topic),
    store_by_key(TopicKey, Subscribers).

%% @doc Invalidate cache entry for a topic.
%% Should be called when subscription changes occur.
-spec invalidate(binary()) -> ok.
invalidate(Topic) when is_binary(Topic) ->
    TopicKey = crypto:hash(sha256, Topic),
    invalidate_by_key(TopicKey).

%% @doc Invalidate all cache entries.
%% Useful for testing or when major topology changes occur.
-spec invalidate_all() -> ok.
invalidate_all() ->
    gen_server:call(?SERVER, invalidate_all).

%% @doc Get cache statistics.
-spec stats() -> map().
stats() ->
    gen_server:call(?SERVER, stats).

%% @doc Check if we should query DHT for a topic (rate-limiting check).
%% Returns true if enough time has passed since last DHT query.
%% Returns false if we recently queried DHT (rate-limited).
-spec should_query_dht(binary()) -> boolean().
should_query_dht(Topic) when is_binary(Topic) ->
    TopicKey = crypto:hash(sha256, Topic),
    should_query_dht_by_key(TopicKey).

%% @doc Record that we just performed a DHT query for a topic.
%% Call this after a successful DHT lookup to reset the rate-limit timer.
-spec record_dht_query(binary()) -> ok.
record_dht_query(Topic) when is_binary(Topic) ->
    TopicKey = crypto:hash(sha256, Topic),
    record_dht_query_by_key(TopicKey).

%%%===================================================================
%%% Internal Lookup Functions (direct ETS access for speed)
%%%===================================================================

lookup_by_key(TopicKey) ->
    case ets:lookup(?TABLE, TopicKey) of
        [{TopicKey, Subscribers, ExpiresAt}] ->
            Now = erlang:system_time(millisecond),
            case ExpiresAt > Now of
                true ->
                    %% Cache hit - increment counter asynchronously
                    gen_server:cast(?SERVER, cache_hit),
                    {ok, Subscribers};
                false ->
                    %% Expired - treat as miss
                    ets:delete(?TABLE, TopicKey),
                    gen_server:cast(?SERVER, cache_miss),
                    {miss, TopicKey}
            end;
        [] ->
            gen_server:cast(?SERVER, cache_miss),
            {miss, TopicKey}
    end.

store_by_key(TopicKey, Subscribers) ->
    gen_server:cast(?SERVER, {store, TopicKey, Subscribers}).

invalidate_by_key(TopicKey) ->
    gen_server:cast(?SERVER, {invalidate, TopicKey}).

%% @private
%% @doc Check rate-limit table to see if we can query DHT.
%% Direct ETS access for performance.
should_query_dht_by_key(TopicKey) ->
    Now = erlang:system_time(millisecond),
    case ets:lookup(?RATE_LIMIT_TABLE, TopicKey) of
        [{TopicKey, LastQueryTime, MinInterval}] ->
            TimeSinceLastQuery = Now - LastQueryTime,
            case TimeSinceLastQuery >= MinInterval of
                true ->
                    %% Enough time has passed - allow query
                    true;
                false ->
                    %% Rate-limited - too soon
                    gen_server:cast(?SERVER, rate_limited),
                    false
            end;
        [] ->
            %% No previous query - allow
            true
    end.

%% @private
%% @doc Record DHT query in rate-limit table.
record_dht_query_by_key(TopicKey) ->
    gen_server:cast(?SERVER, {record_dht_query, TopicKey}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    TtlMs = maps:get(ttl_ms, Opts, ?DEFAULT_TTL_MS),
    MinDiscoveryIntervalMs = maps:get(min_discovery_interval_ms, Opts, ?DEFAULT_MIN_DISCOVERY_INTERVAL_MS),
    Table = ets:new(?TABLE, [
        named_table,
        set,
        public,  % Allow direct reads from any process
        {read_concurrency, true}
    ]),
    RateLimitTable = ets:new(?RATE_LIMIT_TABLE, [
        named_table,
        set,
        public,  % Allow direct reads from any process
        {read_concurrency, true}
    ]),
    %% Schedule periodic cleanup
    erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup),
    {ok, #state{
        table = Table,
        rate_limit_table = RateLimitTable,
        ttl_ms = TtlMs,
        min_discovery_interval_ms = MinDiscoveryIntervalMs,
        hits = 0,
        misses = 0,
        rate_limited = 0
    }}.

handle_call(stats, _From, #state{hits = Hits, misses = Misses, rate_limited = RateLimited} = State) ->
    Total = Hits + Misses,
    HitRate = case Total of
        0 -> 0.0;
        _ -> Hits / Total * 100
    end,
    TableSize = ets:info(?TABLE, size),
    RateLimitTableSize = ets:info(?RATE_LIMIT_TABLE, size),
    Stats = #{
        hits => Hits,
        misses => Misses,
        total => Total,
        hit_rate => HitRate,
        table_size => TableSize,
        rate_limited => RateLimited,
        rate_limit_table_size => RateLimitTableSize
    },
    {reply, Stats, State};

handle_call(invalidate_all, _From, State) ->
    ets:delete_all_objects(?TABLE),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(cache_hit, #state{hits = Hits} = State) ->
    {noreply, State#state{hits = Hits + 1}};

handle_cast(cache_miss, #state{misses = Misses} = State) ->
    {noreply, State#state{misses = Misses + 1}};

handle_cast({store, TopicKey, Subscribers}, #state{ttl_ms = TtlMs} = State) ->
    ExpiresAt = erlang:system_time(millisecond) + TtlMs,
    ets:insert(?TABLE, {TopicKey, Subscribers, ExpiresAt}),
    {noreply, State};

handle_cast({invalidate, TopicKey}, State) ->
    ets:delete(?TABLE, TopicKey),
    {noreply, State};

handle_cast(rate_limited, #state{rate_limited = RateLimited} = State) ->
    {noreply, State#state{rate_limited = RateLimited + 1}};

handle_cast({record_dht_query, TopicKey}, #state{min_discovery_interval_ms = MinInterval} = State) ->
    Now = erlang:system_time(millisecond),
    ets:insert(?RATE_LIMIT_TABLE, {TopicKey, Now, MinInterval}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, #state{min_discovery_interval_ms = MinInterval} = State) ->
    Now = erlang:system_time(millisecond),

    %% Remove expired cache entries
    CacheMatchSpec = [{{'$1', '_', '$2'}, [{'<', '$2', Now}], [true]}],
    _NumCacheDeleted = ets:select_delete(?TABLE, CacheMatchSpec),

    %% Remove stale rate-limit entries (older than 2x min interval)
    %% This prevents the rate-limit table from growing unbounded
    MaxRateLimitAge = MinInterval * 2,
    CutoffTime = Now - MaxRateLimitAge,
    RateLimitMatchSpec = [{{'$1', '$2', '_'}, [{'<', '$2', CutoffTime}], [true]}],
    _NumRateLimitDeleted = ets:select_delete(?RATE_LIMIT_TABLE, RateLimitMatchSpec),

    %% Schedule next cleanup
    erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
