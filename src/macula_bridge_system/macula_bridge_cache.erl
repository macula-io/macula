%%%-------------------------------------------------------------------
%%% @doc
%%% Macula Bridge Cache - Caches results from parent DHT queries.
%%%
%%% When a DHT query is escalated to a parent level and succeeds,
%%% the result is cached locally to avoid repeated parent queries.
%%%
%%% Cache characteristics:
%%% - TTL-based expiration (configurable per mesh level)
%%% - LRU eviction when cache is full
%%% - Different TTLs for different mesh levels:
%%%   - Cluster: 5 minutes (local, changes frequently)
%%%   - Street: 10 minutes
%%%   - Neighborhood: 15 minutes
%%%   - City: 30 minutes
%%%   - Country+: 60 minutes
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_bridge_cache).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    get/2,
    put/3,
    put/4,
    delete/2,
    clear/1,
    size/1,
    get_stats/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% State
-record(state, {
    cache :: ets:tid(),                      % ETS table for cache entries
    mesh_level :: atom(),                    % Determines default TTL
    max_size :: pos_integer(),               % Maximum cache entries
    default_ttl :: pos_integer(),            % Default TTL in seconds
    stats :: map()
}).

-record(cache_entry, {
    key :: binary(),
    value :: term(),
    expires_at :: integer(),                 % Unix timestamp
    inserted_at :: integer(),
    hit_count :: non_neg_integer()
}).

-define(DEFAULT_MAX_SIZE, 10000).
-define(CLEANUP_INTERVAL, 60000).  % 1 minute

%% Default TTLs per mesh level (in seconds)
-define(TTL_CLUSTER, 300).       % 5 minutes
-define(TTL_STREET, 600).        % 10 minutes
-define(TTL_NEIGHBORHOOD, 900).  % 15 minutes
-define(TTL_CITY, 1800).         % 30 minutes
-define(TTL_PROVINCE, 3600).     % 60 minutes
-define(TTL_COUNTRY, 3600).      % 60 minutes
-define(TTL_REGION, 3600).       % 60 minutes
-define(TTL_GLOBAL, 3600).       % 60 minutes

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start bridge cache with registered name.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Get value from cache.
-spec get(pid(), binary()) -> {ok, term()} | not_found | expired.
get(Pid, Key) ->
    gen_server:call(Pid, {get, Key}).

%% @doc Put value in cache with default TTL.
-spec put(pid(), binary(), term()) -> ok.
put(Pid, Key, Value) ->
    gen_server:call(Pid, {put, Key, Value, default}).

%% @doc Put value in cache with specific TTL (in seconds).
-spec put(pid(), binary(), term(), pos_integer() | default) -> ok.
put(Pid, Key, Value, TTL) ->
    gen_server:call(Pid, {put, Key, Value, TTL}).

%% @doc Delete value from cache.
-spec delete(pid(), binary()) -> ok.
delete(Pid, Key) ->
    gen_server:call(Pid, {delete, Key}).

%% @doc Clear entire cache.
-spec clear(pid()) -> ok.
clear(Pid) ->
    gen_server:call(Pid, clear).

%% @doc Get current cache size.
-spec size(pid()) -> non_neg_integer().
size(Pid) ->
    gen_server:call(Pid, size).

%% @doc Get cache statistics.
-spec get_stats(pid()) -> {ok, map()}.
get_stats(Pid) ->
    gen_server:call(Pid, get_stats).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init(Config) ->
    MeshLevel = maps:get(mesh_level, Config, cluster),
    MaxSize = maps:get(cache_max_size, Config, ?DEFAULT_MAX_SIZE),
    DefaultTTL = maps:get(cache_ttl, Config, ttl_for_level(MeshLevel)),

    %% Create ETS table for cache
    Cache = ets:new(bridge_cache, [
        set,
        private,
        {keypos, #cache_entry.key}
    ]),

    ?LOG_INFO("[BridgeCache] Starting for level ~p with TTL ~p seconds, max ~p entries",
              [MeshLevel, DefaultTTL, MaxSize]),

    State = #state{
        cache = Cache,
        mesh_level = MeshLevel,
        max_size = MaxSize,
        default_ttl = DefaultTTL,
        stats = init_stats()
    },

    %% Schedule periodic cleanup
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_expired),

    {ok, State}.

handle_call({get, Key}, _From, #state{cache = Cache, stats = Stats} = State) ->
    Now = erlang:system_time(second),
    {Reply, NewStats} = case ets:lookup(Cache, Key) of
        [] ->
            {not_found, increment_stat(misses, Stats)};
        [#cache_entry{expires_at = ExpiresAt}] when ExpiresAt =< Now ->
            %% Entry expired (ExpiresAt <= Now) - delete and return expired
            ets:delete(Cache, Key),
            {expired, increment_stat(expired_hits, Stats)};
        [#cache_entry{value = Value, hit_count = Hits} = Entry] ->
            %% Valid entry - update hit count
            ets:insert(Cache, Entry#cache_entry{hit_count = Hits + 1}),
            {{ok, Value}, increment_stat(hits, Stats)}
    end,
    {reply, Reply, State#state{stats = NewStats}};

handle_call({put, Key, Value, TTLSpec}, _From,
            #state{cache = Cache, default_ttl = DefaultTTL, max_size = MaxSize,
                   stats = Stats} = State) ->
    %% Ensure we have room
    CurrentSize = ets:info(Cache, size),
    evict_if_needed(Cache, CurrentSize, MaxSize),

    TTL = resolve_ttl(TTLSpec, DefaultTTL),
    Now = erlang:system_time(second),

    Entry = #cache_entry{
        key = Key,
        value = Value,
        expires_at = Now + TTL,
        inserted_at = Now,
        hit_count = 0
    },

    ets:insert(Cache, Entry),
    NewStats = increment_stat(inserts, Stats),
    {reply, ok, State#state{stats = NewStats}};

handle_call({delete, Key}, _From, #state{cache = Cache, stats = Stats} = State) ->
    ets:delete(Cache, Key),
    NewStats = increment_stat(deletes, Stats),
    {reply, ok, State#state{stats = NewStats}};

handle_call(clear, _From, #state{cache = Cache, stats = Stats} = State) ->
    ets:delete_all_objects(Cache),
    NewStats = increment_stat(clears, Stats),
    {reply, ok, State#state{stats = NewStats}};

handle_call(size, _From, #state{cache = Cache} = State) ->
    Size = ets:info(Cache, size),
    {reply, Size, State};

handle_call(get_stats, _From, #state{cache = Cache, stats = Stats,
                                      mesh_level = Level, default_ttl = TTL,
                                      max_size = MaxSize} = State) ->
    FullStats = Stats#{
        current_size => ets:info(Cache, size),
        max_size => MaxSize,
        mesh_level => Level,
        default_ttl => TTL
    },
    {reply, {ok, FullStats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(cleanup_expired, #state{cache = Cache, stats = Stats} = State) ->
    ExpiredCount = cleanup_expired_entries(Cache),
    NewStats = case ExpiredCount > 0 of
        true ->
            ?LOG_DEBUG("[BridgeCache] Cleaned up ~p expired entries", [ExpiredCount]),
            maps:update_with(expired_cleaned, fun(V) -> V + ExpiredCount end, ExpiredCount, Stats);
        false ->
            Stats
    end,
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_expired),
    {noreply, State#state{stats = NewStats}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{cache = Cache}) ->
    ets:delete(Cache),
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Initialize statistics map.
-spec init_stats() -> map().
init_stats() ->
    #{
        hits => 0,
        misses => 0,
        expired_hits => 0,
        inserts => 0,
        deletes => 0,
        clears => 0,
        evictions => 0,
        expired_cleaned => 0,
        started_at => erlang:system_time(second)
    }.

%% @doc Increment a statistic counter.
-spec increment_stat(atom(), map()) -> map().
increment_stat(Key, Stats) ->
    maps:update_with(Key, fun(V) -> V + 1 end, 1, Stats).

%% @doc Get default TTL for mesh level.
-spec ttl_for_level(atom()) -> pos_integer().
ttl_for_level(cluster) -> ?TTL_CLUSTER;
ttl_for_level(street) -> ?TTL_STREET;
ttl_for_level(neighborhood) -> ?TTL_NEIGHBORHOOD;
ttl_for_level(city) -> ?TTL_CITY;
ttl_for_level(province) -> ?TTL_PROVINCE;
ttl_for_level(country) -> ?TTL_COUNTRY;
ttl_for_level(region) -> ?TTL_REGION;
ttl_for_level(global) -> ?TTL_GLOBAL;
ttl_for_level(_) -> ?TTL_CLUSTER.

%% @doc Resolve TTL specification to actual value.
-spec resolve_ttl(pos_integer() | default, pos_integer()) -> pos_integer().
resolve_ttl(default, DefaultTTL) -> DefaultTTL;
resolve_ttl(TTL, _) when is_integer(TTL), TTL > 0 -> TTL.

%% @doc Evict entries if cache is at capacity.
-spec evict_if_needed(ets:tid(), non_neg_integer(), pos_integer()) -> ok.
evict_if_needed(Cache, CurrentSize, MaxSize) when CurrentSize >= MaxSize ->
    %% Evict ~10% of entries using LRU (lowest hit count + oldest)
    EvictCount = max(1, MaxSize div 10),
    evict_lru_entries(Cache, EvictCount);
evict_if_needed(_, _, _) ->
    ok.

%% @doc Evict least recently used entries.
-spec evict_lru_entries(ets:tid(), pos_integer()) -> ok.
evict_lru_entries(Cache, Count) ->
    %% Get all entries sorted by hit_count (ascending), then by inserted_at (ascending)
    AllEntries = ets:tab2list(Cache),
    Sorted = lists:sort(fun(#cache_entry{hit_count = H1, inserted_at = T1},
                            #cache_entry{hit_count = H2, inserted_at = T2}) ->
        case H1 =:= H2 of
            true -> T1 =< T2;
            false -> H1 < H2
        end
    end, AllEntries),

    %% Delete the first Count entries
    ToEvict = lists:sublist(Sorted, Count),
    lists:foreach(fun(#cache_entry{key = Key}) ->
        ets:delete(Cache, Key)
    end, ToEvict),
    ok.

%% @doc Clean up expired entries.
-spec cleanup_expired_entries(ets:tid()) -> non_neg_integer().
cleanup_expired_entries(Cache) ->
    Now = erlang:system_time(second),
    %% Use match_delete for efficiency
    %% Entry is expired if ExpiresAt =< Now (TTL has passed)
    MatchSpec = [{
        #cache_entry{key = '$1', expires_at = '$2', _ = '_'},
        [{'=<', '$2', Now}],
        [true]
    }],
    ets:select_delete(Cache, MatchSpec).
