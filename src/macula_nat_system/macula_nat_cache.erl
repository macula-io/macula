%%%-------------------------------------------------------------------
%%% @doc
%%% NAT Profile Cache with TTL and Stale-While-Revalidate.
%%%
%%% Caches NAT profiles for peers to avoid repeated detection overhead.
%%% Implements stale-while-revalidate pattern:
%%% - Fresh (0-TTL): Use immediately
%%% - Stale (TTL to TTL+60s): Use but trigger background refresh
%%% - Expired (>TTL+60s): Must re-detect
%%%
%%% Based on NATCracker methodology, NAT profiles include:
%%% - Mapping policy: EI (Endpoint-Independent), HD (Host-Dependent), PD (Port-Dependent)
%%% - Filtering policy: EI, HD, PD
%%% - Allocation policy: PP (Port-Preservation), PC (Port-Contiguity), RD (Random)
%%%
%%% Uses ETS for O(1) lookups with bounded memory via LRU eviction.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_nat_cache).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/1,
    get/1,
    get_from_dht/1,
    put/2,
    put/3,
    invalidate/1,
    clear/0,
    stats/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).
-define(TABLE, macula_nat_cache_table).
-define(DEFAULT_MAX_ENTRIES, 10000).
-define(DEFAULT_TTL_SECONDS, 300).      % 5 minutes
-define(STALE_GRACE_SECONDS, 60).       % 1 minute grace period
-define(CLEANUP_INTERVAL_MS, 60000).    % Cleanup every 60 seconds

%%%===================================================================
%%% Types
%%%===================================================================

-type mapping_policy() :: ei | hd | pd.
-type filtering_policy() :: ei | hd | pd.
-type allocation_policy() :: pp | pc | rd.

-type nat_profile() :: #{
    node_id := binary(),
    mapping_policy := mapping_policy(),
    filtering_policy := filtering_policy(),
    allocation_policy := allocation_policy(),
    reflexive_address => {inet:ip_address(), inet:port_number()},
    port_delta => integer(),
    can_receive_unsolicited := boolean(),
    requires_relay := boolean(),
    relay_capable := boolean(),
    detected_at := integer(),
    ttl_seconds := pos_integer(),
    %% Geo-location fields (optional, from environment variables)
    %% Set via MACULA_LATITUDE, MACULA_LONGITUDE, MACULA_LOCATION
    latitude => float(),         % -90.0 to 90.0 (south to north)
    longitude => float(),        % -180.0 to 180.0 (west to east)
    location_label => binary()   % Human-readable label e.g. "Amsterdam, NL"
}.

-export_type([nat_profile/0, mapping_policy/0, filtering_policy/0, allocation_policy/0]).

-record(state, {
    table :: ets:tid(),
    max_entries :: pos_integer(),
    default_ttl :: pos_integer(),
    hits :: non_neg_integer(),
    misses :: non_neg_integer(),
    evictions :: non_neg_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the NAT cache server.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Get NAT profile for a node ID from local cache.
%% Returns {ok, Profile, Status} where Status is fresh | stale
%% Returns not_found if not in cache or expired.
-spec get(binary()) -> {ok, nat_profile(), fresh | stale} | not_found.
get(NodeId) when is_binary(NodeId) ->
    gen_server:call(?SERVER, {get, NodeId}).

%% @doc Get NAT profile from DHT (remote lookup).
%% First checks local cache, then falls back to DHT query.
%% Caches result locally if found in DHT.
%% Returns {ok, Profile} | not_found | {error, Reason}.
-spec get_from_dht(binary()) -> {ok, nat_profile()} | not_found | {error, term()}.
get_from_dht(NodeId) when is_binary(NodeId) ->
    %% First check local cache
    case ?MODULE:get(NodeId) of
        {ok, Profile, _Status} ->
            {ok, Profile};
        not_found ->
            %% Try DHT lookup
            lookup_nat_profile_from_dht(NodeId)
    end.

%% @doc Store NAT profile with default TTL.
-spec put(binary(), nat_profile()) -> ok.
put(NodeId, Profile) when is_binary(NodeId), is_map(Profile) ->
    gen_server:cast(?SERVER, {put, NodeId, Profile, ?DEFAULT_TTL_SECONDS}).

%% @doc Store NAT profile with custom TTL.
-spec put(binary(), nat_profile(), pos_integer()) -> ok.
put(NodeId, Profile, TTL) when is_binary(NodeId), is_map(Profile), TTL > 0 ->
    gen_server:cast(?SERVER, {put, NodeId, Profile, TTL}).

%% @doc Invalidate NAT profile for a node ID.
-spec invalidate(binary()) -> ok.
invalidate(NodeId) when is_binary(NodeId) ->
    gen_server:cast(?SERVER, {invalidate, NodeId}).

%% @doc Clear all cached NAT profiles.
-spec clear() -> ok.
clear() ->
    gen_server:cast(?SERVER, clear).

%% @doc Get cache statistics.
-spec stats() -> #{
    size := non_neg_integer(),
    max_entries := pos_integer(),
    hits := non_neg_integer(),
    misses := non_neg_integer(),
    evictions := non_neg_integer(),
    hit_rate := float()
}.
stats() ->
    gen_server:call(?SERVER, stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    MaxEntries = maps:get(cache_max_entries, Opts, ?DEFAULT_MAX_ENTRIES),
    DefaultTTL = maps:get(cache_ttl_seconds, Opts, ?DEFAULT_TTL_SECONDS),

    %% Create ETS table for O(1) lookups
    Table = ets:new(?TABLE, [
        set,
        protected,
        {keypos, 1},
        {read_concurrency, true}
    ]),

    %% Schedule periodic cleanup
    schedule_cleanup(),

    ?LOG_INFO("NAT cache started (max_entries=~p, ttl=~p seconds)",
              [MaxEntries, DefaultTTL]),

    {ok, #state{
        table = Table,
        max_entries = MaxEntries,
        default_ttl = DefaultTTL,
        hits = 0,
        misses = 0,
        evictions = 0
    }}.

handle_call({get, NodeId}, _From, State) ->
    #state{table = Table, default_ttl = DefaultTTL} = State,
    Now = erlang:system_time(second),

    case ets:lookup(Table, NodeId) of
        [{NodeId, Profile, InsertedAt}] ->
            TTL = maps:get(ttl_seconds, Profile, DefaultTTL),
            Age = Now - InsertedAt,
            Result = classify_cache_entry(Age, TTL, Profile),
            NewState = case Result of
                {ok, _, _} -> State#state{hits = State#state.hits + 1};
                not_found -> State#state{misses = State#state.misses + 1}
            end,
            {reply, Result, NewState};
        [] ->
            {reply, not_found, State#state{misses = State#state.misses + 1}}
    end;

handle_call(stats, _From, State) ->
    #state{
        table = Table,
        max_entries = MaxEntries,
        hits = Hits,
        misses = Misses,
        evictions = Evictions
    } = State,

    Size = ets:info(Table, size),
    Total = Hits + Misses,
    HitRate = calculate_hit_rate(Hits, Total),

    Stats = #{
        size => Size,
        max_entries => MaxEntries,
        hits => Hits,
        misses => Misses,
        evictions => Evictions,
        hit_rate => HitRate
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({put, NodeId, Profile, TTL}, State) ->
    #state{table = Table} = State,
    Now = erlang:system_time(second),

    %% Evict if at capacity (simple LRU: remove oldest)
    NewState = maybe_evict(State),

    %% Store profile with TTL and timestamp
    ProfileWithTTL = Profile#{
        ttl_seconds => TTL,
        detected_at => Now
    },
    ets:insert(Table, {NodeId, ProfileWithTTL, Now}),

    %% Publish to DHT for peer discovery (best effort)
    publish_nat_profile_to_dht(NodeId, ProfileWithTTL),

    ?LOG_DEBUG("Cached NAT profile for ~s (ttl=~p)", [NodeId, TTL]),
    {noreply, NewState};

handle_cast({invalidate, NodeId}, State) ->
    ets:delete(State#state.table, NodeId),
    ?LOG_DEBUG("Invalidated NAT profile for ~s", [NodeId]),
    {noreply, State};

handle_cast(clear, State) ->
    ets:delete_all_objects(State#state.table),
    ?LOG_INFO("NAT cache cleared"),
    {noreply, State#state{hits = 0, misses = 0, evictions = 0}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    #state{table = Table, default_ttl = DefaultTTL} = State,
    Now = erlang:system_time(second),
    MaxAge = DefaultTTL + ?STALE_GRACE_SECONDS,

    ExpiredKeys = collect_expired_keys(Table, Now, MaxAge),
    lists:foreach(fun(Key) -> ets:delete(Table, Key) end, ExpiredKeys),
    log_cleanup_result(ExpiredKeys),

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
%% @doc Classify cache entry based on age.
-spec classify_cache_entry(integer(), integer(), nat_profile()) ->
    {ok, nat_profile(), fresh | stale} | not_found.
classify_cache_entry(Age, TTL, Profile) when Age =< TTL ->
    %% Fresh - use immediately
    {ok, Profile, fresh};
classify_cache_entry(Age, TTL, Profile) when Age =< TTL + ?STALE_GRACE_SECONDS ->
    %% Stale but usable - trigger background refresh
    %% Note: Actual refresh is triggered by caller (detector)
    {ok, Profile, stale};
classify_cache_entry(_Age, _TTL, _Profile) ->
    %% Expired - must re-detect
    not_found.

%% @private
%% @doc Evict oldest entry if at capacity.
-spec maybe_evict(#state{}) -> #state{}.
maybe_evict(#state{table = Table, max_entries = MaxEntries} = State) ->
    do_maybe_evict(ets:info(Table, size) >= MaxEntries, State).

do_maybe_evict(false, State) ->
    State;
do_maybe_evict(true, #state{table = Table, evictions = Evictions} = State) ->
    evict_oldest_if_found(find_oldest_entry(Table), Table, State, Evictions).

evict_oldest_if_found(not_found, _Table, State, _Evictions) ->
    State;
evict_oldest_if_found({ok, OldestKey}, Table, State, Evictions) ->
    ets:delete(Table, OldestKey),
    State#state{evictions = Evictions + 1}.

%% @private
%% @doc Find the oldest entry in the cache.
-spec find_oldest_entry(ets:tid()) -> {ok, binary()} | not_found.
find_oldest_entry(Table) ->
    ets_first_to_result(ets:first(Table)).

ets_first_to_result('$end_of_table') ->
    not_found;
ets_first_to_result(FirstKey) ->
    %% Simple approach: just evict the first key found
    %% For true LRU, we'd need to track access times
    {ok, FirstKey}.

%% @private
%% @doc Schedule periodic cleanup.
-spec schedule_cleanup() -> reference().
schedule_cleanup() ->
    erlang:send_after(?CLEANUP_INTERVAL_MS, self(), cleanup).

%% @private
%% @doc Calculate hit rate with safe division.
-spec calculate_hit_rate(non_neg_integer(), non_neg_integer()) -> float().
calculate_hit_rate(_Hits, 0) ->
    0.0;
calculate_hit_rate(Hits, Total) ->
    Hits / Total.

%% @private
%% @doc Collect keys of expired entries.
-spec collect_expired_keys(ets:tid(), integer(), integer()) -> [binary()].
collect_expired_keys(Table, Now, MaxAge) ->
    ets:foldl(
        fun({NodeId, _Profile, InsertedAt}, Acc) ->
            maybe_add_expired_key(NodeId, Now - InsertedAt > MaxAge, Acc)
        end,
        [],
        Table
    ).

maybe_add_expired_key(NodeId, true, Acc) -> [NodeId | Acc];
maybe_add_expired_key(_NodeId, false, Acc) -> Acc.

%% @private
%% @doc Log cleanup result.
-spec log_cleanup_result([binary()]) -> ok.
log_cleanup_result([]) ->
    ok;
log_cleanup_result(ExpiredKeys) ->
    ?LOG_DEBUG("NAT cache cleanup: removed ~p expired entries",
               [length(ExpiredKeys)]).

%% @private
%% @doc Publish NAT profile to DHT for peer discovery.
%% Key format: nat.profile.{node_id} (hashed with SHA256).
%% This is best-effort - continues silently if routing server unavailable.
-spec publish_nat_profile_to_dht(binary(), nat_profile()) -> ok.
publish_nat_profile_to_dht(NodeId, Profile) ->
    DhtKey = build_nat_dht_key(NodeId),
    DhtValue = prepare_profile_for_dht(Profile),
    do_publish_to_dht(whereis(macula_routing_server), NodeId, DhtKey, DhtValue).

do_publish_to_dht(undefined, NodeId, _DhtKey, _DhtValue) ->
    ?LOG_DEBUG("NAT cache: routing server not available, skipping DHT publish for ~s", [NodeId]),
    ok;
do_publish_to_dht(Pid, NodeId, DhtKey, DhtValue) ->
    %% Best effort - silently continue on failure
    _ = safe_dht_store(Pid, NodeId, DhtKey, DhtValue),
    ok.

safe_dht_store(Pid, NodeId, DhtKey, DhtValue) ->
    try
        macula_routing_server:store(Pid, DhtKey, DhtValue),
        ?LOG_DEBUG("Published NAT profile to DHT for ~s", [NodeId])
    catch
        _:Reason ->
            ?LOG_WARNING("Failed to publish NAT profile to DHT for ~s: ~p", [NodeId, Reason])
    end.

build_nat_dht_key(NodeId) ->
    KeyPrefix = <<"nat.profile.">>,
    crypto:hash(sha256, <<KeyPrefix/binary, NodeId/binary>>).

%% @private
%% @doc Prepare NAT profile for DHT storage.
%% Converts atoms to binaries for MessagePack serialization.
%% Includes optional geo-location fields if present.
-spec prepare_profile_for_dht(nat_profile()) -> map().
prepare_profile_for_dht(Profile) ->
    BaseProfile = #{
        <<"node_id">> => maps:get(node_id, Profile),
        <<"mapping_policy">> => atom_to_binary(maps:get(mapping_policy, Profile)),
        <<"filtering_policy">> => atom_to_binary(maps:get(filtering_policy, Profile)),
        <<"allocation_policy">> => atom_to_binary(maps:get(allocation_policy, Profile)),
        <<"can_receive_unsolicited">> => maps:get(can_receive_unsolicited, Profile, false),
        <<"requires_relay">> => maps:get(requires_relay, Profile, false),
        <<"relay_capable">> => maps:get(relay_capable, Profile, false),
        <<"detected_at">> => maps:get(detected_at, Profile, 0),
        <<"ttl_seconds">> => maps:get(ttl_seconds, Profile, ?DEFAULT_TTL_SECONDS)
    },
    %% Add geo fields if present
    add_geo_fields_to_dht_profile(Profile, BaseProfile).

%% @private
%% @doc Add geo-location fields to DHT profile if present in source profile.
add_geo_fields_to_dht_profile(Profile, DhtProfile) ->
    GeoFields = [
        {latitude, <<"latitude">>},
        {longitude, <<"longitude">>},
        {location_label, <<"location_label">>}
    ],
    lists:foldl(fun({Key, DhtKey}, Acc) ->
        case maps:get(Key, Profile, undefined) of
            undefined -> Acc;
            Value -> maps:put(DhtKey, Value, Acc)
        end
    end, DhtProfile, GeoFields).

%% @private
%% @doc Look up NAT profile from DHT.
%% If found, caches locally and returns the profile.
-spec lookup_nat_profile_from_dht(binary()) -> {ok, nat_profile()} | not_found | {error, term()}.
lookup_nat_profile_from_dht(NodeId) ->
    %% Build DHT key (same as publish)
    KeyPrefix = <<"nat.profile.">>,
    DhtKey = crypto:hash(sha256, <<KeyPrefix/binary, NodeId/binary>>),

    case whereis(macula_routing_server) of
        undefined ->
            {error, routing_server_unavailable};
        Pid ->
            case macula_routing_server:find_value(Pid, DhtKey, 20) of
                {ok, DhtValue} when is_map(DhtValue) ->
                    %% Convert DHT value back to internal format
                    Profile = parse_profile_from_dht(DhtValue),
                    %% Cache locally for future lookups
                    ?MODULE:put(NodeId, Profile),
                    {ok, Profile};
                {ok, []} ->
                    %% DHT returned empty result - value not found
                    not_found;
                {ok, _OtherValue} ->
                    %% DHT returned unexpected format
                    not_found;
                {nodes, _Nodes} ->
                    %% Value not found in DHT
                    not_found;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @private
%% @doc Parse NAT profile from DHT format back to internal format.
%% Includes optional geo-location fields if present.
-spec parse_profile_from_dht(map()) -> nat_profile().
parse_profile_from_dht(DhtValue) ->
    BaseProfile = #{
        node_id => maps:get(<<"node_id">>, DhtValue),
        mapping_policy => binary_to_existing_atom(maps:get(<<"mapping_policy">>, DhtValue, <<"ei">>), utf8),
        filtering_policy => binary_to_existing_atom(maps:get(<<"filtering_policy">>, DhtValue, <<"ei">>), utf8),
        allocation_policy => binary_to_existing_atom(maps:get(<<"allocation_policy">>, DhtValue, <<"pp">>), utf8),
        can_receive_unsolicited => maps:get(<<"can_receive_unsolicited">>, DhtValue, false),
        requires_relay => maps:get(<<"requires_relay">>, DhtValue, false),
        relay_capable => maps:get(<<"relay_capable">>, DhtValue, false),
        detected_at => maps:get(<<"detected_at">>, DhtValue, 0),
        ttl_seconds => maps:get(<<"ttl_seconds">>, DhtValue, ?DEFAULT_TTL_SECONDS)
    },
    %% Add geo fields if present in DHT value
    parse_geo_fields_from_dht(DhtValue, BaseProfile).

%% @private
%% @doc Parse geo-location fields from DHT value if present.
parse_geo_fields_from_dht(DhtValue, Profile) ->
    GeoFields = [
        {<<"latitude">>, latitude},
        {<<"longitude">>, longitude},
        {<<"location_label">>, location_label}
    ],
    lists:foldl(fun({DhtKey, Key}, Acc) ->
        case maps:get(DhtKey, DhtValue, undefined) of
            undefined -> Acc;
            Value -> maps:put(Key, Value, Acc)
        end
    end, Profile, GeoFields).
