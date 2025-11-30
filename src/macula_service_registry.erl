%%%-------------------------------------------------------------------
%%% @doc
%%% Decentralized service advertisement registry using DHT.
%%%
%%% Provides service discovery via Kademlia DHT instead of centralized
%%% registration. Services advertise their capabilities to the DHT,
%%% and clients discover providers by querying the DHT.
%%%
%%% == Architecture ==
%%%
%%% - Services advertise: "I provide procedure X" → DHT stores node_id at key=hash(procedure)
%%% - Clients discover: "Who provides procedure X?" → DHT returns list of node_ids
%%% - Local cache: Recent discoveries cached with TTL for low-latency lookups
%%% - Re-advertisement: Periodic republish to DHT for TTL renewal (default: every 5 min)
%%%
%%% == Features ==
%%%
%%% - Fully decentralized (no central authority)
%%% - Multiple providers supported (DHT returns list)
%%% - Load balancing (client picks from list)
%%% - Fault tolerant (try another provider if one fails)
%%% - Low latency after first lookup (local cache)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(macula_service_registry).

%% API
-export([
    new/0,
    new/1,

    %% Local service management
    advertise_local/4,
    unadvertise_local/2,
    get_local_handler/2,
    list_local_services/1,

    %% Service discovery (with cache)
    discover_service/2,
    discover_service/3,
    cache_service/4,

    %% Subscriber discovery (pub/sub cache)
    discover_subscribers/2,
    cache_subscribers/4,
    prune_expired_subscribers/1,
    clear_subscriber_cache/1,

    %% DHT integration
    publish_to_dht/5,
    query_dht_for_service/3,
    remove_from_dht/3,

    %% Cache management
    prune_expired/1,
    clear_cache/1,

    %% Local service cleanup
    prune_expired_local_services/1
]).

%% Types
-type service_id() :: binary().
%% Service identifier (procedure URI). Example: &lt;&lt;"energy.home.get"&gt;&gt;.

-type node_id() :: binary().
%% 32-byte node identifier.

-type handler_fn() :: fun((map()) -> {ok, term()} | {error, term()}).
%% Handler function for local service implementations.

-type provider_info() :: #{
    node_id := node_id(),
    endpoint := binary(),       % Connection endpoint
    metadata := map(),          % Custom metadata
    advertised_at := integer()  % Unix timestamp
}.

-type cache_entry() :: #{
    service_id := service_id(),
    providers := [provider_info()],
    cached_at := integer(),     % Unix timestamp
    ttl := pos_integer()        % Seconds
}.

-type local_service() :: #{
    service_id := service_id(),
    handler := handler_fn(),
    metadata := map(),
    advertised_at := integer()
}.

-type registry() :: #{
    %% Local services this node provides
    local_services := #{service_id() => local_service()},

    %% Discovery cache: #{ServiceId => CacheEntry}
    cache := #{service_id() => cache_entry()},

    %% Subscriber cache: #{Topic => CacheEntry}
    subscriber_cache := #{binary() => cache_entry()},

    %% Configuration
    default_ttl := pos_integer(),       % Default TTL in seconds
    cache_ttl := pos_integer(),         % How long to cache discoveries
    service_ttl := pos_integer()        % TTL for local services (cleanup)
}.

-export_type([
    service_id/0,
    node_id/0,
    handler_fn/0,
    provider_info/0,
    registry/0
]).

-include("macula_config.hrl").

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create new empty service registry with default settings.
-spec new() -> registry().
new() ->
    new(#{}).

%% @doc Create new service registry with custom options.
%%
%% Options:
%% - `default_ttl' - Default TTL for DHT advertisements (default: 300s)
%% - `cache_ttl' - How long to cache discovered services (default: 60s)
%% - `service_ttl' - TTL for local services before cleanup (default: 300s, 5 minutes)
-spec new(map()) -> registry().
new(Opts) ->
    #{
        local_services => #{},
        cache => #{},
        subscriber_cache => #{},
        default_ttl => maps:get(default_ttl, Opts, ?DEFAULT_TTL),
        cache_ttl => maps:get(cache_ttl, Opts, ?CACHE_TTL),
        service_ttl => maps:get(service_ttl, Opts, 300)  % 5 minutes default
    }.

%% @doc Advertise a service locally (stores handler for incoming calls).
%%
%% This registers the service handler locally so this node can respond
%% to incoming RPC calls. The actual DHT advertisement must be done
%% separately (see `publish_to_dht/4').
-spec advertise_local(registry(), service_id(), handler_fn(), map()) -> registry().
advertise_local(#{local_services := Services} = Registry, ServiceId, Handler, Metadata) ->
    LocalService = #{
        service_id => ServiceId,
        handler => Handler,
        metadata => Metadata,
        advertised_at => erlang:system_time(second)
    },

    NewServices = Services#{ServiceId => LocalService},
    Registry#{local_services => NewServices}.

%% @doc Remove a local service advertisement.
-spec unadvertise_local(registry(), service_id()) -> registry().
unadvertise_local(#{local_services := Services} = Registry, ServiceId) ->
    NewServices = maps:remove(ServiceId, Services),
    Registry#{local_services => NewServices}.

%% @doc Get handler function for a locally advertised service.
-spec get_local_handler(registry(), service_id()) -> {ok, handler_fn()} | not_found.
get_local_handler(#{local_services := Services}, ServiceId) ->
    get_handler_from_service(maps:get(ServiceId, Services, undefined)).

get_handler_from_service(undefined) ->
    not_found;
get_handler_from_service(#{handler := Handler}) ->
    {ok, Handler}.

%% @doc List all locally advertised services.
-spec list_local_services(registry()) -> [service_id()].
list_local_services(#{local_services := Services}) ->
    maps:keys(Services).

%% @doc Discover service providers (checks cache first, returns cached if available).
-spec discover_service(registry(), service_id()) ->
    {ok, [provider_info()], registry()} | {cache_miss, registry()}.
discover_service(Registry, ServiceId) ->
    discover_service(Registry, ServiceId, #{}).

%% @doc Discover service providers with options.
%%
%% Checks local cache first. If found and not expired, returns cached providers.
%% If cache miss or expired, returns `{cache_miss, Registry}' so caller can
%% query DHT.
%%
%% Options:
%% - `force_refresh' - Skip cache, force DHT lookup (default: false)
-spec discover_service(registry(), service_id(), map()) ->
    {ok, [provider_info()], registry()} | {cache_miss, registry()}.
discover_service(Registry, _ServiceId, #{force_refresh := true}) ->
    {cache_miss, Registry};
discover_service(#{cache := Cache, cache_ttl := CacheTTL} = Registry, ServiceId, _Opts) ->
    check_service_cache(maps:get(ServiceId, Cache, undefined), CacheTTL, Registry).

%% Check cache entry and return result based on expiry
check_service_cache(undefined, _CacheTTL, Registry) ->
    {cache_miss, Registry};
check_service_cache(#{cached_at := CachedAt, providers := Providers}, CacheTTL, Registry) ->
    Now = erlang:system_time(second),
    Age = Now - CachedAt,
    check_cache_expiry(Age >= CacheTTL, Providers, Registry).

%% Return result based on whether cache is expired
check_cache_expiry(true, _Providers, Registry) ->
    {cache_miss, Registry};
check_cache_expiry(false, Providers, Registry) ->
    {ok, Providers, Registry}.

%% @doc Cache discovered service providers.
%%
%% Stores providers in local cache with TTL. Subsequent `discover_service'
%% calls will return cached results until TTL expires.
-spec cache_service(registry(), service_id(), [provider_info()], pos_integer()) -> registry().
cache_service(#{cache := Cache} = Registry, ServiceId, Providers, TTL) ->
    CacheEntry = #{
        service_id => ServiceId,
        providers => Providers,
        cached_at => erlang:system_time(second),
        ttl => TTL
    },

    NewCache = Cache#{ServiceId => CacheEntry},
    Registry#{cache => NewCache}.

%% @doc Remove expired entries from discovery cache.
%%
%% Should be called periodically to prevent memory leaks.
%% Returns updated registry and count of removed entries.
-spec prune_expired(registry()) -> {registry(), non_neg_integer()}.
prune_expired(#{cache := Cache, cache_ttl := CacheTTL} = Registry) ->
    Now = erlang:system_time(second),

    {NewCache, Removed} = maps:fold(
        fun(ServiceId, CacheEntry, {Acc, Count}) ->
            CachedAt = maps:get(cached_at, CacheEntry),
            Age = Now - CachedAt,
            prune_cache_by_age(Age, CacheTTL, ServiceId, CacheEntry, Acc, Count)
        end,
        {#{}, 0},
        Cache
    ),

    {Registry#{cache => NewCache}, Removed}.

%% @private Cache entry expired - don't include (>= allows 0-second TTL)
prune_cache_by_age(Age, CacheTTL, _ServiceId, _CacheEntry, Acc, Count)
  when Age >= CacheTTL ->
    {Acc, Count + 1};
%% @private Cache entry still valid
prune_cache_by_age(_Age, _CacheTTL, ServiceId, CacheEntry, Acc, Count) ->
    {Acc#{ServiceId => CacheEntry}, Count}.

%% @doc Clear the entire discovery cache.
-spec clear_cache(registry()) -> registry().
clear_cache(Registry) ->
    Registry#{cache => #{}}.

%% @doc Remove expired local services.
%%
%% Should be called periodically to prevent memory leaks from stale service
%% registrations. Returns updated registry and count of removed services.
-spec prune_expired_local_services(registry()) -> {registry(), non_neg_integer()}.
prune_expired_local_services(#{local_services := Services, service_ttl := ServiceTTL} = Registry) ->
    Now = erlang:system_time(second),

    {NewServices, Removed} = maps:fold(
        fun(ServiceId, LocalService, {Acc, Count}) ->
            AdvertisedAt = maps:get(advertised_at, LocalService),
            Age = Now - AdvertisedAt,

            prune_service_by_age(Age, ServiceTTL, ServiceId, LocalService, Acc, Count)
        end,
        {#{}, 0},
        Services
    ),

    {Registry#{local_services => NewServices}, Removed}.

%% Pattern match with guard for service expiry check
prune_service_by_age(Age, ServiceTTL, _ServiceId, _LocalService, Acc, Count)
  when Age >= ServiceTTL ->
    %% Expired - don't include (>= allows 0-second TTL for testing)
    {Acc, Count + 1};
prune_service_by_age(_Age, _ServiceTTL, ServiceId, LocalService, Acc, Count) ->
    %% Still valid
    {Acc#{ServiceId => LocalService}, Count}.

%%%===================================================================
%%% Subscriber Cache Functions (Pub/Sub)
%%%===================================================================

%% @doc Discover subscribers for a topic (checks cache first).
%%
%% Similar to discover_service/2 but for pub/sub subscribers.
%% Returns cached subscribers if found and not expired, otherwise cache_miss.
-spec discover_subscribers(registry(), binary()) ->
    {ok, [provider_info()], registry()} | {cache_miss, registry()}.
discover_subscribers(#{subscriber_cache := Cache, cache_ttl := CacheTTL} = Registry, Topic) ->
    CacheEntry = maps:get(Topic, Cache, undefined),
    do_discover_subscribers(CacheEntry, CacheTTL, Registry).

%% @private No cache entry
do_discover_subscribers(undefined, _CacheTTL, Registry) ->
    {cache_miss, Registry};
%% @private Cache entry found - check expiry
do_discover_subscribers(CacheEntry, CacheTTL, Registry) ->
    check_subscriber_cache_expiry(CacheEntry, CacheTTL, Registry).

%% Check if cached entry is expired (pattern matching)
check_subscriber_cache_expiry(#{cached_at := CachedAt, providers := Subscribers}, CacheTTL, Registry) ->
    Now = erlang:system_time(second),
    Age = Now - CachedAt,
    check_expiry_by_age(Age, CacheTTL, Subscribers, Registry).

%% Guard-based expiry check
check_expiry_by_age(Age, CacheTTL, Subscribers, Registry) when Age < CacheTTL ->
    {ok, Subscribers, Registry};
check_expiry_by_age(_Age, _CacheTTL, _Subscribers, Registry) ->
    {cache_miss, Registry}.

%% @doc Cache discovered subscribers for a topic.
%%
%% Stores subscribers in local cache with TTL. Subsequent discover_subscribers/2
%% calls will return cached results until TTL expires.
-spec cache_subscribers(registry(), binary(), [provider_info()], pos_integer()) -> registry().
cache_subscribers(#{subscriber_cache := Cache} = Registry, Topic, Subscribers, TTL) ->
    CacheEntry = #{
        service_id => Topic,
        providers => Subscribers,
        cached_at => erlang:system_time(second),
        ttl => TTL
    },
    NewCache = Cache#{Topic => CacheEntry},
    Registry#{subscriber_cache => NewCache}.

%% @doc Remove expired subscriber cache entries.
%%
%% Should be called periodically to prevent memory leaks.
%% Returns updated registry and count of removed entries.
-spec prune_expired_subscribers(registry()) -> {registry(), non_neg_integer()}.
prune_expired_subscribers(#{subscriber_cache := Cache, cache_ttl := CacheTTL} = Registry) ->
    Now = erlang:system_time(second),
    {NewCache, Removed} = maps:fold(
        fun(Topic, CacheEntry, {Acc, Count}) ->
            prune_if_expired(Topic, CacheEntry, Now, CacheTTL, Acc, Count)
        end,
        {#{}, 0},
        Cache
    ),
    {Registry#{subscriber_cache => NewCache}, Removed}.

%% Pattern match with guard for expiry check
prune_if_expired(_Topic, #{cached_at := CachedAt}, Now, CacheTTL, Acc, Count)
  when Now - CachedAt >= CacheTTL ->
    %% Expired - don't include (>= allows 0-second TTL)
    {Acc, Count + 1};
prune_if_expired(Topic, CacheEntry, _Now, _CacheTTL, Acc, Count) ->
    %% Still valid
    {Acc#{Topic => CacheEntry}, Count}.

%% @doc Clear the entire subscriber cache.
-spec clear_subscriber_cache(registry()) -> registry().
clear_subscriber_cache(Registry) ->
    Registry#{subscriber_cache => #{}}.

%%%===================================================================
%%% DHT Integration Functions
%%%===================================================================

%% @doc Publish a service advertisement to the DHT.
%%
%% This function publishes a service's provider information to the DHT
%% so other nodes can discover it. The service_id is hashed to create
%% a DHT key, and the provider information is stored at that key.
%%
%% Parameters:
%% - DhtPid: Process ID or registered name of macula_routing_server
%% - ServiceId: The service identifier (procedure URI)
%% - ProviderInfo: Information about this provider (node_id, endpoint, metadata)
%% - TTL: Time-to-live in seconds for this advertisement
%% - K: Number of nodes to store at (typically 20 for Kademlia)
%%
%% Returns:
%% - ok if successful
%% - {error, Reason} if publication failed
%%
%% Example:
%% ```
%% ProviderInfo = #{
%%     node_id => <<"my-node-123">>,
%%     endpoint => <<"https://localhost:9443">>,
%%     metadata => #{version => <<"1.0">>}
%% },
%% ok = publish_to_dht(DhtPid, &lt;&lt;"energy.home.get"&gt;&gt;, ProviderInfo, 300, 20).
%% '''
-spec publish_to_dht(pid() | atom(), service_id(), provider_info(), pos_integer(), pos_integer()) ->
    ok | {error, term()}.
publish_to_dht(DhtPid, ServiceId, ProviderInfo, TTL, _K) ->
    Key = service_key(ServiceId),
    EnrichedProviderInfo = ProviderInfo#{
        advertised_at => erlang:system_time(second),
        ttl => TTL
    },
    ResolvedPid = resolve_pid(DhtPid),
    do_publish_to_dht(ResolvedPid, Key, EnrichedProviderInfo).

%% @private DHT not available - configuration error
do_publish_to_dht(undefined, _Key, _ProviderInfo) ->
    error(dht_not_available);
%% @private DHT available - store with propagation
do_publish_to_dht(Pid, Key, ProviderInfo) when is_pid(Pid) ->
    ok = macula_routing_server:store(Pid, Key, ProviderInfo),
    ok.

%% @doc Query the DHT for service providers.
%%
%% This function queries the DHT to find nodes that provide a given service.
%% It returns a list of provider_info() maps, each containing node_id, endpoint,
%% and metadata for a provider.
%%
%% Parameters:
%% - DhtPid: Process ID or registered name of macula_routing_server
%% - ServiceId: The service identifier to query for
%% - K: Number of closest nodes to query (typically 20 for Kademlia)
%%
%% Returns:
%% - {ok, [ProviderInfo]} if providers found
%% - {ok, []} if no providers found
%% - {error, Reason} if query failed
%%
%% Example:
%% ```
%% {ok, Providers} = query_dht_for_service(DhtPid, &lt;&lt;"energy.home.get"&gt;&gt;, 20),
%% %% Returns: [{ok, [#{node_id => ..., endpoint => ..., metadata => ...}]}]
%% '''
-spec query_dht_for_service(pid() | atom(), service_id(), pos_integer()) ->
    {ok, [provider_info()]} | {error, term()}.
query_dht_for_service(DhtPid, ServiceId, K) ->
    Key = service_key(ServiceId),
    ResolvedPid = resolve_pid(DhtPid),
    do_query_dht(ResolvedPid, Key, K).

%% @private DHT not available
do_query_dht(undefined, _Key, _K) ->
    error(dht_not_available);
%% @private DHT available - perform query
do_query_dht(Pid, Key, K) when is_pid(Pid) ->
    QueryResult = macula_routing_server:find_value(Pid, Key, K),
    handle_dht_query_result(QueryResult).

%% @private Single provider stored
handle_dht_query_result({ok, Value}) when is_map(Value) ->
    {ok, [Value]};
%% @private Multiple providers stored
handle_dht_query_result({ok, Values}) when is_list(Values) ->
    {ok, Values};
%% @private Value not found in DHT
handle_dht_query_result({nodes, _Nodes}) ->
    {ok, []};
%% @private DHT query failed
handle_dht_query_result({error, Reason}) ->
    error({dht_query_failed, Reason});
%% @private Unexpected DHT response
handle_dht_query_result(Other) ->
    error({unexpected_dht_response, Other}).

%% @doc Remove a service advertisement from the DHT.
%%
%% This function removes a service advertisement when unadvertising.
%% Note: In practice, DHT entries expire naturally via TTL, so this
%% is optional and mainly useful for immediate cleanup.
%%
%% Parameters:
%% - DhtPid: Process ID or registered name of macula_routing_server
%% - ServiceId: The service identifier to remove
%% - NodeId: This node's identifier (to remove only this provider)
%%
%% Returns:
%% - ok if successful or entry not found
%% - {error, Reason} if removal failed
-spec remove_from_dht(pid() | atom(), service_id(), node_id()) ->
    ok | {error, term()}.
remove_from_dht(DhtPid, ServiceId, NodeId) ->
    %% Compute DHT key from service_id
    Key = service_key(ServiceId),

    %% Remove specific provider from DHT by node_id (best effort)
    ResolvedPid = resolve_pid(DhtPid),
    do_remove_from_dht(ResolvedPid, Key, NodeId).

%% Pattern match on whereis result for best-effort cleanup
do_remove_from_dht(undefined, _Key, _NodeId) ->
    ok;  % DHT not available, nothing to remove
do_remove_from_dht(Pid, Key, NodeId) when is_pid(Pid) ->
    %% Remove this specific provider from the list
    %% Pattern match on expected success, crash on unexpected errors
    ok = gen_server:call(Pid, {delete_local, Key, NodeId}),
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Compute DHT key for a service.
%% Uses SHA-256 hash of service_id to create a 32-byte DHT key.
-spec service_key(service_id()) -> binary().
service_key(ServiceId) ->
    crypto:hash(sha256, ServiceId).

%% @doc Resolve PID or atom to a PID.
%% Handles both direct PIDs and registered names.
-spec resolve_pid(pid() | atom()) -> pid() | undefined.
resolve_pid(Pid) when is_pid(Pid) ->
    Pid;
resolve_pid(Name) when is_atom(Name) ->
    whereis(Name);
resolve_pid(_) ->
    undefined.
