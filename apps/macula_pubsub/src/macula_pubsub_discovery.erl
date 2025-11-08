%%%-------------------------------------------------------------------
%%% @doc
%%% DHT integration for finding remote subscribers.
%%% Uses Kademlia DHT to publish and discover subscriptions.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_discovery).

%% API
-export([
    find_subscribers/2,
    find_with_cache/3,
    find_with_cache/4,
    announce/4,
    unannounce/3,
    refresh_cache/2
]).

%% Types
-type pattern() :: binary().
-type node_id() :: binary().
-type address() :: {inet:ip_address(), inet:port_number()}.
-type subscriber() :: #{node_id := node_id(), address := address()}.

-type dht_lookup_fun() :: fun((pattern()) -> {ok, [subscriber()]} | {error, term()}).
-type dht_publish_fun() :: fun((pattern(), node_id(), address()) -> ok | {error, term()}).
-type dht_unpublish_fun() :: fun((pattern(), node_id()) -> ok | {error, term()}).

-export_type([
    pattern/0,
    node_id/0,
    address/0,
    subscriber/0,
    dht_lookup_fun/0,
    dht_publish_fun/0,
    dht_unpublish_fun/0
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Find remote subscribers for a pattern via DHT.
-spec find_subscribers(pattern(), dht_lookup_fun()) -> {ok, [subscriber()]} | {error, term()}.
find_subscribers(Pattern, DhtLookupFun) ->
    DhtLookupFun(Pattern).

%% @doc Find subscribers with cache (default TTL: 300 seconds).
-spec find_with_cache(pattern(), macula_pubsub_cache:cache(), dht_lookup_fun()) ->
    {ok, [subscriber()], macula_pubsub_cache:cache()}.
find_with_cache(Pattern, Cache, DhtLookupFun) ->
    find_with_cache(Pattern, Cache, DhtLookupFun, 300).

%% @doc Find subscribers with cache and custom TTL.
-spec find_with_cache(pattern(), macula_pubsub_cache:cache(), dht_lookup_fun(), pos_integer()) ->
    {ok, [subscriber()], macula_pubsub_cache:cache()}.
find_with_cache(Pattern, Cache, DhtLookupFun, TTL) ->
    %% Check if cached entry is fresh
    case macula_pubsub_cache:is_expired(Cache, Pattern, TTL) of
        true ->
            %% Cache miss or expired, query DHT
            case DhtLookupFun(Pattern) of
                {ok, Subscribers} ->
                    %% Cache the result
                    Cache2 = macula_pubsub_cache:put(Cache, Pattern, Subscribers),
                    {ok, Subscribers, Cache2};

                {error, Reason} ->
                    %% Return error, don't update cache
                    {error, Reason, Cache}
            end;

        false ->
            %% Cache hit with fresh entry
            {ok, Subscribers, Cache2} = macula_pubsub_cache:get(Cache, Pattern),
            {ok, Subscribers, Cache2}
    end.

%% @doc Announce local subscription to DHT.
-spec announce(pattern(), node_id(), address(), dht_publish_fun()) -> ok | {error, term()}.
announce(Pattern, LocalNodeId, LocalAddress, DhtPublishFun) ->
    DhtPublishFun(Pattern, LocalNodeId, LocalAddress).

%% @doc Remove local subscription from DHT.
-spec unannounce(pattern(), node_id(), dht_unpublish_fun()) -> ok | {error, term()}.
unannounce(Pattern, LocalNodeId, DhtUnpublishFun) ->
    DhtUnpublishFun(Pattern, LocalNodeId).

%% @doc Refresh cache by removing expired entries.
-spec refresh_cache(macula_pubsub_cache:cache(), pos_integer()) -> macula_pubsub_cache:cache().
refresh_cache(Cache, TTL) ->
    %% Get all patterns in cache
    Patterns = get_all_patterns(Cache),

    %% Invalidate expired patterns
    lists:foldl(
        fun(Pattern, AccCache) ->
            case macula_pubsub_cache:is_expired(AccCache, Pattern, TTL) of
                true -> macula_pubsub_cache:invalidate(AccCache, Pattern);
                false -> AccCache
            end
        end,
        Cache,
        Patterns
    ).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Extract all patterns from cache.
%% NOTE: This is a workaround since macula_pubsub_cache doesn't expose list_patterns.
%% We reconstruct it from the cache entries.
-spec get_all_patterns(macula_pubsub_cache:cache()) -> [pattern()].
get_all_patterns(#{entries := Entries}) ->
    lists:map(
        fun(Entry) -> maps:get(pattern, Entry) end,
        Entries
    ).
