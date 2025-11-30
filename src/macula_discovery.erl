%%%-------------------------------------------------------------------
%%% @doc
%%% Generic DHT-based service discovery.
%%% Provides cache-integrated lookup and announcement operations.
%%% Used by both pub/sub and RPC discovery layers.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_discovery).

%% API
-export([
    find/2,
    find_with_cache/3,
    find_with_cache/4,
    announce/2,
    unannounce/2,
    filter_by_age/3
]).

%% Types
-type key() :: binary().
-type value() :: term().
-type lookup_fun() :: fun((key()) -> {ok, [value()]} | {error, term()}).
-type publish_fun() :: fun((key(), value()) -> ok | {error, term()}).
-type unpublish_fun() :: fun((key()) -> ok | {error, term()}).

-export_type([
    key/0,
    value/0,
    lookup_fun/0,
    publish_fun/0,
    unpublish_fun/0
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Find values for a key via DHT lookup.
-spec find(key(), lookup_fun()) -> {ok, [value()]} | {error, term()}.
find(Key, LookupFun) ->
    LookupFun(Key).

%% @doc Find values with cache (default TTL: 300 seconds).
-spec find_with_cache(key(), macula_cache:cache(), lookup_fun()) ->
    {ok, [value()], macula_cache:cache()} | {error, term(), macula_cache:cache()}.
find_with_cache(Key, Cache, LookupFun) ->
    find_with_cache(Key, Cache, LookupFun, 300).

%% @doc Find values with cache and custom TTL.
-spec find_with_cache(key(), macula_cache:cache(), lookup_fun(), pos_integer()) ->
    {ok, [value()], macula_cache:cache()} | {error, term(), macula_cache:cache()}.
find_with_cache(Key, Cache, LookupFun, TTL) ->
    CacheResult = macula_cache:get(Cache, Key),
    do_find_with_cache(CacheResult, Key, Cache, LookupFun, TTL).

%% @private Cache hit - check expiry
do_find_with_cache({ok, Values, UpdatedCache}, Key, Cache, LookupFun, TTL) ->
    Expired = is_cache_entry_expired(UpdatedCache, Key, TTL),
    handle_cache_expiry(Expired, Values, UpdatedCache, Key, Cache, LookupFun);
%% @private Cache miss - query DHT
do_find_with_cache(not_found, Key, Cache, LookupFun, _TTL) ->
    perform_dht_lookup(Key, Cache, LookupFun).

%% @private Cache entry expired - query DHT
handle_cache_expiry(true, _Values, _UpdatedCache, Key, Cache, LookupFun) ->
    perform_dht_lookup(Key, Cache, LookupFun);
%% @private Cache entry valid - return it
handle_cache_expiry(false, Values, UpdatedCache, _Key, _Cache, _LookupFun) ->
    {ok, Values, UpdatedCache}.

%% @doc Announce value to DHT.
-spec announce(key(), publish_fun()) -> ok | {error, term()}.
announce(Key, PublishFun) ->
    PublishFun(Key).

%% @doc Remove value from DHT.
-spec unannounce(key(), unpublish_fun()) -> ok | {error, term()}.
unannounce(Key, UnpublishFun) ->
    UnpublishFun(Key).

%% @doc Filter items by age based on last_seen timestamp and TTL.
%% Items must have #{last_seen := integer()} in their structure.
-spec filter_by_age([map()], pos_integer(), atom()) -> [map()].
filter_by_age(Items, TTL, TimestampField) ->
    Now = erlang:system_time(millisecond),
    MaxAge = TTL * 1000,  % TTL in seconds, convert to milliseconds
    lists:filter(fun(Item) -> is_item_fresh(Item, TimestampField, Now, MaxAge) end, Items).

%% @private Check if item is fresh based on timestamp
is_item_fresh(Item, TimestampField, Now, MaxAge) ->
    FindResult = maps:find(TimestampField, Item),
    check_item_freshness(FindResult, Now, MaxAge).

%% @private Timestamp found - check age
check_item_freshness({ok, LastSeen}, Now, MaxAge) ->
    Age = Now - LastSeen,
    Age =< MaxAge;
%% @private No timestamp field - assume expired
check_item_freshness(error, _Now, _MaxAge) ->
    false.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Perform DHT lookup and update cache.
-spec perform_dht_lookup(key(), macula_cache:cache(), lookup_fun()) ->
    {ok, [value()], macula_cache:cache()} | {error, term(), macula_cache:cache()}.
perform_dht_lookup(Key, Cache, LookupFun) ->
    LookupResult = LookupFun(Key),
    do_dht_lookup(LookupResult, Key, Cache).

%% @private DHT lookup succeeded - cache result
do_dht_lookup({ok, Values}, Key, Cache) ->
    UpdatedCache = macula_cache:put(Cache, Key, Values),
    {ok, Values, UpdatedCache};
%% @private DHT lookup failed - don't update cache
do_dht_lookup({error, Reason}, _Key, Cache) ->
    {error, Reason, Cache}.

%% @doc Check if cache entry is expired based on TTL.
-spec is_cache_entry_expired(macula_cache:cache(), key(), pos_integer()) -> boolean().
is_cache_entry_expired(#{entries := Entries}, Key, TTL) ->
    SearchResult = lists:search(fun(E) -> maps:get(key, E) =:= Key end, Entries),
    check_entry_expiry(SearchResult, TTL).

%% @private Entry found - check if expired
check_entry_expiry({value, Entry}, TTL) ->
    Timestamp = maps:get(timestamp, Entry),
    Now = erlang:system_time(millisecond),
    Age = Now - Timestamp,
    Age > (TTL * 1000);  % TTL in seconds, Age in milliseconds
%% @private Entry not found - consider expired
check_entry_expiry(false, _TTL) ->
    true.
