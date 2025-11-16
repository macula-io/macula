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
    %% Check cache first
    case macula_cache:get(Cache, Key) of
        {ok, Values, UpdatedCache} ->
            %% Check if expired based on TTL
            case is_cache_entry_expired(UpdatedCache, Key, TTL) of
                true ->
                    %% Expired, query DHT
                    perform_dht_lookup(Key, Cache, LookupFun);

                false ->
                    %% Valid cached entry
                    {ok, Values, UpdatedCache}
            end;

        not_found ->
            %% Cache miss, query DHT
            perform_dht_lookup(Key, Cache, LookupFun)
    end.

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

    lists:filter(
        fun(Item) ->
            case maps:find(TimestampField, Item) of
                {ok, LastSeen} ->
                    Age = Now - LastSeen,
                    Age =< MaxAge;
                error ->
                    %% No timestamp field, assume expired
                    false
            end
        end,
        Items
    ).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Perform DHT lookup and update cache.
-spec perform_dht_lookup(key(), macula_cache:cache(), lookup_fun()) ->
    {ok, [value()], macula_cache:cache()} | {error, term(), macula_cache:cache()}.
perform_dht_lookup(Key, Cache, LookupFun) ->
    case LookupFun(Key) of
        {ok, Values} ->
            %% Cache the result
            UpdatedCache = macula_cache:put(Cache, Key, Values),
            {ok, Values, UpdatedCache};

        {error, Reason} ->
            %% Return error, don't update cache
            {error, Reason, Cache}
    end.

%% @doc Check if cache entry is expired based on TTL.
-spec is_cache_entry_expired(macula_cache:cache(), key(), pos_integer()) -> boolean().
is_cache_entry_expired(#{entries := Entries}, Key, TTL) ->
    case lists:search(fun(E) -> maps:get(key, E) =:= Key end, Entries) of
        {value, Entry} ->
            Timestamp = maps:get(timestamp, Entry),
            Now = erlang:system_time(millisecond),
            Age = Now - Timestamp,
            Age > (TTL * 1000);  % TTL in seconds, Age in milliseconds

        false ->
            true  % Not found means expired
    end.
