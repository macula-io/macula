%%%-------------------------------------------------------------------
%%% @doc
%%% LRU cache for remote subscriber lists.
%%% Caches DHT query results to avoid repeated lookups.
%%% Wraps macula_cache with subscriber-specific logic.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_cache).

%% API
-export([
    new/1,
    put/3,
    put_with_timestamp/4,
    get/2,
    invalidate/2,
    clear/1,
    is_expired/3,
    size/1,
    max_size/1
]).

%% Types
-type cache() :: macula_cache:cache().
-export_type([cache/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create new cache with max size.
-spec new(pos_integer()) -> cache().
new(MaxSize) ->
    macula_cache:new(MaxSize).

%% @doc Put entry in cache.
-spec put(cache(), binary(), [map()]) -> cache().
put(Cache, Pattern, Subscribers) ->
    Timestamp = erlang:system_time(millisecond),
    put_with_timestamp(Cache, Pattern, Subscribers, Timestamp).

%% @doc Put entry with custom timestamp (for testing).
-spec put_with_timestamp(cache(), binary(), [map()], integer()) -> cache().
put_with_timestamp(Cache, Pattern, Subscribers, Timestamp) ->
    macula_cache:put(Cache, Pattern, Subscribers, Timestamp).

%% @doc Get entry from cache.
%% Returns {ok, Subscribers, UpdatedCache} or not_found.
%% The updated cache has the entry moved to front (LRU).
-spec get(cache(), binary()) -> {ok, [map()], cache()} | not_found.
get(Cache, Pattern) ->
    macula_cache:get(Cache, Pattern).

%% @doc Invalidate (remove) entry.
-spec invalidate(cache(), binary()) -> cache().
invalidate(Cache, Pattern) ->
    macula_cache:remove(Cache, Pattern).

%% @doc Clear all entries.
-spec clear(cache()) -> cache().
clear(Cache) ->
    macula_cache:clear(Cache).

%% @doc Check if entry is expired based on TTL.
%% This checks if entry exists and its age exceeds TTL.
-spec is_expired(cache(), binary(), pos_integer()) -> boolean().
is_expired(Cache, Pattern, TTL) ->
    case macula_cache:get(Cache, Pattern) of
        {ok, _Subscribers, UpdatedCache} ->
            %% Get the entry's timestamp
            case get_entry_timestamp(UpdatedCache, Pattern) of
                {ok, Timestamp} ->
                    Now = erlang:system_time(millisecond),
                    Age = Now - Timestamp,
                    Age > (TTL * 1000);  % TTL in seconds, Age in milliseconds

                not_found ->
                    true  % Should not happen, but treat as expired
            end;

        not_found ->
            true  % Missing entry is considered expired
    end.

%% @doc Get number of entries.
-spec size(cache()) -> non_neg_integer().
size(Cache) ->
    macula_cache:size(Cache).

%% @doc Get max size.
-spec max_size(cache()) -> pos_integer().
max_size(Cache) ->
    macula_cache:max_size(Cache).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Get timestamp of entry (internal helper for is_expired).
-spec get_entry_timestamp(cache(), binary()) -> {ok, integer()} | not_found.
get_entry_timestamp(#{entries := Entries}, Pattern) ->
    case lists:search(fun(E) -> maps:get(key, E) =:= Pattern end, Entries) of
        {value, Entry} ->
            {ok, maps:get(timestamp, Entry)};
        false ->
            not_found
    end.
