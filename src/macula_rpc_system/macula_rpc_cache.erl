%%%-------------------------------------------------------------------
%%% @doc
%%% LRU cache for RPC procedure results.
%%% Caches results of idempotent procedures to avoid repeated execution.
%%% Wraps macula_cache with RPC-specific logic and TTL handling.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_cache).

%% API
-export([
    new/1,
    put/5,
    put_with_timestamp/6,
    get/3,
    invalidate/3,
    clear/1,
    is_expired/3,
    size/1,
    max_size/1,
    make_key/2
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

%% @doc Make cache key from URI and args.
%% Uses hash of URI and args for consistent key generation.
-spec make_key(binary(), map()) -> binary().
make_key(Uri, Args) ->
    %% Sort args by key for consistent hashing
    SortedArgs = lists:sort(maps:to_list(Args)),
    Data = term_to_binary({Uri, SortedArgs}),
    crypto:hash(sha256, Data).

%% @doc Put entry in cache.
-spec put(cache(), binary(), map(), term(), pos_integer()) -> cache().
put(Cache, Uri, Args, Result, TTL) ->
    Timestamp = erlang:system_time(millisecond),
    put_with_timestamp(Cache, Uri, Args, Result, TTL, Timestamp).

%% @doc Put entry with custom timestamp (for testing).
-spec put_with_timestamp(cache(), binary(), map(), term(), pos_integer(), integer()) -> cache().
put_with_timestamp(Cache, Uri, Args, Result, TTL, Timestamp) ->
    Key = make_key(Uri, Args),
    Value = #{result => Result, ttl => TTL},
    macula_cache:put(Cache, Key, Value, Timestamp).

%% @doc Get entry from cache.
%% Returns {ok, Result, UpdatedCache} or not_found.
%% The updated cache has the entry moved to front (LRU).
%% Automatically removes expired entries.
-spec get(cache(), binary(), map()) -> {ok, term(), cache()} | not_found.
get(Cache, Uri, Args) ->
    Key = make_key(Uri, Args),

    case macula_cache:get(Cache, Key) of
        {ok, Value, TempCache} ->
            %% Check if expired
            Result = maps:get(result, Value),

            case is_value_expired(Value, TempCache, Key) of
                true ->
                    %% Entry expired, remove it
                    _UpdatedCache = macula_cache:remove(Cache, Key),
                    not_found;

                false ->
                    %% Valid entry, return it
                    %% Note: TempCache already has entry moved to front
                    {ok, Result, TempCache}
            end;

        not_found ->
            not_found
    end.

%% @doc Invalidate (remove) entry.
-spec invalidate(cache(), binary(), map()) -> cache().
invalidate(Cache, Uri, Args) ->
    Key = make_key(Uri, Args),
    macula_cache:remove(Cache, Key).

%% @doc Clear all entries.
-spec clear(cache()) -> cache().
clear(Cache) ->
    macula_cache:clear(Cache).

%% @doc Check if entry is expired.
-spec is_expired(cache(), binary(), map()) -> boolean().
is_expired(Cache, Uri, Args) ->
    Key = make_key(Uri, Args),

    case macula_cache:get(Cache, Key) of
        {ok, Value, UpdatedCache} ->
            is_value_expired(Value, UpdatedCache, Key);

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

%% @doc Check if value is expired based on its TTL and timestamp.
-spec is_value_expired(map(), cache(), term()) -> boolean().
is_value_expired(Value, Cache, Key) ->
    TTL = maps:get(ttl, Value),

    %% Get timestamp from cache entry
    case get_entry_timestamp(Cache, Key) of
        {ok, Timestamp} ->
            Now = erlang:system_time(millisecond),
            Age = Now - Timestamp,
            Age > (TTL * 1000);  % TTL in seconds, Age in milliseconds

        not_found ->
            true  % No timestamp means expired
    end.

%% @doc Get timestamp of entry (internal helper).
-spec get_entry_timestamp(cache(), term()) -> {ok, integer()} | not_found.
get_entry_timestamp(#{entries := Entries}, Key) ->
    case lists:search(fun(E) -> maps:get(key, E) =:= Key end, Entries) of
        {value, Entry} ->
            {ok, maps:get(timestamp, Entry)};
        false ->
            not_found
    end.
