%%%-------------------------------------------------------------------
%%% @doc
%%% LRU cache for RPC procedure results.
%%% Caches results of idempotent procedures to avoid repeated execution.
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
-type cache_entry() :: #{
    key := binary(),
    result := term(),
    timestamp := integer(),
    ttl := pos_integer()
}.

-type cache() :: #{
    entries := [cache_entry()],  % Ordered: head = most recent, tail = oldest
    max_size := pos_integer()
}.

-export_type([cache/0, cache_entry/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create new cache with max size.
-spec new(pos_integer()) -> cache().
new(MaxSize) ->
    #{
        entries => [],
        max_size => MaxSize
    }.

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
put_with_timestamp(#{entries := Entries, max_size := MaxSize} = Cache, Uri, Args, Result, TTL, Timestamp) ->
    Key = make_key(Uri, Args),

    Entry = #{
        key => Key,
        result => Result,
        timestamp => Timestamp,
        ttl => TTL
    },

    %% Remove existing entry for this key (if any)
    EntriesWithoutKey = lists:filter(
        fun(E) -> maps:get(key, E) =/= Key end,
        Entries
    ),

    %% Add new entry at head (most recent)
    NewEntries = [Entry | EntriesWithoutKey],

    %% Enforce max size (evict oldest)
    FinalEntries = case length(NewEntries) > MaxSize of
        true -> lists:sublist(NewEntries, MaxSize);
        false -> NewEntries
    end,

    Cache#{entries => FinalEntries}.

%% @doc Get entry from cache.
%% Returns {ok, Result, UpdatedCache} or not_found.
%% The updated cache has the entry moved to front (LRU).
-spec get(cache(), binary(), map()) -> {ok, term(), cache()} | not_found.
get(#{entries := Entries} = Cache, Uri, Args) ->
    Key = make_key(Uri, Args),

    case find_entry(Entries, Key) of
        {found, Entry} ->
            %% Check if expired
            case is_entry_expired(Entry) of
                true ->
                    %% Entry expired, remove it
                    _UpdatedCache = invalidate(Cache, Uri, Args),
                    not_found;

                false ->
                    %% Move to front (most recently used)
                    Result = maps:get(result, Entry),
                    TTL = maps:get(ttl, Entry),
                    Timestamp = erlang:system_time(millisecond),
                    UpdatedCache = put_with_timestamp(Cache, Uri, Args, Result, TTL, Timestamp),

                    {ok, Result, UpdatedCache}
            end;

        not_found ->
            not_found
    end.

%% @doc Invalidate (remove) entry.
-spec invalidate(cache(), binary(), map()) -> cache().
invalidate(#{entries := Entries} = Cache, Uri, Args) ->
    Key = make_key(Uri, Args),

    NewEntries = lists:filter(
        fun(E) -> maps:get(key, E) =/= Key end,
        Entries
    ),
    Cache#{entries => NewEntries}.

%% @doc Clear all entries.
-spec clear(cache()) -> cache().
clear(Cache) ->
    Cache#{entries => []}.

%% @doc Check if entry is expired.
-spec is_expired(cache(), binary(), map()) -> boolean().
is_expired(#{entries := Entries}, Uri, Args) ->
    Key = make_key(Uri, Args),

    case find_entry(Entries, Key) of
        {found, Entry} ->
            is_entry_expired(Entry);

        not_found ->
            true  % Missing entry is considered expired
    end.

%% @doc Get number of entries.
-spec size(cache()) -> non_neg_integer().
size(#{entries := Entries}) ->
    length(Entries).

%% @doc Get max size.
-spec max_size(cache()) -> pos_integer().
max_size(#{max_size := MaxSize}) ->
    MaxSize.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Find entry by key.
-spec find_entry([cache_entry()], binary()) -> {found, cache_entry()} | not_found.
find_entry([], _Key) ->
    not_found;
find_entry([Entry | Rest], Key) ->
    case maps:get(key, Entry) of
        Key -> {found, Entry};
        _ -> find_entry(Rest, Key)
    end.

%% @doc Check if entry is expired based on its TTL.
-spec is_entry_expired(cache_entry()) -> boolean().
is_entry_expired(Entry) ->
    Timestamp = maps:get(timestamp, Entry),
    TTL = maps:get(ttl, Entry),
    Now = erlang:system_time(millisecond),
    Age = Now - Timestamp,
    Age > (TTL * 1000).  % TTL in seconds, Age in milliseconds
