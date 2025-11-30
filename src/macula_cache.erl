%%%-------------------------------------------------------------------
%%% @doc
%%% Generic LRU cache implementation.
%%% Provides least-recently-used eviction with configurable max size.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cache).

%% API
-export([
    new/1,
    put/3,
    put/4,
    get/2,
    remove/2,
    clear/1,
    size/1,
    max_size/1,
    keys/1
]).

%% Types
-type key() :: term().
-type value() :: term().
-type timestamp() :: integer().

-type entry() :: #{
    key := key(),
    value := value(),
    timestamp := timestamp()
}.

-type cache() :: #{
    entries := [entry()],  % Ordered: head = most recent, tail = oldest
    max_size := pos_integer()
}.

-export_type([cache/0, entry/0, key/0, value/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create new cache with max size.
-spec new(pos_integer()) -> cache().
new(MaxSize) when MaxSize > 0 ->
    #{
        entries => [],
        max_size => MaxSize
    }.

%% @doc Put entry in cache with current timestamp.
-spec put(cache(), key(), value()) -> cache().
put(Cache, Key, Value) ->
    Timestamp = erlang:system_time(millisecond),
    put(Cache, Key, Value, Timestamp).

%% @doc Put entry in cache with custom timestamp (for testing).
-spec put(cache(), key(), value(), timestamp()) -> cache().
put(#{entries := Entries, max_size := MaxSize} = Cache, Key, Value, Timestamp) ->
    Entry = #{
        key => Key,
        value => Value,
        timestamp => Timestamp
    },

    %% Remove existing entry for this key (if any)
    EntriesWithoutKey = lists:filter(
        fun(E) -> maps:get(key, E) =/= Key end,
        Entries
    ),

    %% Add new entry at head (most recent)
    NewEntries = [Entry | EntriesWithoutKey],

    %% Enforce max size (evict oldest)
    FinalEntries = enforce_max_size(NewEntries, MaxSize),

    Cache#{entries => FinalEntries}.

%% @private Enforce max size by evicting oldest entries
enforce_max_size(Entries, MaxSize) when length(Entries) > MaxSize ->
    lists:sublist(Entries, MaxSize);
enforce_max_size(Entries, _MaxSize) ->
    Entries.

%% @doc Get entry from cache.
%% Returns {ok, Value, UpdatedCache} or not_found.
%% The updated cache has the entry moved to front (LRU).
-spec get(cache(), key()) -> {ok, value(), cache()} | not_found.
get(#{entries := Entries} = Cache, Key) ->
    FindResult = find_entry(Entries, Key),
    do_get(FindResult, Cache, Key).

%% @private Entry found - move to front
do_get({found, Entry}, Cache, Key) ->
    Value = maps:get(value, Entry),
    Timestamp = erlang:system_time(millisecond),
    UpdatedCache = put(Cache, Key, Value, Timestamp),
    {ok, Value, UpdatedCache};
%% @private Entry not found
do_get(not_found, _Cache, _Key) ->
    not_found.

%% @doc Remove entry from cache.
-spec remove(cache(), key()) -> cache().
remove(#{entries := Entries} = Cache, Key) ->
    NewEntries = lists:filter(
        fun(E) -> maps:get(key, E) =/= Key end,
        Entries
    ),
    Cache#{entries => NewEntries}.

%% @doc Clear all entries.
-spec clear(cache()) -> cache().
clear(Cache) ->
    Cache#{entries => []}.

%% @doc Get number of entries.
-spec size(cache()) -> non_neg_integer().
size(#{entries := Entries}) ->
    length(Entries).

%% @doc Get max size.
-spec max_size(cache()) -> pos_integer().
max_size(#{max_size := MaxSize}) ->
    MaxSize.

%% @doc Get all keys in cache (most recent first).
-spec keys(cache()) -> [key()].
keys(#{entries := Entries}) ->
    [maps:get(key, E) || E <- Entries].

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Find entry by key.
-spec find_entry([entry()], key()) -> {found, entry()} | not_found.
find_entry([], _Key) ->
    not_found;
find_entry([#{key := Key} = Entry | _Rest], Key) ->
    {found, Entry};
find_entry([_Entry | Rest], Key) ->
    find_entry(Rest, Key).
