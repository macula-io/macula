%%%-------------------------------------------------------------------
%%% @doc
%%% LRU cache for remote subscriber lists.
%%% Caches DHT query results to avoid repeated lookups.
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
-type cache_entry() :: #{
    pattern := binary(),
    subscribers := [map()],
    timestamp := integer()
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

%% @doc Put entry in cache.
-spec put(cache(), binary(), [map()]) -> cache().
put(Cache, Pattern, Subscribers) ->
    Timestamp = erlang:system_time(millisecond),
    put_with_timestamp(Cache, Pattern, Subscribers, Timestamp).

%% @doc Put entry with custom timestamp (for testing).
-spec put_with_timestamp(cache(), binary(), [map()], integer()) -> cache().
put_with_timestamp(#{entries := Entries, max_size := MaxSize} = Cache, Pattern, Subscribers, Timestamp) ->
    Entry = #{
        pattern => Pattern,
        subscribers => Subscribers,
        timestamp => Timestamp
    },

    %% Remove existing entry for this pattern (if any)
    EntriesWithoutPattern = lists:filter(
        fun(E) -> maps:get(pattern, E) =/= Pattern end,
        Entries
    ),

    %% Add new entry at head (most recent)
    NewEntries = [Entry | EntriesWithoutPattern],

    %% Enforce max size (evict oldest)
    FinalEntries = case length(NewEntries) > MaxSize of
        true -> lists:sublist(NewEntries, MaxSize);
        false -> NewEntries
    end,

    Cache#{entries => FinalEntries}.

%% @doc Get entry from cache.
%% Returns {ok, Subscribers, UpdatedCache} or not_found.
%% The updated cache has the entry moved to front (LRU).
-spec get(cache(), binary()) -> {ok, [map()], cache()} | not_found.
get(#{entries := Entries} = Cache, Pattern) ->
    case find_entry(Entries, Pattern) of
        {found, Entry} ->
            %% Move to front (most recently used)
            Subscribers = maps:get(subscribers, Entry),
            Timestamp = erlang:system_time(millisecond),
            UpdatedCache = put_with_timestamp(Cache, Pattern, Subscribers, Timestamp),

            {ok, Subscribers, UpdatedCache};

        not_found ->
            not_found
    end.

%% @doc Invalidate (remove) entry.
-spec invalidate(cache(), binary()) -> cache().
invalidate(#{entries := Entries} = Cache, Pattern) ->
    NewEntries = lists:filter(
        fun(E) -> maps:get(pattern, E) =/= Pattern end,
        Entries
    ),
    Cache#{entries => NewEntries}.

%% @doc Clear all entries.
-spec clear(cache()) -> cache().
clear(Cache) ->
    Cache#{entries => []}.

%% @doc Check if entry is expired based on TTL.
-spec is_expired(cache(), binary(), pos_integer()) -> boolean().
is_expired(#{entries := Entries}, Pattern, TTL) ->
    case find_entry(Entries, Pattern) of
        {found, Entry} ->
            Timestamp = maps:get(timestamp, Entry),
            Now = erlang:system_time(millisecond),
            Age = Now - Timestamp,
            Age > (TTL * 1000);  % TTL in seconds, Age in milliseconds

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

%% @doc Find entry by pattern.
-spec find_entry([cache_entry()], binary()) -> {found, cache_entry()} | not_found.
find_entry([], _Pattern) ->
    not_found;
find_entry([Entry | Rest], Pattern) ->
    case maps:get(pattern, Entry) of
        Pattern -> {found, Entry};
        _ -> find_entry(Rest, Pattern)
    end.
