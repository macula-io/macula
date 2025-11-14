%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_cache module.
%%% Tests LRU cache functionality.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_cache_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% new/1 Tests
%%%===================================================================

new_creates_empty_cache_test() ->
    Cache = macula_cache:new(10),
    ?assertEqual(0, macula_cache:size(Cache)).

new_sets_max_size_test() ->
    Cache = macula_cache:new(42),
    ?assertEqual(42, macula_cache:max_size(Cache)).

new_returns_map_test() ->
    Cache = macula_cache:new(10),
    ?assert(is_map(Cache)).

new_has_empty_entries_test() ->
    Cache = macula_cache:new(10),
    ?assertEqual([], macula_cache:keys(Cache)).

%%%===================================================================
%%% put/3 and put/4 Tests
%%%===================================================================

put_adds_entry_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    ?assertEqual(1, macula_cache:size(Cache2)),
    ?assertEqual([key1], macula_cache:keys(Cache2)).

put_overwrites_existing_key_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:put(Cache2, key1, value2, 2000),
    ?assertEqual(1, macula_cache:size(Cache3)),
    {ok, Value, _} = macula_cache:get(Cache3, key1),
    ?assertEqual(value2, Value).

put_multiple_entries_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:put(Cache2, key2, value2, 2000),
    Cache4 = macula_cache:put(Cache3, key3, value3, 3000),
    ?assertEqual(3, macula_cache:size(Cache4)),
    ?assertEqual([key3, key2, key1], macula_cache:keys(Cache4)).

put_enforces_max_size_test() ->
    Cache = macula_cache:new(3),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:put(Cache2, key2, value2, 2000),
    Cache4 = macula_cache:put(Cache3, key3, value3, 3000),
    Cache5 = macula_cache:put(Cache4, key4, value4, 4000),
    ?assertEqual(3, macula_cache:size(Cache5)),
    %% key1 (oldest) should be evicted
    ?assertEqual([key4, key3, key2], macula_cache:keys(Cache5)).

put_evicts_oldest_when_full_test() ->
    Cache = macula_cache:new(2),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:put(Cache2, key2, value2, 2000),
    Cache4 = macula_cache:put(Cache3, key3, value3, 3000),
    %% key1 should be evicted
    ?assertEqual(not_found, macula_cache:get(Cache4, key1)).

put_with_timestamp_uses_custom_timestamp_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 123456),
    ?assertEqual(1, macula_cache:size(Cache2)).

put_max_size_1_test() ->
    Cache = macula_cache:new(1),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:put(Cache2, key2, value2, 2000),
    ?assertEqual(1, macula_cache:size(Cache3)),
    ?assertEqual([key2], macula_cache:keys(Cache3)).

%%%===================================================================
%%% get/2 Tests
%%%===================================================================

get_existing_key_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    {ok, Value, _Cache3} = macula_cache:get(Cache2, key1),
    ?assertEqual(value1, Value).

get_non_existing_key_test() ->
    Cache = macula_cache:new(10),
    ?assertEqual(not_found, macula_cache:get(Cache, key1)).

get_moves_entry_to_front_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:put(Cache2, key2, value2, 2000),
    Cache4 = macula_cache:put(Cache3, key3, value3, 3000),
    %% Keys in order: [key3, key2, key1]
    %% Get key1, should move to front
    {ok, _, Cache5} = macula_cache:get(Cache4, key1),
    ?assertEqual([key1, key3, key2], macula_cache:keys(Cache5)).

get_returns_updated_cache_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    {ok, _, Cache3} = macula_cache:get(Cache2, key1),
    ?assert(is_map(Cache3)),
    ?assertEqual(1, macula_cache:size(Cache3)).

get_from_empty_cache_test() ->
    Cache = macula_cache:new(10),
    ?assertEqual(not_found, macula_cache:get(Cache, key1)).

%%%===================================================================
%%% remove/2 Tests
%%%===================================================================

remove_existing_key_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:remove(Cache2, key1),
    ?assertEqual(0, macula_cache:size(Cache3)),
    ?assertEqual(not_found, macula_cache:get(Cache3, key1)).

remove_non_existing_key_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:remove(Cache2, key2),
    ?assertEqual(1, macula_cache:size(Cache3)).

remove_from_multiple_entries_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:put(Cache2, key2, value2, 2000),
    Cache4 = macula_cache:put(Cache3, key3, value3, 3000),
    Cache5 = macula_cache:remove(Cache4, key2),
    ?assertEqual(2, macula_cache:size(Cache5)),
    ?assertEqual([key3, key1], macula_cache:keys(Cache5)).

remove_from_empty_cache_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:remove(Cache, key1),
    ?assertEqual(0, macula_cache:size(Cache2)).

%%%===================================================================
%%% clear/1 Tests
%%%===================================================================

clear_empty_cache_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:clear(Cache),
    ?assertEqual(0, macula_cache:size(Cache2)).

clear_non_empty_cache_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:put(Cache2, key2, value2, 2000),
    Cache4 = macula_cache:clear(Cache3),
    ?assertEqual(0, macula_cache:size(Cache4)),
    ?assertEqual([], macula_cache:keys(Cache4)).

clear_preserves_max_size_test() ->
    Cache = macula_cache:new(42),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:clear(Cache2),
    ?assertEqual(42, macula_cache:max_size(Cache3)).

%%%===================================================================
%%% size/1 Tests
%%%===================================================================

size_empty_cache_test() ->
    Cache = macula_cache:new(10),
    ?assertEqual(0, macula_cache:size(Cache)).

size_after_put_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    ?assertEqual(1, macula_cache:size(Cache2)).

size_after_multiple_puts_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:put(Cache2, key2, value2, 2000),
    Cache4 = macula_cache:put(Cache3, key3, value3, 3000),
    ?assertEqual(3, macula_cache:size(Cache4)).

size_never_exceeds_max_test() ->
    Cache = macula_cache:new(3),
    Cache2 = lists:foldl(
        fun(I, Acc) ->
            macula_cache:put(Acc, {key, I}, {value, I}, I * 1000)
        end,
        Cache,
        lists:seq(1, 10)
    ),
    ?assertEqual(3, macula_cache:size(Cache2)).

%%%===================================================================
%%% max_size/1 Tests
%%%===================================================================

max_size_test() ->
    Cache = macula_cache:new(100),
    ?assertEqual(100, macula_cache:max_size(Cache)).

max_size_after_operations_test() ->
    Cache = macula_cache:new(50),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:remove(Cache2, key1),
    Cache4 = macula_cache:clear(Cache3),
    ?assertEqual(50, macula_cache:max_size(Cache4)).

%%%===================================================================
%%% keys/1 Tests
%%%===================================================================

keys_empty_cache_test() ->
    Cache = macula_cache:new(10),
    ?assertEqual([], macula_cache:keys(Cache)).

keys_single_entry_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    ?assertEqual([key1], macula_cache:keys(Cache2)).

keys_multiple_entries_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:put(Cache2, key2, value2, 2000),
    Cache4 = macula_cache:put(Cache3, key3, value3, 3000),
    ?assertEqual([key3, key2, key1], macula_cache:keys(Cache4)).

keys_order_reflects_lru_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:put(Cache2, key2, value2, 2000),
    %% Get key1, moves it to front
    {ok, _, Cache4} = macula_cache:get(Cache3, key1),
    ?assertEqual([key1, key2], macula_cache:keys(Cache4)).

%%%===================================================================
%%% LRU Behavior Tests
%%%===================================================================

lru_eviction_test() ->
    %% Create cache with max size 3
    Cache = macula_cache:new(3),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:put(Cache2, key2, value2, 2000),
    Cache4 = macula_cache:put(Cache3, key3, value3, 3000),
    %% key1 is oldest, key3 is newest
    %% Add key4, should evict key1
    Cache5 = macula_cache:put(Cache4, key4, value4, 4000),
    ?assertEqual(not_found, macula_cache:get(Cache5, key1)),
    ?assertEqual([key4, key3, key2], macula_cache:keys(Cache5)).

lru_get_refreshes_entry_test() ->
    Cache = macula_cache:new(3),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:put(Cache2, key2, value2, 2000),
    Cache4 = macula_cache:put(Cache3, key3, value3, 3000),
    %% Get key1, refreshes it
    {ok, _, Cache5} = macula_cache:get(Cache4, key1),
    %% Add key4, should evict key2 (now oldest)
    Cache6 = macula_cache:put(Cache5, key4, value4, 4000),
    ?assertEqual(not_found, macula_cache:get(Cache6, key2)),
    ?assertEqual([key4, key1, key3], macula_cache:keys(Cache6)).

lru_put_same_key_refreshes_test() ->
    Cache = macula_cache:new(3),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:put(Cache2, key2, value2, 2000),
    Cache4 = macula_cache:put(Cache3, key3, value3, 3000),
    %% Update key1, refreshes it
    Cache5 = macula_cache:put(Cache4, key1, value_new, 4000),
    %% Add key4, should evict key2 (now oldest)
    Cache6 = macula_cache:put(Cache5, key4, value4, 5000),
    ?assertEqual(not_found, macula_cache:get(Cache6, key2)),
    ?assertEqual([key4, key1, key3], macula_cache:keys(Cache6)).

%%%===================================================================
%%% Edge Cases
%%%===================================================================

put_different_value_types_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, key1, <<"binary">>, 1000),
    Cache3 = macula_cache:put(Cache2, key2, [1, 2, 3], 2000),
    Cache4 = macula_cache:put(Cache3, key3, #{map => value}, 3000),
    Cache5 = macula_cache:put(Cache4, key4, {tuple, value}, 4000),
    ?assertEqual(4, macula_cache:size(Cache5)).

put_different_key_types_test() ->
    Cache = macula_cache:new(10),
    Cache2 = macula_cache:put(Cache, <<"binary_key">>, value1, 1000),
    Cache3 = macula_cache:put(Cache2, 123, value2, 2000),
    Cache4 = macula_cache:put(Cache3, {tuple, key}, value3, 3000),
    ?assertEqual(3, macula_cache:size(Cache4)).

empty_cache_operations_test() ->
    Cache = macula_cache:new(10),
    ?assertEqual(0, macula_cache:size(Cache)),
    ?assertEqual([], macula_cache:keys(Cache)),
    ?assertEqual(not_found, macula_cache:get(Cache, key1)),
    Cache2 = macula_cache:remove(Cache, key1),
    ?assertEqual(0, macula_cache:size(Cache2)),
    Cache3 = macula_cache:clear(Cache),
    ?assertEqual(0, macula_cache:size(Cache3)).

max_size_1_lru_test() ->
    Cache = macula_cache:new(1),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    ?assertEqual([key1], macula_cache:keys(Cache2)),
    Cache3 = macula_cache:put(Cache2, key2, value2, 2000),
    ?assertEqual([key2], macula_cache:keys(Cache3)),
    ?assertEqual(not_found, macula_cache:get(Cache3, key1)).

large_cache_test() ->
    Cache = macula_cache:new(1000),
    Cache2 = lists:foldl(
        fun(I, Acc) ->
            macula_cache:put(Acc, {key, I}, {value, I}, I * 1000)
        end,
        Cache,
        lists:seq(1, 1000)
    ),
    ?assertEqual(1000, macula_cache:size(Cache2)),
    %% Verify last entry is at front
    [{key, 1000} | _] = macula_cache:keys(Cache2).

many_operations_test() ->
    Cache = macula_cache:new(5),
    Cache2 = lists:foldl(
        fun(I, Acc) ->
            Acc1 = macula_cache:put(Acc, {key, I}, {value, I}, I * 1000),
            case I rem 2 of
                0 -> macula_cache:remove(Acc1, {key, I div 2});
                _ -> Acc1
            end
        end,
        Cache,
        lists:seq(1, 20)
    ),
    ?assert(macula_cache:size(Cache2) =< 5).

get_put_interleaved_test() ->
    Cache = macula_cache:new(3),
    Cache2 = macula_cache:put(Cache, key1, value1, 1000),
    Cache3 = macula_cache:put(Cache2, key2, value2, 2000),
    {ok, _, Cache4} = macula_cache:get(Cache3, key1),
    Cache5 = macula_cache:put(Cache4, key3, value3, 3000),
    {ok, _, Cache6} = macula_cache:get(Cache5, key2),
    Cache7 = macula_cache:put(Cache6, key4, value4, 4000),
    %% key1 should be evicted (oldest after gets)
    %% Order after Cache6: [key2, key3, key1]
    %% After put(key4): [key4, key2, key3], key1 evicted
    ?assertEqual(not_found, macula_cache:get(Cache7, key1)).
