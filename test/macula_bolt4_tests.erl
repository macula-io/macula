%% EUnit tests for hecate_bolt4.
-module(macula_bolt4_tests).

-include_lib("eunit/include/eunit.hrl").

%%---------------------------------------------------------------------
%% Table shape
%%---------------------------------------------------------------------

table_has_sixteen_entries_test() ->
    ?assertEqual(16, length(macula_bolt4:table())).

every_entry_has_distinct_code_test() ->
    Codes = [maps:get(code, E) || E <- macula_bolt4:table()],
    ?assertEqual(length(Codes), length(lists:usort(Codes))).

every_entry_has_distinct_name_test() ->
    Names = [maps:get(name, E) || E <- macula_bolt4:table()],
    ?assertEqual(length(Names), length(lists:usort(Names))).

codes_are_dense_zero_through_15_test() ->
    Codes = lists:sort([maps:get(code, E) || E <- macula_bolt4:table()]),
    ?assertEqual(lists:seq(16#00, 16#0F), Codes).

%%---------------------------------------------------------------------
%% code/1 ↔ name/1 round-trip
%%---------------------------------------------------------------------

code_name_round_trip_for_every_entry_test() ->
    [?assertEqual(maps:get(code, E),
                  macula_bolt4:code(maps:get(name, E)))
     || E <- macula_bolt4:table()],
    [?assertEqual(maps:get(name, E),
                  macula_bolt4:name(maps:get(code, E)))
     || E <- macula_bolt4:table()].

unknown_name_raises_test() ->
    ?assertError({bolt4_unknown, _, _},
                 macula_bolt4:code(definitely_not_a_real_name)).

unknown_code_raises_test() ->
    ?assertError({bolt4_unknown, _, _},
                 macula_bolt4:name(99)).

%%---------------------------------------------------------------------
%% info/1
%%---------------------------------------------------------------------

info_by_name_returns_full_entry_test() ->
    Info = macula_bolt4:info(unknown_next_peer),
    ?assertMatch(#{code := 16#01,
                   name := unknown_next_peer,
                   retry := different_path}, Info).

info_by_code_returns_full_entry_test() ->
    Info = macula_bolt4:info(16#02),
    ?assertEqual(temporary_relay_failure, maps:get(name, Info)),
    ?assertEqual(same_path_after_backoff, maps:get(retry, Info)).

%%---------------------------------------------------------------------
%% is_retryable/1
%%---------------------------------------------------------------------

ok_is_not_retryable_test() ->
    ?assertNot(macula_bolt4:is_retryable(ok)).

application_failures_not_retryable_test() ->
    ?assertNot(macula_bolt4:is_retryable(target_realm_refused)),
    ?assertNot(macula_bolt4:is_retryable(tombstoned)),
    ?assertNot(macula_bolt4:is_retryable(payload_too_large)).

crypto_failures_not_retryable_test() ->
    ?assertNot(macula_bolt4:is_retryable(crypto_puzzle_invalid)),
    ?assertNot(macula_bolt4:is_retryable(signature_invalid)).

transient_failures_are_retryable_test() ->
    ?assert(macula_bolt4:is_retryable(unknown_next_peer)),
    ?assert(macula_bolt4:is_retryable(temporary_relay_failure)),
    ?assert(macula_bolt4:is_retryable(upstream_congestion)),
    ?assert(macula_bolt4:is_retryable(unknown_error)).
