%% EUnit tests for macula_record_uuid.
-module(macula_record_uuid_tests).

-include_lib("eunit/include/eunit.hrl").

v7_returns_16_bytes_test() ->
    ?assertEqual(16, byte_size(macula_record_uuid:v7())).

v7_uniqueness_test() ->
    ?assertNotEqual(macula_record_uuid:v7(), macula_record_uuid:v7()).

v7_encodes_timestamp_test() ->
    Ms = 1700000000000,
    <<MsBack:48, _/bitstring>> = macula_record_uuid:v7(Ms),
    ?assertEqual(Ms, MsBack).

v7_version_field_is_7_test() ->
    <<_:48, V:4, _/bitstring>> = macula_record_uuid:v7(),
    ?assertEqual(7, V).

v7_variant_field_is_10_binary_test() ->
    <<_:64, Var:2, _/bitstring>> = macula_record_uuid:v7(),
    ?assertEqual(2#10, Var).

v7_random_portion_differs_within_same_ms_test() ->
    Ms = 1700000000000,
    Uuids = [macula_record_uuid:v7(Ms) || _ <- lists:seq(1, 5)],
    %% All five distinct.
    ?assertEqual(5, length(lists:usort(Uuids))).

v7_now_returns_current_ms_test() ->
    Before = erlang:system_time(millisecond),
    <<MsBack:48, _/bitstring>> = macula_record_uuid:v7(),
    After = erlang:system_time(millisecond),
    ?assert(MsBack >= Before),
    ?assert(MsBack =< After).
