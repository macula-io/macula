%% EUnit tests for macula_record_uuid: random UUIDv7 ids, and record versions that strictly increase on a node
%% (RFC 9562, section 6.2, method 1).
-module(macula_record_uuid_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TABLE, macula_record_uuid_test).
-define(MS, 1700000000000).

%%------------------------------------------------------------------
%% v7/0: random ids
%%------------------------------------------------------------------

v7_returns_16_bytes_test() ->
    ?assertEqual(16, byte_size(macula_record_uuid:v7())).

v7_uniqueness_test() ->
    ?assertNotEqual(macula_record_uuid:v7(), macula_record_uuid:v7()).

v7_version_field_is_7_test() ->
    <<_:48, V:4, _/bitstring>> = macula_record_uuid:v7(),
    ?assertEqual(7, V).

v7_variant_field_is_10_binary_test() ->
    <<_:64, Var:2, _/bitstring>> = macula_record_uuid:v7(),
    ?assertEqual(2#10, Var).

v7_now_returns_current_ms_test() ->
    Before = erlang:system_time(millisecond),
    <<MsBack:48, _/bitstring>> = macula_record_uuid:v7(),
    After = erlang:system_time(millisecond),
    ?assert(MsBack >= Before),
    ?assert(MsBack =< After).

%%------------------------------------------------------------------
%% v7_monotonic: record versions
%%------------------------------------------------------------------

monotonic_test_() ->
    {foreach, fun() -> ok end, fun(ok) -> stopped(ets:info(?TABLE, owner)) end,
     [{"a first version carries its millisecond, version 7 and variant 10",
       fun a_first_version_carries_its_millisecond_version_and_variant/0},
      {"versions in one millisecond strictly increase",
       fun versions_in_one_millisecond_strictly_increase/0},
      {"a counter that runs out moves to the next millisecond",
       fun a_counter_that_runs_out_moves_to_the_next_millisecond/0},
      {"a clock step back never lowers a version",
       fun a_clock_step_back_never_lowers_a_version/0},
      {"concurrent callers get distinct versions, each in increasing order",
       fun concurrent_callers_get_distinct_increasing_versions/0},
      {"without its table a version is a UUIDv7 at its millisecond",
       fun without_its_table_a_version_is_a_uuidv7_at_its_millisecond/0}]}.

tombstone_test_() ->
    {setup, fun() -> {ok, _} = application:ensure_all_started(macula), ok end, fun(ok) -> ok end,
     [{"a tombstone built in the same millisecond as its record has the higher version",
       fun a_tombstone_has_a_higher_version_than_its_record/0}]}.

%% Each case starts the table's owner itself: eunit runs a foreach setup in another process than the case.
a_first_version_carries_its_millisecond_version_and_variant() ->
    _ = owner(),
    <<Ms:48, Version:4, _Counter:12, Variant:2, _RandB:62>> = macula_record_uuid:v7_monotonic(?TABLE, ?MS),
    ?assertEqual({?MS, 7, 2#10}, {Ms, Version, Variant}).

versions_in_one_millisecond_strictly_increase() ->
    _ = owner(),
    Versions = [macula_record_uuid:v7_monotonic(?TABLE, ?MS) || _ <- lists:seq(1, 10_000)],
    ?assert(strictly_increasing(Versions)).

%% A seed is below 2,048, so 5,000 versions in one millisecond run past the 12-bit counter.
a_counter_that_runs_out_moves_to_the_next_millisecond() ->
    _ = owner(),
    Versions = [macula_record_uuid:v7_monotonic(?TABLE, ?MS) || _ <- lists:seq(1, 5_000)],
    <<LastMs:48, _/bitstring>> = lists:last(Versions),
    ?assert(strictly_increasing(Versions)),
    ?assertEqual(?MS + 1, LastMs).

a_clock_step_back_never_lowers_a_version() ->
    _ = owner(),
    Before = macula_record_uuid:v7_monotonic(?TABLE, ?MS),
    After = macula_record_uuid:v7_monotonic(?TABLE, ?MS - 60_000),
    ?assert(After > Before).

concurrent_callers_get_distinct_increasing_versions() ->
    _ = owner(),
    Test = self(),
    Callers = [spawn_link(fun() -> Test ! {versions, [macula_record_uuid:v7_monotonic(?TABLE, ?MS)
                                                     || _ <- lists:seq(1, 50)]} end)
               || _ <- lists:seq(1, 200)],
    Lists = [receive {versions, Versions} -> Versions after 5_000 -> erlang:error(caller_stuck) end || _ <- Callers],
    ?assert(lists:all(fun strictly_increasing/1, Lists)),
    ?assertEqual(10_000, length(lists:usort(lists:append(Lists)))).

without_its_table_a_version_is_a_uuidv7_at_its_millisecond() ->
    <<Ms:48, Version:4, _Counter:12, Variant:2, _RandB:62>> =
        macula_record_uuid:v7_monotonic(macula_record_uuid_no_table, ?MS),
    ?assertEqual({?MS, 7, 2#10}, {Ms, Version, Variant}).

%% Mars's case: a record and its tombstone built one after the other, nearly always in the same millisecond.
a_tombstone_has_a_higher_version_than_its_record() ->
    Pairs = [begin
                 Record = macula_record:station_endpoint(4433),
                 {macula_record:version(Record), macula_record:version(macula_record:tombstone(Record, shutdown))}
             end || _ <- lists:seq(1, 1_000)],
    ?assertEqual([], [Pair || {RecordVersion, TombstoneVersion} = Pair <- Pairs, TombstoneVersion =< RecordVersion]).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

owner() ->
    {ok, Owner} = macula_record_uuid:start_link(#{table => ?TABLE}),
    Owner.

stopped(undefined) -> ok;
stopped(Owner) -> gen_server:stop(Owner).

strictly_increasing([A, B | Rest]) when A < B -> strictly_increasing([B | Rest]);
strictly_increasing([_, _ | _]) -> false;
strictly_increasing(_Shorter) -> true.
