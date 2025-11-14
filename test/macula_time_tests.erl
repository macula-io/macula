%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_time module.
%%% Tests time utilities with actual code execution.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_time_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% timestamp/0 Tests
%%%===================================================================

timestamp_returns_integer_test() ->
    Timestamp = macula_time:timestamp(),
    ?assert(is_integer(Timestamp)).

timestamp_is_positive_test() ->
    Timestamp = macula_time:timestamp(),
    ?assert(Timestamp > 0).

timestamp_is_reasonable_test() ->
    %% Should be a recent timestamp (after 2020-01-01)
    Timestamp = macula_time:timestamp(),
    MinTimestamp = 1577836800000,  % 2020-01-01 00:00:00 UTC in milliseconds
    ?assert(Timestamp > MinTimestamp).

timestamp_increases_test() ->
    %% Timestamps should increase over time
    T1 = macula_time:timestamp(),
    timer:sleep(10),  % Sleep 10ms
    T2 = macula_time:timestamp(),
    ?assert(T2 >= T1).

%%%===================================================================
%%% timestamp_microseconds/0 Tests
%%%===================================================================

timestamp_microseconds_returns_integer_test() ->
    Timestamp = macula_time:timestamp_microseconds(),
    ?assert(is_integer(Timestamp)).

timestamp_microseconds_is_positive_test() ->
    Timestamp = macula_time:timestamp_microseconds(),
    ?assert(Timestamp > 0).

timestamp_microseconds_is_larger_than_milliseconds_test() ->
    %% Microseconds should be ~1000x larger than milliseconds
    Ms = macula_time:timestamp(),
    Us = macula_time:timestamp_microseconds(),
    ?assert(Us > Ms * 900),  % Allow some timing variation
    ?assert(Us < Ms * 1100).

timestamp_microseconds_increases_test() ->
    T1 = macula_time:timestamp_microseconds(),
    timer:sleep(1),
    T2 = macula_time:timestamp_microseconds(),
    ?assert(T2 >= T1).

%%%===================================================================
%%% duration_ms/2 Tests
%%%===================================================================

duration_ms_positive_difference_test() ->
    Start = 1000,
    End = 2000,
    Duration = macula_time:duration_ms(Start, End),
    ?assertEqual(1000, Duration).

duration_ms_zero_difference_test() ->
    Start = 1000,
    End = 1000,
    Duration = macula_time:duration_ms(Start, End),
    ?assertEqual(0, Duration).

duration_ms_negative_difference_returns_zero_test() ->
    Start = 2000,
    End = 1000,
    Duration = macula_time:duration_ms(Start, End),
    ?assertEqual(0, Duration).

duration_ms_large_duration_test() ->
    Start = 0,
    End = 86400000,  % 1 day in milliseconds
    Duration = macula_time:duration_ms(Start, End),
    ?assertEqual(86400000, Duration).

duration_ms_actual_timestamps_test() ->
    Start = macula_time:timestamp(),
    timer:sleep(100),
    End = macula_time:timestamp(),
    Duration = macula_time:duration_ms(Start, End),
    ?assert(Duration >= 100),
    ?assert(Duration < 200).  % Allow some timing variation

%%%===================================================================
%%% is_expired/2 Tests
%%%===================================================================

is_expired_not_expired_test() ->
    %% Just started, should not be expired
    Start = macula_time:timestamp(),
    Timeout = 10000,  % 10 seconds
    ?assertNot(macula_time:is_expired(Start, Timeout)).

is_expired_after_timeout_test() ->
    %% Past timestamp should be expired
    Start = macula_time:timestamp() - 5000,  % 5 seconds ago
    Timeout = 1000,  % 1 second timeout
    ?assert(macula_time:is_expired(Start, Timeout)).

is_expired_exactly_at_timeout_test() ->
    %% At exactly timeout duration, should be expired
    Now = macula_time:timestamp(),
    Start = Now - 1000,
    Timeout = 1000,
    ?assert(macula_time:is_expired(Start, Timeout)).

is_expired_just_before_timeout_test() ->
    %% Just before timeout should not be expired
    Now = macula_time:timestamp(),
    Start = Now - 900,
    Timeout = 1000,
    ?assertNot(macula_time:is_expired(Start, Timeout)).

%%%===================================================================
%%% time_remaining/2 Tests
%%%===================================================================

time_remaining_full_timeout_test() ->
    Start = macula_time:timestamp(),
    Timeout = 10000,
    Remaining = macula_time:time_remaining(Start, Timeout),
    ?assert(Remaining > 9900),  % Should be close to 10000
    ?assert(Remaining =< 10000).

time_remaining_partial_timeout_test() ->
    Start = macula_time:timestamp() - 3000,  % 3 seconds ago
    Timeout = 5000,  % 5 second timeout
    Remaining = macula_time:time_remaining(Start, Timeout),
    ?assert(Remaining >= 1900),  % Should be close to 2000
    ?assert(Remaining =< 2100).

time_remaining_expired_returns_zero_test() ->
    Start = macula_time:timestamp() - 5000,  % 5 seconds ago
    Timeout = 1000,  % 1 second timeout
    Remaining = macula_time:time_remaining(Start, Timeout),
    ?assertEqual(0, Remaining).

time_remaining_exactly_expired_test() ->
    Now = macula_time:timestamp(),
    Start = Now - 1000,
    Timeout = 1000,
    Remaining = macula_time:time_remaining(Start, Timeout),
    ?assertEqual(0, Remaining).

%%%===================================================================
%%% format_duration_ms/1 Tests
%%%===================================================================

format_duration_ms_zero_test() ->
    Formatted = macula_time:format_duration_ms(0),
    ?assertEqual(<<"0ms">>, Formatted).

format_duration_ms_milliseconds_test() ->
    Formatted = macula_time:format_duration_ms(500),
    ?assertEqual(<<"500ms">>, Formatted).

format_duration_ms_999ms_test() ->
    Formatted = macula_time:format_duration_ms(999),
    ?assertEqual(<<"999ms">>, Formatted).

format_duration_ms_one_second_test() ->
    Formatted = macula_time:format_duration_ms(1000),
    ?assertEqual(<<"1s">>, Formatted).

format_duration_ms_multiple_seconds_test() ->
    Formatted = macula_time:format_duration_ms(5000),
    ?assertEqual(<<"5s">>, Formatted).

format_duration_ms_fractional_seconds_test() ->
    Formatted = macula_time:format_duration_ms(1500),
    ?assertEqual(<<"1.5s">>, Formatted).

format_duration_ms_fractional_with_decimal_test() ->
    Formatted = macula_time:format_duration_ms(2750),
    ?assertEqual(<<"2.8s">>, Formatted).  % Rounded to 1 decimal

format_duration_ms_large_duration_test() ->
    Formatted = macula_time:format_duration_ms(86400000),  % 1 day
    ?assertEqual(<<"86400s">>, Formatted).

%%%===================================================================
%%% format_timestamp/1 Tests
%%%===================================================================

format_timestamp_epoch_test() ->
    %% Unix epoch: 1970-01-01 00:00:00 UTC
    Formatted = macula_time:format_timestamp(0),
    ?assertEqual(<<"1970-01-01T00:00:00.000Z">>, Formatted).

format_timestamp_known_timestamp_test() ->
    %% 2020-01-01 00:00:00 UTC = 1577836800000 milliseconds
    Formatted = macula_time:format_timestamp(1577836800000),
    ?assertEqual(<<"2020-01-01T00:00:00.000Z">>, Formatted).

format_timestamp_with_milliseconds_test() ->
    %% 2020-01-01 00:00:00.123 UTC
    Formatted = macula_time:format_timestamp(1577836800123),
    ?assertEqual(<<"2020-01-01T00:00:00.123Z">>, Formatted).

format_timestamp_iso8601_format_test() ->
    %% Verify ISO 8601 format: YYYY-MM-DDTHH:MM:SS.sssZ
    Formatted = macula_time:format_timestamp(1577836800000),
    ?assert(is_binary(Formatted)),
    ?assertEqual(24, byte_size(Formatted)),  % Fixed length
    ?assertMatch(<<_:4/binary, $-, _:2/binary, $-, _:2/binary, $T, _/binary>>, Formatted).

format_timestamp_with_date_components_test() ->
    %% 2021-06-15 14:10:45.678 UTC = 1623766245678 milliseconds
    Formatted = macula_time:format_timestamp(1623766245678),
    ?assertEqual(<<"2021-06-15T14:10:45.678Z">>, Formatted).

format_timestamp_current_time_test() ->
    Now = macula_time:timestamp(),
    Formatted = macula_time:format_timestamp(Now),
    ?assert(is_binary(Formatted)),
    ?assertEqual(24, byte_size(Formatted)),
    %% Should end with Z
    ?assertMatch(<<_:23/binary, $Z>>, Formatted).

%%%===================================================================
%%% seconds_to_ms/1 Tests
%%%===================================================================

seconds_to_ms_zero_test() ->
    ?assertEqual(0, macula_time:seconds_to_ms(0)).

seconds_to_ms_one_second_test() ->
    ?assertEqual(1000, macula_time:seconds_to_ms(1)).

seconds_to_ms_multiple_seconds_test() ->
    ?assertEqual(5000, macula_time:seconds_to_ms(5)).

seconds_to_ms_one_minute_test() ->
    ?assertEqual(60000, macula_time:seconds_to_ms(60)).

seconds_to_ms_one_hour_test() ->
    ?assertEqual(3600000, macula_time:seconds_to_ms(3600)).

seconds_to_ms_large_value_test() ->
    ?assertEqual(86400000, macula_time:seconds_to_ms(86400)).  % 1 day

%%%===================================================================
%%% ms_to_seconds/1 Tests
%%%===================================================================

ms_to_seconds_zero_test() ->
    ?assertEqual(0, macula_time:ms_to_seconds(0)).

ms_to_seconds_one_second_test() ->
    ?assertEqual(1, macula_time:ms_to_seconds(1000)).

ms_to_seconds_multiple_seconds_test() ->
    ?assertEqual(5, macula_time:ms_to_seconds(5000)).

ms_to_seconds_truncates_test() ->
    %% 1500ms should truncate to 1 second
    ?assertEqual(1, macula_time:ms_to_seconds(1500)).

ms_to_seconds_999ms_test() ->
    %% Less than 1 second should be 0
    ?assertEqual(0, macula_time:ms_to_seconds(999)).

ms_to_seconds_one_minute_test() ->
    ?assertEqual(60, macula_time:ms_to_seconds(60000)).

ms_to_seconds_large_value_test() ->
    ?assertEqual(86400, macula_time:ms_to_seconds(86400000)).  % 1 day

%%%===================================================================
%%% minutes_to_ms/1 Tests
%%%===================================================================

minutes_to_ms_zero_test() ->
    ?assertEqual(0, macula_time:minutes_to_ms(0)).

minutes_to_ms_one_minute_test() ->
    ?assertEqual(60000, macula_time:minutes_to_ms(1)).

minutes_to_ms_multiple_minutes_test() ->
    ?assertEqual(300000, macula_time:minutes_to_ms(5)).

minutes_to_ms_one_hour_test() ->
    ?assertEqual(3600000, macula_time:minutes_to_ms(60)).

minutes_to_ms_large_value_test() ->
    ?assertEqual(86400000, macula_time:minutes_to_ms(1440)).  % 1 day

%%%===================================================================
%%% Conversion Roundtrip Tests
%%%===================================================================

seconds_ms_roundtrip_test() ->
    Seconds = 42,
    Ms = macula_time:seconds_to_ms(Seconds),
    ConvertedSeconds = macula_time:ms_to_seconds(Ms),
    ?assertEqual(Seconds, ConvertedSeconds).

minutes_seconds_conversion_test() ->
    Minutes = 5,
    Ms = macula_time:minutes_to_ms(Minutes),
    Seconds = macula_time:ms_to_seconds(Ms),
    ?assertEqual(Minutes * 60, Seconds).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

timeout_workflow_test() ->
    %% Simulate a timeout workflow
    Start = macula_time:timestamp(),
    Timeout = 100,  % 100ms timeout

    %% Should not be expired immediately
    ?assertNot(macula_time:is_expired(Start, Timeout)),

    %% Should have time remaining
    Remaining1 = macula_time:time_remaining(Start, Timeout),
    ?assert(Remaining1 > 0),
    ?assert(Remaining1 =< Timeout),

    %% Wait for timeout
    timer:sleep(110),

    %% Should be expired
    ?assert(macula_time:is_expired(Start, Timeout)),

    %% Should have no time remaining
    Remaining2 = macula_time:time_remaining(Start, Timeout),
    ?assertEqual(0, Remaining2),

    %% Duration should be >= timeout
    End = macula_time:timestamp(),
    Duration = macula_time:duration_ms(Start, End),
    ?assert(Duration >= Timeout).

format_current_time_test() ->
    %% Format current timestamp and verify it's valid ISO 8601
    Now = macula_time:timestamp(),
    Formatted = macula_time:format_timestamp(Now),

    %% Should be 24 characters (YYYY-MM-DDTHH:MM:SS.sssZ)
    ?assertEqual(24, byte_size(Formatted)),

    %% Should end with Z
    ?assertMatch(<<_:23/binary, $Z>>, Formatted),

    %% Should contain T separator
    ?assert(binary:match(Formatted, <<"T">>) =/= nomatch).

duration_formatting_test() ->
    %% Test duration measurement and formatting together
    Start = macula_time:timestamp(),
    timer:sleep(50),
    End = macula_time:timestamp(),

    Duration = macula_time:duration_ms(Start, End),
    Formatted = macula_time:format_duration_ms(Duration),

    ?assert(is_binary(Formatted)),
    ?assert(Duration >= 50),
    ?assert(Duration < 100).

conversion_chain_test() ->
    %% Test conversion chain: minutes -> ms -> seconds
    Minutes = 10,

    Ms = macula_time:minutes_to_ms(Minutes),
    ?assertEqual(600000, Ms),

    Seconds = macula_time:ms_to_seconds(Ms),
    ?assertEqual(600, Seconds),

    MsAgain = macula_time:seconds_to_ms(Seconds),
    ?assertEqual(Ms, MsAgain).
