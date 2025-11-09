%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_time module.
%%% Tests written FIRST (TDD red phase).
%%% Time utilities.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_time_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Timestamp Tests
%%%===================================================================

%% Test: timestamp returns current millisecond timestamp
timestamp_test() ->
    Timestamp1 = macula_time:timestamp(),
    timer:sleep(5),
    Timestamp2 = macula_time:timestamp(),

    ?assert(is_integer(Timestamp1)),
    ?assert(Timestamp2 > Timestamp1).

%% Test: timestamp_microseconds returns microsecond timestamp
timestamp_microseconds_test() ->
    Timestamp = macula_time:timestamp_microseconds(),

    ?assert(is_integer(Timestamp)),
    ?assert(Timestamp > 0).

%%%===================================================================
%%% Duration Tests
%%%===================================================================

%% Test: duration_ms calculates milliseconds between timestamps
duration_ms_test() ->
    Start = macula_time:timestamp(),
    timer:sleep(10),
    End = macula_time:timestamp(),

    Duration = macula_time:duration_ms(Start, End),

    ?assert(Duration >= 10),
    ?assert(Duration < 100).  % Should be around 10ms

%% Test: duration_ms with same timestamp returns 0
duration_ms_zero_test() ->
    Timestamp = macula_time:timestamp(),

    Duration = macula_time:duration_ms(Timestamp, Timestamp),

    ?assertEqual(0, Duration).

%%%===================================================================
%%% Timeout Tests
%%%===================================================================

%% Test: is_expired checks if timeout has expired
is_expired_test() ->
    Start = macula_time:timestamp(),
    Timeout = 100,  % 100ms

    %% Not expired immediately
    ?assertNot(macula_time:is_expired(Start, Timeout)),

    %% Wait and check again
    timer:sleep(150),
    ?assert(macula_time:is_expired(Start, Timeout)).

%% Test: time_remaining calculates remaining time
time_remaining_test() ->
    Start = macula_time:timestamp(),
    Timeout = 1000,  % 1 second

    Remaining = macula_time:time_remaining(Start, Timeout),

    ?assert(Remaining > 900),  % Should be close to 1000
    ?assert(Remaining =< 1000).

%% Test: time_remaining returns 0 when expired
time_remaining_expired_test() ->
    Start = macula_time:timestamp() - 2000,  % 2 seconds ago
    Timeout = 1000,  % 1 second

    Remaining = macula_time:time_remaining(Start, Timeout),

    ?assertEqual(0, Remaining).

%%%===================================================================
%%% Formatting Tests
%%%===================================================================

%% Test: format_duration_ms formats milliseconds
format_duration_ms_test() ->
    ?assertEqual(<<"100ms">>, macula_time:format_duration_ms(100)),
    ?assertEqual(<<"1s">>, macula_time:format_duration_ms(1000)),
    ?assertEqual(<<"1.5s">>, macula_time:format_duration_ms(1500)),
    ?assertEqual(<<"60s">>, macula_time:format_duration_ms(60000)).

%% Test: format_timestamp formats ISO 8601
format_timestamp_test() ->
    %% Use a known timestamp
    Timestamp = 1609459200000,  % 2021-01-01T00:00:00Z

    Formatted = macula_time:format_timestamp(Timestamp),

    ?assert(is_binary(Formatted)),
    %% Should start with date
    ?assertEqual(<<$2, $0, $2, $1>>, binary:part(Formatted, 0, 4)).

%%%===================================================================
%%% Conversion Tests
%%%===================================================================

%% Test: seconds_to_ms converts seconds to milliseconds
seconds_to_ms_test() ->
    ?assertEqual(1000, macula_time:seconds_to_ms(1)),
    ?assertEqual(5000, macula_time:seconds_to_ms(5)),
    ?assertEqual(0, macula_time:seconds_to_ms(0)).

%% Test: ms_to_seconds converts milliseconds to seconds
ms_to_seconds_test() ->
    ?assertEqual(1, macula_time:ms_to_seconds(1000)),
    ?assertEqual(5, macula_time:ms_to_seconds(5000)),
    ?assertEqual(0, macula_time:ms_to_seconds(500)).  % Truncates

%% Test: minutes_to_ms converts minutes to milliseconds
minutes_to_ms_test() ->
    ?assertEqual(60000, macula_time:minutes_to_ms(1)),
    ?assertEqual(300000, macula_time:minutes_to_ms(5)).
