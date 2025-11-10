%%%-------------------------------------------------------------------
%%% @doc
%%% Time utilities for Macula.
%%% Provides functions for timestamps, durations, and timeouts.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_time).

%% API
-export([
    timestamp/0,
    timestamp_microseconds/0,
    duration_ms/2,
    is_expired/2,
    time_remaining/2,
    format_duration_ms/1,
    format_timestamp/1,
    seconds_to_ms/1,
    ms_to_seconds/1,
    minutes_to_ms/1
]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Get current timestamp in milliseconds since epoch.
-spec timestamp() -> integer().
timestamp() ->
    erlang:system_time(millisecond).

%% @doc Get current timestamp in microseconds since epoch.
-spec timestamp_microseconds() -> integer().
timestamp_microseconds() ->
    erlang:system_time(microsecond).

%% @doc Calculate duration in milliseconds between two timestamps.
-spec duration_ms(integer(), integer()) -> non_neg_integer().
duration_ms(StartTimestamp, EndTimestamp) ->
    max(0, EndTimestamp - StartTimestamp).

%% @doc Check if timeout has expired.
-spec is_expired(integer(), pos_integer()) -> boolean().
is_expired(StartTimestamp, TimeoutMs) ->
    Now = timestamp(),
    (Now - StartTimestamp) >= TimeoutMs.

%% @doc Calculate remaining time before timeout (in milliseconds).
%% Returns 0 if already expired.
-spec time_remaining(integer(), pos_integer()) -> non_neg_integer().
time_remaining(StartTimestamp, TimeoutMs) ->
    Now = timestamp(),
    Elapsed = Now - StartTimestamp,
    max(0, TimeoutMs - Elapsed).

%% @doc Format duration in milliseconds to human-readable string.
-spec format_duration_ms(non_neg_integer()) -> binary().
format_duration_ms(Ms) when Ms < 1000 ->
    list_to_binary(io_lib:format("~Bms", [Ms]));
format_duration_ms(Ms) ->
    Seconds = Ms / 1000,
    Truncated = trunc(Seconds),
    %% Show as seconds if >= 1 second
    %% Use integer format if no fractional part
    case abs(Seconds - Truncated) < 0.01 of
        true ->
            list_to_binary(io_lib:format("~Bs", [Truncated]));
        false ->
            list_to_binary(io_lib:format("~.1fs", [Seconds]))
    end.

%% @doc Format timestamp to ISO 8601 string.
-spec format_timestamp(integer()) -> binary().
format_timestamp(TimestampMs) ->
    %% Convert milliseconds to seconds and microseconds
    Seconds = TimestampMs div 1000,
    Microseconds = (TimestampMs rem 1000) * 1000,

    %% Convert to datetime
    DateTime = calendar:system_time_to_universal_time(Seconds, second),
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,

    %% Format as ISO 8601
    list_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
                      [Year, Month, Day, Hour, Minute, Second, Microseconds div 1000])
    ).

%% @doc Convert seconds to milliseconds.
-spec seconds_to_ms(non_neg_integer()) -> non_neg_integer().
seconds_to_ms(Seconds) ->
    Seconds * 1000.

%% @doc Convert milliseconds to seconds (truncates).
-spec ms_to_seconds(non_neg_integer()) -> non_neg_integer().
ms_to_seconds(Ms) ->
    Ms div 1000.

%% @doc Convert minutes to milliseconds.
-spec minutes_to_ms(non_neg_integer()) -> non_neg_integer().
minutes_to_ms(Minutes) ->
    Minutes * 60 * 1000.
