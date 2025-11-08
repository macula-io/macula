%%%-------------------------------------------------------------------
%%% @doc
%%% Topic utilities for pub/sub system.
%%% Handles topic validation, pattern matching, and normalization.
%%% Supports MQTT-style wildcards: * (single-level) and # (multi-level).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_topic).

%% API
-export([
    validate/1,
    matches/2,
    normalize/1,
    namespace/1,
    segment_count/1
]).

%% Types
-type topic() :: binary().
-type pattern() :: binary().
-export_type([topic/0, pattern/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Validate topic syntax.
%% Valid topics:
%% - Non-empty
%% - Segments separated by dots
%% - Segments contain alphanumeric, underscore, hyphen
%% - No leading or trailing dots
-spec validate(topic()) -> ok | {error, invalid_topic}.
validate(<<>>) ->
    {error, invalid_topic};
validate(Topic) ->
    %% Check for leading/trailing dots
    case binary:first(Topic) of
        $. -> {error, invalid_topic};
        _ ->
            case binary:last(Topic) of
                $. -> {error, invalid_topic};
                _ -> validate_segments(Topic)
            end
    end.

%% @doc Check if topic matches pattern.
%% Patterns can contain:
%% - * matches exactly one segment
%% - # matches zero or more segments
-spec matches(topic(), pattern()) -> boolean().
matches(Topic, Pattern) ->
    TopicSegments = binary:split(Topic, <<".">>, [global]),
    PatternSegments = binary:split(Pattern, <<".">>, [global]),
    match_segments(TopicSegments, PatternSegments).

%% @doc Normalize topic (lowercase, trim, remove double dots).
-spec normalize(topic()) -> topic().
normalize(Topic) ->
    %% Trim whitespace
    Trimmed = string:trim(Topic),

    %% Lowercase
    Lower = string:lowercase(Trimmed),

    %% Remove double dots
    remove_double_dots(Lower).

%% @doc Extract namespace (first segment).
-spec namespace(topic()) -> binary().
namespace(<<>>) ->
    <<>>;
namespace(Topic) ->
    case binary:split(Topic, <<".">>) of
        [Namespace] -> Namespace;
        [Namespace, _Rest] -> Namespace
    end.

%% @doc Count number of segments in topic.
-spec segment_count(topic()) -> non_neg_integer().
segment_count(<<>>) ->
    0;
segment_count(Topic) ->
    Segments = binary:split(Topic, <<".">>, [global]),
    length(Segments).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Validate all segments contain valid characters.
validate_segments(Topic) ->
    Segments = binary:split(Topic, <<".">>, [global]),
    case lists:all(fun is_valid_segment/1, Segments) of
        true -> ok;
        false -> {error, invalid_topic}
    end.

%% @doc Check if segment contains only valid characters.
is_valid_segment(<<>>) ->
    false;  % Empty segment not allowed
is_valid_segment(Segment) ->
    %% Allow alphanumeric, underscore, hyphen, wildcards
    is_valid_segment_chars(binary_to_list(Segment)).

is_valid_segment_chars([]) ->
    true;
is_valid_segment_chars([C | Rest]) when (C >= $a andalso C =< $z) orelse
                                          (C >= $A andalso C =< $Z) orelse
                                          (C >= $0 andalso C =< $9) orelse
                                          C =:= $_ orelse
                                          C =:= $- orelse
                                          C =:= $* orelse
                                          C =:= $# ->
    is_valid_segment_chars(Rest);
is_valid_segment_chars(_) ->
    false.

%% @doc Match topic segments against pattern segments.
match_segments([], []) ->
    true;  % Both exhausted, match
match_segments(_TopicSegs, [<<"#">>]) ->
    true;  % Multi-level wildcard at end matches rest
match_segments([], _PatternSegs) ->
    false;  % Topic exhausted, pattern has more
match_segments(_TopicSegs, []) ->
    false;  % Pattern exhausted, topic has more
match_segments([_TSeg | TRest], [<<"*">> | PRest]) ->
    %% Single-level wildcard matches one segment
    match_segments(TRest, PRest);
match_segments(TopicSegs, [<<"#">> | PRest]) ->
    %% Multi-level wildcard in middle
    %% Try matching rest of pattern at various positions
    match_multi_level(TopicSegs, PRest);
match_segments([TSeg | TRest], [PSeg | PRest]) when TSeg =:= PSeg ->
    %% Exact match
    match_segments(TRest, PRest);
match_segments(_TopicSegs, _PatternSegs) ->
    false.  % No match

%% @doc Match multi-level wildcard in middle of pattern.
match_multi_level(_TopicSegs, []) ->
    %% # at end matches everything
    true;
match_multi_level([], PatternSegs) ->
    %% Topic exhausted, check if pattern can match zero segments
    case PatternSegs of
        [] -> true;
        _ -> false
    end;
match_multi_level([_TSeg | TRest] = TopicSegs, PatternSegs) ->
    %% Try matching pattern at current position
    case match_segments(TopicSegs, PatternSegs) of
        true -> true;
        false ->
            %% Try next position (consume one topic segment)
            match_multi_level(TRest, PatternSegs)
    end.

%% @doc Remove consecutive dots.
remove_double_dots(Binary) ->
    case binary:replace(Binary, <<"..">>, <<".">>, [global]) of
        Binary -> Binary;  % No change, done
        Modified -> remove_double_dots(Modified)  % Keep removing
    end.
