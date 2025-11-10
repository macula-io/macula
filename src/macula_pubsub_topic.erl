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
%% - Segments contain alphanumeric, underscore, hyphen, wildcards
%% - No leading or trailing dots
-spec validate(topic()) -> ok | {error, invalid_topic}.
validate(Topic) ->
    case macula_names:validate(Topic, #{allow_wildcards => true}) of
        ok -> ok;
        {error, invalid_name} -> {error, invalid_topic}
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
    macula_names:normalize(Topic).

%% @doc Extract namespace (first segment).
-spec namespace(topic()) -> binary().
namespace(Topic) ->
    macula_names:namespace(Topic).

%% @doc Count number of segments in topic.
-spec segment_count(topic()) -> non_neg_integer().
segment_count(Topic) ->
    macula_names:segment_count(Topic).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

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
