%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_topic module.
%%% Tests topic pattern matching with MQTT-style wildcards.
%%% Wildcards: * (single-level), # (multi-level)
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_topic_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% validate/1 Tests
%%%===================================================================

validate_simple_topic_test() ->
    ?assertEqual(ok, macula_pubsub_topic:validate(<<"example.topic">>)).

validate_with_wildcards_test() ->
    ?assertEqual(ok, macula_pubsub_topic:validate(<<"example.*.topic">>)),
    ?assertEqual(ok, macula_pubsub_topic:validate(<<"example.#">>)).

validate_empty_topic_test() ->
    ?assertEqual({error, invalid_topic}, macula_pubsub_topic:validate(<<>>)).

validate_invalid_characters_test() ->
    ?assertEqual({error, invalid_topic}, macula_pubsub_topic:validate(<<"example topic">>)).

%%%===================================================================
%%% matches/2 - Exact Matching Tests
%%%===================================================================

matches_exact_single_segment_test() ->
    ?assert(macula_pubsub_topic:matches(<<"test">>, <<"test">>)).

matches_exact_multiple_segments_test() ->
    ?assert(macula_pubsub_topic:matches(<<"example.topic.message">>, <<"example.topic.message">>)).

matches_case_sensitive_test() ->
    ?assertNot(macula_pubsub_topic:matches(<<"Example.Topic">>, <<"example.topic">>)).

matches_different_topics_test() ->
    ?assertNot(macula_pubsub_topic:matches(<<"example.topic">>, <<"other.topic">>)).

matches_different_segment_count_test() ->
    ?assertNot(macula_pubsub_topic:matches(<<"example.topic">>, <<"example.topic.more">>)).

%%%===================================================================
%%% matches/2 - Single-Level Wildcard (*) Tests
%%%===================================================================

matches_star_single_segment_test() ->
    ?assert(macula_pubsub_topic:matches(<<"test">>, <<"*">>)).

matches_star_first_segment_test() ->
    ?assert(macula_pubsub_topic:matches(<<"example.topic">>, <<"*.topic">>)).

matches_star_middle_segment_test() ->
    ?assert(macula_pubsub_topic:matches(<<"example.test.message">>, <<"example.*.message">>)).

matches_star_last_segment_test() ->
    ?assert(macula_pubsub_topic:matches(<<"example.topic">>, <<"example.*">>)).

matches_multiple_stars_test() ->
    ?assert(macula_pubsub_topic:matches(<<"one.two.three">>, <<"*.*.three">>)),
    ?assert(macula_pubsub_topic:matches(<<"one.two.three">>, <<"*.two.*">>)),
    ?assert(macula_pubsub_topic:matches(<<"one.two.three">>, <<"*.*.*">>)).

matches_star_requires_segment_test() ->
    %% * must match exactly one segment, not zero
    ?assertNot(macula_pubsub_topic:matches(<<"example">>, <<"example.*">>)),
    ?assertNot(macula_pubsub_topic:matches(<<"example.topic">>, <<"example.*.topic.*">>)).

matches_star_does_not_match_multiple_segments_test() ->
    ?assertNot(macula_pubsub_topic:matches(<<"example.test.message">>, <<"*">>)),
    ?assertNot(macula_pubsub_topic:matches(<<"one.two.three.four">>, <<"*.three">>)).

%%%===================================================================
%%% matches/2 - Multi-Level Wildcard (#) Tests
%%%===================================================================

matches_hash_at_end_zero_segments_test() ->
    %% # at end matches zero or more segments
    ?assert(macula_pubsub_topic:matches(<<"example">>, <<"example.#">>)).

matches_hash_at_end_one_segment_test() ->
    ?assert(macula_pubsub_topic:matches(<<"example.topic">>, <<"example.#">>)).

matches_hash_at_end_multiple_segments_test() ->
    ?assert(macula_pubsub_topic:matches(<<"example.topic.message.data">>, <<"example.#">>)).

matches_hash_alone_test() ->
    ?assert(macula_pubsub_topic:matches(<<"test">>, <<"#">>)),
    ?assert(macula_pubsub_topic:matches(<<"one.two.three">>, <<"#">>)).

matches_hash_in_middle_zero_segments_test() ->
    %% # can match zero segments in middle
    ?assert(macula_pubsub_topic:matches(<<"example.message">>, <<"example.#.message">>)).

matches_hash_in_middle_one_segment_test() ->
    ?assert(macula_pubsub_topic:matches(<<"example.test.message">>, <<"example.#.message">>)).

matches_hash_in_middle_multiple_segments_test() ->
    ?assert(macula_pubsub_topic:matches(<<"example.one.two.three.message">>, <<"example.#.message">>)).

matches_hash_does_not_match_wrong_suffix_test() ->
    ?assertNot(macula_pubsub_topic:matches(<<"example.topic">>, <<"example.#.other">>)),
    ?assertNot(macula_pubsub_topic:matches(<<"example.one.two">>, <<"example.#.three">>)).

%%%===================================================================
%%% matches/2 - Combined Wildcards Tests
%%%===================================================================

matches_star_and_hash_test() ->
    ?assert(macula_pubsub_topic:matches(<<"example.test.data">>, <<"*.test.#">>)),
    ?assert(macula_pubsub_topic:matches(<<"one.two.three.four">>, <<"*.#">>)).

matches_hash_and_star_test() ->
    ?assert(macula_pubsub_topic:matches(<<"one.two.three.end">>, <<"#.*">>)),
    ?assert(macula_pubsub_topic:matches(<<"example.test.message">>, <<"example.#.*">>)).

matches_complex_pattern_test() ->
    ?assert(macula_pubsub_topic:matches(<<"iot.sensor.temp.living_room">>, <<"iot.*.temp.*">>)),
    ?assert(macula_pubsub_topic:matches(<<"iot.sensor.temp.kitchen.alert">>, <<"iot.#.alert">>)),
    ?assert(macula_pubsub_topic:matches(<<"app.user.login.success">>, <<"app.*.#">>)).

%%%===================================================================
%%% matches/2 - Edge Cases
%%%===================================================================

matches_empty_topic_and_pattern_test() ->
    %% Empty topic and pattern should match (both have zero segments)
    %% Actually, splitting "" gives [""], so both have one empty segment
    ?assert(macula_pubsub_topic:matches(<<>>, <<>>)).

matches_single_dot_test() ->
    %% Single dot creates two empty segments: ["", ""]
    ?assert(macula_pubsub_topic:matches(<<".">>, <<".">>)),
    ?assert(macula_pubsub_topic:matches(<<".">>, <<"*.*">>)).

matches_trailing_dots_test() ->
    ?assert(macula_pubsub_topic:matches(<<"example.">>, <<"example.">>)),
    ?assert(macula_pubsub_topic:matches(<<"example.">>, <<"example.*">>)).

matches_leading_dots_test() ->
    ?assert(macula_pubsub_topic:matches(<<".example">>, <<".example">>)),
    ?assert(macula_pubsub_topic:matches(<<".example">>, <<"*.example">>)).

matches_pattern_longer_than_topic_test() ->
    ?assertNot(macula_pubsub_topic:matches(<<"short">>, <<"much.longer.pattern">>)),
    ?assertNot(macula_pubsub_topic:matches(<<"one.two">>, <<"one.two.three">>)).

matches_topic_longer_than_pattern_test() ->
    %% Without # at end, topic can't be longer
    ?assertNot(macula_pubsub_topic:matches(<<"one.two.three">>, <<"one.two">>)),
    %% With # at end, it matches
    ?assert(macula_pubsub_topic:matches(<<"one.two.three">>, <<"one.two.#">>)).

%%%===================================================================
%%% normalize/1 Tests
%%%===================================================================

normalize_simple_topic_test() ->
    ?assertEqual(<<"example.topic">>, macula_pubsub_topic:normalize(<<"Example.Topic">>)).

normalize_already_normalized_test() ->
    ?assertEqual(<<"example.topic">>, macula_pubsub_topic:normalize(<<"example.topic">>)).

normalize_with_spaces_test() ->
    ?assertEqual(<<"example.topic">>, macula_pubsub_topic:normalize(<<"  Example.Topic  ">>)).

normalize_preserves_wildcards_test() ->
    ?assertEqual(<<"example.*.topic">>, macula_pubsub_topic:normalize(<<"Example.*.Topic">>)).

%%%===================================================================
%%% namespace/1 Tests
%%%===================================================================

namespace_single_segment_test() ->
    ?assertEqual(<<"example">>, macula_pubsub_topic:namespace(<<"example">>)).

namespace_multiple_segments_test() ->
    ?assertEqual(<<"example">>, macula_pubsub_topic:namespace(<<"example.topic.message">>)).

namespace_with_wildcard_test() ->
    ?assertEqual(<<"example">>, macula_pubsub_topic:namespace(<<"example.*.topic">>)).

namespace_empty_first_segment_test() ->
    ?assertEqual(<<>>, macula_pubsub_topic:namespace(<<".example">>)).

%%%===================================================================
%%% segment_count/1 Tests
%%%===================================================================

segment_count_single_segment_test() ->
    ?assertEqual(1, macula_pubsub_topic:segment_count(<<"example">>)).

segment_count_two_segments_test() ->
    ?assertEqual(2, macula_pubsub_topic:segment_count(<<"example.topic">>)).

segment_count_many_segments_test() ->
    ?assertEqual(5, macula_pubsub_topic:segment_count(<<"one.two.three.four.five">>)).

segment_count_with_wildcards_test() ->
    ?assertEqual(3, macula_pubsub_topic:segment_count(<<"example.*.topic">>)),
    ?assertEqual(2, macula_pubsub_topic:segment_count(<<"example.#">>)).

segment_count_empty_topic_test() ->
    %% Empty binary has 0 segments
    ?assertEqual(0, macula_pubsub_topic:segment_count(<<>>)).

segment_count_trailing_dot_test() ->
    %% "example." splits to ["example", ""]
    ?assertEqual(2, macula_pubsub_topic:segment_count(<<"example.">>)).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

full_workflow_test() ->
    Topic = <<"  IoT.Sensor.Temperature  ">>,

    %% Normalize
    Normalized = macula_pubsub_topic:normalize(Topic),
    ?assertEqual(<<"iot.sensor.temperature">>, Normalized),

    %% Validate
    ?assertEqual(ok, macula_pubsub_topic:validate(Normalized)),

    %% Extract namespace
    Namespace = macula_pubsub_topic:namespace(Normalized),
    ?assertEqual(<<"iot">>, Namespace),

    %% Count segments
    Count = macula_pubsub_topic:segment_count(Normalized),
    ?assertEqual(3, Count),

    %% Pattern matching
    ?assert(macula_pubsub_topic:matches(Normalized, <<"iot.*.temperature">>)),
    ?assert(macula_pubsub_topic:matches(Normalized, <<"iot.#">>)),
    ?assertNot(macula_pubsub_topic:matches(Normalized, <<"iot.sensor.humidity">>)).

mqtt_style_patterns_test() ->
    %% MQTT-style subscription patterns (using dots instead of slashes)
    ?assert(macula_pubsub_topic:matches(<<"home.living_room.temp">>, <<"home.*.temp">>)),
    ?assert(macula_pubsub_topic:matches(<<"home.kitchen.humidity">>, <<"home.#">>)),
    ?assert(macula_pubsub_topic:matches(<<"sensor.data">>, <<"*.data">>)),
    ?assert(macula_pubsub_topic:matches(<<"any.topic.structure">>, <<"#">>)).

iot_sensor_patterns_test() ->
    %% IoT sensor topics
    Topics = [
        <<"iot.sensor.temperature.living_room">>,
        <<"iot.sensor.humidity.kitchen">>,
        <<"iot.actuator.light.bedroom">>
    ],

    %% All sensors
    lists:foreach(fun(Topic) ->
        ?assert(macula_pubsub_topic:matches(Topic, <<"iot.sensor.#">>))
    end, lists:sublist(Topics, 2)),

    %% Temperature sensors
    ?assert(macula_pubsub_topic:matches(lists:nth(1, Topics), <<"iot.*.temperature.*">>)),

    %% All devices in kitchen
    ?assert(macula_pubsub_topic:matches(lists:nth(2, Topics), <<"iot.#.kitchen">>)),

    %% Specific device type
    ?assertNot(macula_pubsub_topic:matches(lists:nth(3, Topics), <<"iot.sensor.#">>)),
    ?assert(macula_pubsub_topic:matches(lists:nth(3, Topics), <<"iot.actuator.#">>)).

application_event_patterns_test() ->
    %% Application event topics
    Events = [
        <<"app.user.login.success">>,
        <<"app.user.logout.success">>,
        <<"app.payment.processed.success">>,
        <<"app.payment.failed.error">>
    ],

    %% All user events
    lists:foreach(fun(Event) ->
        case binary:match(Event, <<"user">>) of
            nomatch -> ok;
            _ -> ?assert(macula_pubsub_topic:matches(Event, <<"app.user.#">>))
        end
    end, Events),

    %% All success events
    ?assert(macula_pubsub_topic:matches(lists:nth(1, Events), <<"app.#.success">>)),
    ?assert(macula_pubsub_topic:matches(lists:nth(2, Events), <<"app.#.success">>)),
    ?assert(macula_pubsub_topic:matches(lists:nth(3, Events), <<"app.#.success">>)),

    %% All payment events
    ?assert(macula_pubsub_topic:matches(lists:nth(3, Events), <<"app.payment.#">>)),
    ?assert(macula_pubsub_topic:matches(lists:nth(4, Events), <<"app.payment.#">>)).

pattern_specificity_test() ->
    %% Test pattern specificity - more specific patterns
    Topic = <<"iot.sensor.temperature.living_room">>,

    %% All patterns match, but with different specificity
    ?assert(macula_pubsub_topic:matches(Topic, <<"#">>)),                           % Least specific
    ?assert(macula_pubsub_topic:matches(Topic, <<"iot.#">>)),
    ?assert(macula_pubsub_topic:matches(Topic, <<"iot.sensor.#">>)),
    ?assert(macula_pubsub_topic:matches(Topic, <<"iot.*.temperature.*">>)),
    ?assert(macula_pubsub_topic:matches(Topic, <<"iot.sensor.temperature.*">>)),
    ?assert(macula_pubsub_topic:matches(Topic, <<"iot.sensor.temperature.living_room">>)).  % Most specific

wildcards_in_subscription_test() ->
    %% Simulate subscription patterns
    Subscriptions = [
        <<"iot.#">>,
        <<"app.user.*">>,
        <<"*.error">>,
        <<"#.alert">>
    ],

    %% Test messages against subscriptions
    ?assert(macula_pubsub_topic:matches(<<"iot.sensor.temp">>, lists:nth(1, Subscriptions))),
    ?assert(macula_pubsub_topic:matches(<<"app.user.login">>, lists:nth(2, Subscriptions))),
    ?assert(macula_pubsub_topic:matches(<<"system.error">>, lists:nth(3, Subscriptions))),
    ?assert(macula_pubsub_topic:matches(<<"iot.sensor.high.alert">>, lists:nth(4, Subscriptions))),

    %% Non-matching cases
    ?assertNot(macula_pubsub_topic:matches(<<"app.system.error">>, lists:nth(2, Subscriptions))),
    ?assertNot(macula_pubsub_topic:matches(<<"system.info.error">>, lists:nth(3, Subscriptions))).
