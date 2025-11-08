%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_pubsub_topic module.
%%% Tests written FIRST (TDD red phase).
%%% @end
%%%-------------------------------------------------------------------
-module(macula_pubsub_topic_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Topic Validation Tests
%%%===================================================================

%% Test: validate accepts valid topic
validate_accepts_valid_test() ->
    Topic = <<"be.cortexiq.home.measured">>,
    ?assertEqual(ok, macula_pubsub_topic:validate(Topic)).

%% Test: validate accepts topic with numbers
validate_accepts_numbers_test() ->
    Topic = <<"be.cortexiq.home123.measured">>,
    ?assertEqual(ok, macula_pubsub_topic:validate(Topic)).

%% Test: validate rejects empty topic
validate_rejects_empty_test() ->
    Topic = <<>>,
    ?assertEqual({error, invalid_topic}, macula_pubsub_topic:validate(Topic)).

%% Test: validate rejects topic with invalid characters
validate_rejects_invalid_chars_test() ->
    Topic = <<"be.cortexiq.home!.measured">>,
    ?assertEqual({error, invalid_topic}, macula_pubsub_topic:validate(Topic)).

%% Test: validate rejects topic starting with dot
validate_rejects_leading_dot_test() ->
    Topic = <<".be.cortexiq.home">>,
    ?assertEqual({error, invalid_topic}, macula_pubsub_topic:validate(Topic)).

%% Test: validate rejects topic ending with dot
validate_rejects_trailing_dot_test() ->
    Topic = <<"be.cortexiq.home.">>,
    ?assertEqual({error, invalid_topic}, macula_pubsub_topic:validate(Topic)).

%%%===================================================================
%%% Pattern Matching Tests (Exact)
%%%===================================================================

%% Test: matches exact topic
matches_exact_test() ->
    Topic = <<"be.cortexiq.home.measured">>,
    Pattern = <<"be.cortexiq.home.measured">>,
    ?assert(macula_pubsub_topic:matches(Topic, Pattern)).

%% Test: does not match different topic
matches_different_test() ->
    Topic = <<"be.cortexiq.home.measured">>,
    Pattern = <<"be.cortexiq.provider.measured">>,
    ?assertNot(macula_pubsub_topic:matches(Topic, Pattern)).

%%%===================================================================
%%% Pattern Matching Tests (Single-level wildcard *)
%%%===================================================================

%% Test: matches with single-level wildcard
matches_single_wildcard_test() ->
    Topic = <<"be.cortexiq.home.measured">>,
    Pattern = <<"be.cortexiq.*.measured">>,
    ?assert(macula_pubsub_topic:matches(Topic, Pattern)).

%% Test: wildcard matches one segment exactly
matches_wildcard_one_segment_test() ->
    Topic = <<"be.cortexiq.home.measured">>,
    Pattern = <<"be.*.home.measured">>,
    ?assert(macula_pubsub_topic:matches(Topic, Pattern)).

%% Test: wildcard does not match multiple segments
matches_wildcard_not_multiple_test() ->
    Topic = <<"be.cortexiq.home.electricity.measured">>,
    Pattern = <<"be.cortexiq.*.measured">>,
    ?assertNot(macula_pubsub_topic:matches(Topic, Pattern)).

%% Test: multiple wildcards in pattern
matches_multiple_wildcards_test() ->
    Topic = <<"be.cortexiq.home.measured">>,
    Pattern = <<"be.*.*.measured">>,
    ?assert(macula_pubsub_topic:matches(Topic, Pattern)).

%%%===================================================================
%%% Pattern Matching Tests (Multi-level wildcard #)
%%%===================================================================

%% Test: matches with multi-level wildcard at end
matches_multi_wildcard_end_test() ->
    Topic = <<"be.cortexiq.home.measured">>,
    Pattern = <<"be.cortexiq.#">>,
    ?assert(macula_pubsub_topic:matches(Topic, Pattern)).

%% Test: multi-level wildcard matches zero segments
matches_multi_wildcard_zero_test() ->
    Topic = <<"be.cortexiq">>,
    Pattern = <<"be.cortexiq.#">>,
    ?assert(macula_pubsub_topic:matches(Topic, Pattern)).

%% Test: multi-level wildcard matches one segment
matches_multi_wildcard_one_test() ->
    Topic = <<"be.cortexiq.home">>,
    Pattern = <<"be.cortexiq.#">>,
    ?assert(macula_pubsub_topic:matches(Topic, Pattern)).

%% Test: multi-level wildcard matches many segments
matches_multi_wildcard_many_test() ->
    Topic = <<"be.cortexiq.home.electricity.measured.watts">>,
    Pattern = <<"be.cortexiq.#">>,
    ?assert(macula_pubsub_topic:matches(Topic, Pattern)).

%% Test: multi-level wildcard in middle
matches_multi_wildcard_middle_test() ->
    Topic = <<"be.cortexiq.home.electricity.measured">>,
    Pattern = <<"be.#.measured">>,
    ?assert(macula_pubsub_topic:matches(Topic, Pattern)).

%% Test: multi-level wildcard does not match wrong prefix
matches_multi_wildcard_wrong_prefix_test() ->
    Topic = <<"be.cortexiq.home.measured">>,
    Pattern = <<"org.example.#">>,
    ?assertNot(macula_pubsub_topic:matches(Topic, Pattern)).

%%%===================================================================
%%% Normalization Tests
%%%===================================================================

%% Test: normalize trims whitespace
normalize_trims_test() ->
    Topic = <<"  be.cortexiq.home  ">>,
    ?assertEqual(<<"be.cortexiq.home">>, macula_pubsub_topic:normalize(Topic)).

%% Test: normalize lowercases
normalize_lowercases_test() ->
    Topic = <<"BE.CortexIQ.Home">>,
    ?assertEqual(<<"be.cortexiq.home">>, macula_pubsub_topic:normalize(Topic)).

%% Test: normalize removes double dots
normalize_removes_double_dots_test() ->
    Topic = <<"be..cortexiq.home">>,
    ?assertEqual(<<"be.cortexiq.home">>, macula_pubsub_topic:normalize(Topic)).

%%%===================================================================
%%% Namespace Extraction Tests
%%%===================================================================

%% Test: namespace extracts first segment
namespace_extracts_first_test() ->
    Topic = <<"be.cortexiq.home.measured">>,
    ?assertEqual(<<"be">>, macula_pubsub_topic:namespace(Topic)).

%% Test: namespace handles single segment
namespace_single_segment_test() ->
    Topic = <<"be">>,
    ?assertEqual(<<"be">>, macula_pubsub_topic:namespace(Topic)).

%% Test: namespace returns empty for empty topic
namespace_empty_test() ->
    Topic = <<>>,
    ?assertEqual(<<>>, macula_pubsub_topic:namespace(Topic)).

%%%===================================================================
%%% Segment Count Tests
%%%===================================================================

%% Test: segment_count returns correct count
segment_count_test() ->
    Topic = <<"be.cortexiq.home.measured">>,
    ?assertEqual(4, macula_pubsub_topic:segment_count(Topic)).

%% Test: segment_count for single segment
segment_count_single_test() ->
    Topic = <<"be">>,
    ?assertEqual(1, macula_pubsub_topic:segment_count(Topic)).

%% Test: segment_count for empty topic
segment_count_empty_test() ->
    Topic = <<>>,
    ?assertEqual(0, macula_pubsub_topic:segment_count(Topic)).
