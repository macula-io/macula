%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_rpc_names module.
%%%
%%% Tests name validation and utilities for RPC procedures.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_names_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% validate/1 Tests
%%%===================================================================

validate_valid_simple_name_test() ->
    %% GIVEN: A valid simple name
    Name = <<"com.example.service">>,

    %% WHEN: Validating
    Result = macula_rpc_names:validate(Name),

    %% THEN: Should be valid
    ?assertEqual(ok, Result).

validate_valid_long_name_test() ->
    %% GIVEN: A valid long name
    Name = <<"org.macula.rpc.user.profile.get">>,

    %% WHEN: Validating
    Result = macula_rpc_names:validate(Name),

    %% THEN: Should be valid
    ?assertEqual(ok, Result).

validate_valid_with_underscores_test() ->
    %% GIVEN: A name with underscores
    Name = <<"com.example.my_service.get_user">>,

    %% WHEN: Validating
    Result = macula_rpc_names:validate(Name),

    %% THEN: Should be valid
    ?assertEqual(ok, Result).

validate_valid_with_hyphens_test() ->
    %% GIVEN: A name with hyphens
    Name = <<"com.example.my-service.get-user">>,

    %% WHEN: Validating
    Result = macula_rpc_names:validate(Name),

    %% THEN: Should be valid
    ?assertEqual(ok, Result).

validate_valid_with_numbers_test() ->
    %% GIVEN: A name with numbers
    Name = <<"com.example.service2.api_v3">>,

    %% WHEN: Validating
    Result = macula_rpc_names:validate(Name),

    %% THEN: Should be valid
    ?assertEqual(ok, Result).

validate_invalid_empty_test() ->
    %% GIVEN: An empty name
    Name = <<>>,

    %% WHEN: Validating
    Result = macula_rpc_names:validate(Name),

    %% THEN: Should be invalid
    ?assertEqual({error, invalid_name}, Result).

validate_invalid_leading_dot_test() ->
    %% GIVEN: A name with leading dot
    Name = <<".com.example.service">>,

    %% WHEN: Validating
    Result = macula_rpc_names:validate(Name),

    %% THEN: Should be invalid
    ?assertEqual({error, invalid_name}, Result).

validate_invalid_trailing_dot_test() ->
    %% GIVEN: A name with trailing dot
    Name = <<"com.example.service.">>,

    %% WHEN: Validating
    Result = macula_rpc_names:validate(Name),

    %% THEN: Should be invalid
    ?assertEqual({error, invalid_name}, Result).

validate_invalid_double_dot_test() ->
    %% GIVEN: A name with double dots
    Name = <<"com..example.service">>,

    %% WHEN: Validating
    Result = macula_rpc_names:validate(Name),

    %% THEN: Should be invalid
    ?assertEqual({error, invalid_name}, Result).

%%%===================================================================
%%% matches/2 Tests
%%%===================================================================

matches_exact_match_test() ->
    %% GIVEN: Identical names
    Name = <<"com.example.service">>,
    Pattern = <<"com.example.service">>,

    %% WHEN: Matching
    Result = macula_rpc_names:matches(Name, Pattern),

    %% THEN: Should match
    ?assertEqual(true, Result).

matches_different_names_test() ->
    %% GIVEN: Different names
    Name = <<"com.example.service1">>,
    Pattern = <<"com.example.service2">>,

    %% WHEN: Matching
    Result = macula_rpc_names:matches(Name, Pattern),

    %% THEN: Should not match
    ?assertEqual(false, Result).

%%%===================================================================
%%% normalize/1 Tests
%%%===================================================================

normalize_lowercase_test() ->
    %% GIVEN: A name with uppercase
    Name = <<"COM.EXAMPLE.SERVICE">>,

    %% WHEN: Normalizing
    Result = macula_rpc_names:normalize(Name),

    %% THEN: Should be lowercase
    ?assertEqual(<<"com.example.service">>, Result).

normalize_mixed_case_test() ->
    %% GIVEN: A mixed case name
    Name = <<"Com.Example.MyService">>,

    %% WHEN: Normalizing
    Result = macula_rpc_names:normalize(Name),

    %% THEN: Should be lowercase
    ?assertEqual(<<"com.example.myservice">>, Result).

%%%===================================================================
%%% namespace/1 Tests
%%%===================================================================

namespace_extracts_first_segment_test() ->
    %% GIVEN: A multi-segment name
    Name = <<"com.example.service.method">>,

    %% WHEN: Getting namespace
    Result = macula_rpc_names:namespace(Name),

    %% THEN: Should return first segment
    ?assertEqual(<<"com">>, Result).

namespace_single_segment_test() ->
    %% GIVEN: A single segment name
    Name = <<"service">>,

    %% WHEN: Getting namespace
    Result = macula_rpc_names:namespace(Name),

    %% THEN: Should return the name itself
    ?assertEqual(<<"service">>, Result).

%%%===================================================================
%%% segment_count/1 Tests
%%%===================================================================

segment_count_single_test() ->
    %% GIVEN: Single segment name
    Name = <<"service">>,

    %% WHEN: Counting segments
    Result = macula_rpc_names:segment_count(Name),

    %% THEN: Should be 1
    ?assertEqual(1, Result).

segment_count_multiple_test() ->
    %% GIVEN: Multi-segment name
    Name = <<"com.example.service.method">>,

    %% WHEN: Counting segments
    Result = macula_rpc_names:segment_count(Name),

    %% THEN: Should be 4
    ?assertEqual(4, Result).

segment_count_two_test() ->
    %% GIVEN: Two segment name
    Name = <<"example.service">>,

    %% WHEN: Counting segments
    Result = macula_rpc_names:segment_count(Name),

    %% THEN: Should be 2
    ?assertEqual(2, Result).
