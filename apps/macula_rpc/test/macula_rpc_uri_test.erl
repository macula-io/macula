%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for macula_rpc_uri module.
%%% Tests written FIRST (TDD red phase).
%%% URI validation and pattern matching for RPC procedures.
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_uri_test).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Validation Tests
%%%===================================================================

%% Test: valid URIs pass validation
validate_valid_uri_test() ->
    ?assertEqual(ok, macula_rpc_uri:validate(<<"be.cortexiq.home.get_measurement">>)),
    ?assertEqual(ok, macula_rpc_uri:validate(<<"org.example.service.calculate">>)),
    ?assertEqual(ok, macula_rpc_uri:validate(<<"com.acme.api.v1.get_user">>)).

%% Test: empty URI fails validation
validate_empty_test() ->
    ?assertEqual({error, invalid_uri}, macula_rpc_uri:validate(<<>>)).

%% Test: leading dot fails validation
validate_leading_dot_test() ->
    ?assertEqual({error, invalid_uri}, macula_rpc_uri:validate(<<".be.cortexiq.home">>)).

%% Test: trailing dot fails validation
validate_trailing_dot_test() ->
    ?assertEqual({error, invalid_uri}, macula_rpc_uri:validate(<<"be.cortexiq.home.">>)).

%% Test: double dots fail validation
validate_double_dots_test() ->
    ?assertEqual({error, invalid_uri}, macula_rpc_uri:validate(<<"be..cortexiq.home">>)).

%% Test: single segment is valid
validate_single_segment_test() ->
    ?assertEqual(ok, macula_rpc_uri:validate(<<"procedure">>)).

%% Test: invalid characters fail validation
validate_invalid_chars_test() ->
    ?assertEqual({error, invalid_uri}, macula_rpc_uri:validate(<<"be.cortexiq.home@invalid">>)),
    ?assertEqual({error, invalid_uri}, macula_rpc_uri:validate(<<"be.cortexiq.home with space">>)),
    ?assertEqual({error, invalid_uri}, macula_rpc_uri:validate(<<"be.cortexiq.home/invalid">>)).

%%%===================================================================
%%% Normalization Tests
%%%===================================================================

%% Test: normalize lowercases URI
normalize_lowercase_test() ->
    ?assertEqual(<<"be.cortexiq.home.get">>,
                 macula_rpc_uri:normalize(<<"BE.CORTEXIQ.HOME.GET">>)).

%% Test: normalize trims whitespace
normalize_trim_test() ->
    ?assertEqual(<<"be.cortexiq.home">>,
                 macula_rpc_uri:normalize(<<"  be.cortexiq.home  ">>)).

%% Test: normalize removes double dots
normalize_double_dots_test() ->
    ?assertEqual(<<"be.cortexiq.home">>,
                 macula_rpc_uri:normalize(<<"be..cortexiq..home">>)).

%% Test: normalize handles multiple issues
normalize_combined_test() ->
    ?assertEqual(<<"be.cortexiq.home">>,
                 macula_rpc_uri:normalize(<<"  BE..CORTEXIQ.HOME  ">>)).

%%%===================================================================
%%% Namespace Tests
%%%===================================================================

%% Test: namespace extracts first segment
namespace_test() ->
    ?assertEqual(<<"be">>, macula_rpc_uri:namespace(<<"be.cortexiq.home.get">>)).

%% Test: namespace of single segment returns whole URI
namespace_single_segment_test() ->
    ?assertEqual(<<"procedure">>, macula_rpc_uri:namespace(<<"procedure">>)).

%% Test: namespace of empty URI returns empty
namespace_empty_test() ->
    ?assertEqual(<<>>, macula_rpc_uri:namespace(<<>>)).

%%%===================================================================
%%% Segment Count Tests
%%%===================================================================

%% Test: segment_count returns correct count
segment_count_test() ->
    ?assertEqual(4, macula_rpc_uri:segment_count(<<"be.cortexiq.home.get">>)),
    ?assertEqual(1, macula_rpc_uri:segment_count(<<"procedure">>)),
    ?assertEqual(0, macula_rpc_uri:segment_count(<<>>)).

%%%===================================================================
%%% Pattern Matching Tests (Future)
%%%===================================================================

%% Test: exact match
matches_exact_test() ->
    Uri = <<"be.cortexiq.home.get">>,
    Pattern = <<"be.cortexiq.home.get">>,
    ?assert(macula_rpc_uri:matches(Uri, Pattern)).

%% Test: no match
matches_no_match_test() ->
    Uri = <<"be.cortexiq.home.get">>,
    Pattern = <<"org.example.service">>,
    ?assertNot(macula_rpc_uri:matches(Uri, Pattern)).

%% Note: For now, RPC uses exact matching only.
%% Future: Could add wildcard patterns like pub/sub if needed.
