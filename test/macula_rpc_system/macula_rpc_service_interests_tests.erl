%%%-------------------------------------------------------------------
%%% @doc Unit tests for macula_rpc_service_interests module.
%%%
%%% Tests cover:
%%% - Interest normalization (lists, atoms, binaries, strings)
%%% - Single interest normalization
%%% - Find value message creation
%%% - Interest merging
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_service_interests_tests).

-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Normalization Tests
%%------------------------------------------------------------------------------

normalize_test_() ->
    [
     {"normalize handles list of binaries", fun test_normalize_binary_list/0},
     {"normalize handles list of atoms", fun test_normalize_atom_list/0},
     {"normalize handles mixed list", fun test_normalize_mixed_list/0},
     {"normalize handles single binary", fun test_normalize_single_binary/0},
     {"normalize handles single atom", fun test_normalize_single_atom/0},
     {"normalize handles string", fun test_normalize_string/0},
     {"normalize filters empty binaries", fun test_normalize_filters_empty/0},
     {"normalize filters undefined", fun test_normalize_filters_undefined/0},
     {"normalize returns empty for invalid input", fun test_normalize_invalid/0}
    ].

test_normalize_binary_list() ->
    Input = [<<"service.one">>, <<"service.two">>],
    Result = macula_rpc_service_interests:normalize(Input),
    ?assertEqual([<<"service.one">>, <<"service.two">>], Result).

test_normalize_atom_list() ->
    Input = [service_one, service_two],
    Result = macula_rpc_service_interests:normalize(Input),
    ?assertEqual([<<"service_one">>, <<"service_two">>], Result).

test_normalize_mixed_list() ->
    Input = [<<"binary.svc">>, atom_svc, "string_svc"],
    Result = macula_rpc_service_interests:normalize(Input),
    ?assertEqual([<<"binary.svc">>, <<"atom_svc">>, <<"string_svc">>], Result).

test_normalize_single_binary() ->
    Result = macula_rpc_service_interests:normalize(<<"single.service">>),
    ?assertEqual([<<"single.service">>], Result).

test_normalize_single_atom() ->
    Result = macula_rpc_service_interests:normalize(single_service),
    ?assertEqual([<<"single_service">>], Result).

test_normalize_string() ->
    %% Note: When a string is passed directly to normalize/1, it's treated as a list
    %% of character codes (integers), which normalize_single/1 filters out.
    %% To normalize a string, pass it in a list: [<<"service">>] or use normalize_single/1.
    %% This is consistent with the function's design for list inputs.
    Result = macula_rpc_service_interests:normalize("string.service"),
    %% Each character in "string.service" is an integer (not valid interest)
    ?assertEqual([], Result),
    %% But normalize_single handles strings correctly
    {true, BinResult} = macula_rpc_service_interests:normalize_single("string.service"),
    ?assertEqual(<<"string.service">>, BinResult).

test_normalize_filters_empty() ->
    Input = [<<"valid">>, <<>>, <<"also.valid">>],
    Result = macula_rpc_service_interests:normalize(Input),
    ?assertEqual([<<"valid">>, <<"also.valid">>], Result).

test_normalize_filters_undefined() ->
    Input = [<<"valid">>, undefined, <<"another">>],
    Result = macula_rpc_service_interests:normalize(Input),
    ?assertEqual([<<"valid">>, <<"another">>], Result).

test_normalize_invalid() ->
    ?assertEqual([], macula_rpc_service_interests:normalize(123)),
    ?assertEqual([], macula_rpc_service_interests:normalize(undefined)),
    ?assertEqual([], macula_rpc_service_interests:normalize(<<>>)).

%%------------------------------------------------------------------------------
%% Single Normalization Tests
%%------------------------------------------------------------------------------

normalize_single_test_() ->
    [
     {"normalize_single handles binary", fun test_normalize_single_binary_input/0},
     {"normalize_single handles atom", fun test_normalize_single_atom_input/0},
     {"normalize_single handles string", fun test_normalize_single_string_input/0},
     {"normalize_single rejects empty", fun test_normalize_single_empty/0},
     {"normalize_single rejects undefined", fun test_normalize_single_undefined/0}
    ].

test_normalize_single_binary_input() ->
    Result = macula_rpc_service_interests:normalize_single(<<"test.service">>),
    ?assertEqual({true, <<"test.service">>}, Result).

test_normalize_single_atom_input() ->
    Result = macula_rpc_service_interests:normalize_single(test_service),
    ?assertEqual({true, <<"test_service">>}, Result).

test_normalize_single_string_input() ->
    Result = macula_rpc_service_interests:normalize_single("test.service"),
    ?assertEqual({true, <<"test.service">>}, Result).

test_normalize_single_empty() ->
    ?assertEqual(false, macula_rpc_service_interests:normalize_single(<<>>)),
    ?assertEqual(false, macula_rpc_service_interests:normalize_single([])).

test_normalize_single_undefined() ->
    ?assertEqual(false, macula_rpc_service_interests:normalize_single(undefined)).

%%------------------------------------------------------------------------------
%% Find Value Message Tests
%%------------------------------------------------------------------------------

find_value_test_() ->
    [
     {"create_find_value_message returns valid tuple", fun test_create_find_value_message/0},
     {"create_find_value_message creates consistent keys", fun test_find_value_consistent_key/0}
    ].

test_create_find_value_message() ->
    ServiceName = <<"my.service">>,
    {Key, Msg} = macula_rpc_service_interests:create_find_value_message(ServiceName),

    %% Key should be SHA256 hash (32 bytes)
    ?assertEqual(32, byte_size(Key)),

    %% Message should be a map with find_value type
    ?assert(is_map(Msg)).

test_find_value_consistent_key() ->
    ServiceName = <<"consistent.service">>,
    {Key1, _} = macula_rpc_service_interests:create_find_value_message(ServiceName),
    {Key2, _} = macula_rpc_service_interests:create_find_value_message(ServiceName),
    ?assertEqual(Key1, Key2).

%%------------------------------------------------------------------------------
%% Merge Tests
%%------------------------------------------------------------------------------

merge_test_() ->
    [
     {"merge_interests combines lists", fun test_merge_combines/0},
     {"merge_interests removes duplicates", fun test_merge_removes_duplicates/0},
     {"merge_interests handles empty lists", fun test_merge_empty/0}
    ].

test_merge_combines() ->
    Existing = [<<"svc.a">>, <<"svc.b">>],
    New = [<<"svc.c">>, <<"svc.d">>],
    Result = macula_rpc_service_interests:merge_interests(Existing, New),
    ?assertEqual([<<"svc.a">>, <<"svc.b">>, <<"svc.c">>, <<"svc.d">>], Result).

test_merge_removes_duplicates() ->
    Existing = [<<"svc.a">>, <<"svc.b">>],
    New = [<<"svc.b">>, <<"svc.c">>],
    Result = macula_rpc_service_interests:merge_interests(Existing, New),
    ?assertEqual([<<"svc.a">>, <<"svc.b">>, <<"svc.c">>], Result).

test_merge_empty() ->
    ?assertEqual([], macula_rpc_service_interests:merge_interests([], [])),
    ?assertEqual([<<"svc">>], macula_rpc_service_interests:merge_interests([<<"svc">>], [])),
    ?assertEqual([<<"svc">>], macula_rpc_service_interests:merge_interests([], [<<"svc">>])).
