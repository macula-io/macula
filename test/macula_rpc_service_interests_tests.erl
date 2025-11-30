%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for macula_rpc_service_interests module.
%%% Tests service interest normalization and configuration including:
%%% - Single value normalization
%%% - List normalization
%%% - Different input types (binary, atom, string)
%%% - Interest merging
%%% @end
%%%-------------------------------------------------------------------
-module(macula_rpc_service_interests_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Normalize Single Tests
%%%===================================================================

normalize_single_test_() ->
    [
        {"binary interest returns {true, Binary}",
         fun() ->
             ?assertEqual({true, <<"my.service">>},
                         macula_rpc_service_interests:normalize_single(<<"my.service">>))
         end},

        {"atom interest converts to binary",
         fun() ->
             ?assertEqual({true, <<"my_service">>},
                         macula_rpc_service_interests:normalize_single(my_service))
         end},

        {"string interest converts to binary",
         fun() ->
             ?assertEqual({true, <<"string.service">>},
                         macula_rpc_service_interests:normalize_single("string.service"))
         end},

        {"empty binary returns false",
         fun() ->
             ?assertEqual(false, macula_rpc_service_interests:normalize_single(<<>>))
         end},

        {"undefined returns false",
         fun() ->
             ?assertEqual(false, macula_rpc_service_interests:normalize_single(undefined))
         end},

        {"empty list returns false",
         fun() ->
             ?assertEqual(false, macula_rpc_service_interests:normalize_single([]))
         end},

        {"number returns false",
         fun() ->
             ?assertEqual(false, macula_rpc_service_interests:normalize_single(123))
         end}
    ].

%%%===================================================================
%%% Normalize List Tests
%%%===================================================================

normalize_list_test_() ->
    [
        {"normalizes list of binaries",
         fun() ->
             Input = [<<"svc1">>, <<"svc2">>, <<"svc3">>],
             ?assertEqual([<<"svc1">>, <<"svc2">>, <<"svc3">>],
                         macula_rpc_service_interests:normalize(Input))
         end},

        {"normalizes mixed types",
         fun() ->
             Input = [<<"binary.svc">>, atom_svc, "string.svc"],
             Result = macula_rpc_service_interests:normalize(Input),
             ?assertEqual(3, length(Result)),
             ?assert(lists:member(<<"binary.svc">>, Result)),
             ?assert(lists:member(<<"atom_svc">>, Result)),
             ?assert(lists:member(<<"string.svc">>, Result))
         end},

        {"filters out invalid entries",
         fun() ->
             Input = [<<"valid">>, undefined, <<>>, <<"also_valid">>],
             Result = macula_rpc_service_interests:normalize(Input),
             ?assertEqual([<<"valid">>, <<"also_valid">>], Result)
         end},

        {"handles single binary",
         fun() ->
             ?assertEqual([<<"single">>], macula_rpc_service_interests:normalize(<<"single">>))
         end},

        {"handles single atom",
         fun() ->
             ?assertEqual([<<"single_atom">>], macula_rpc_service_interests:normalize(single_atom))
         end},

        {"empty list returns empty",
         fun() ->
             ?assertEqual([], macula_rpc_service_interests:normalize([]))
         end},

        {"undefined returns empty",
         fun() ->
             ?assertEqual([], macula_rpc_service_interests:normalize(undefined))
         end}
    ].

%%%===================================================================
%%% Merge Interests Tests
%%%===================================================================

merge_interests_test_() ->
    [
        {"merges two disjoint lists",
         fun() ->
             Existing = [<<"svc1">>, <<"svc2">>],
             New = [<<"svc3">>, <<"svc4">>],
             Result = macula_rpc_service_interests:merge_interests(Existing, New),
             ?assertEqual(4, length(Result)),
             ?assert(lists:member(<<"svc1">>, Result)),
             ?assert(lists:member(<<"svc2">>, Result)),
             ?assert(lists:member(<<"svc3">>, Result)),
             ?assert(lists:member(<<"svc4">>, Result))
         end},

        {"removes duplicates",
         fun() ->
             Existing = [<<"svc1">>, <<"svc2">>],
             New = [<<"svc2">>, <<"svc3">>],
             Result = macula_rpc_service_interests:merge_interests(Existing, New),
             ?assertEqual([<<"svc1">>, <<"svc2">>, <<"svc3">>], Result)
         end},

        {"merging with empty returns original",
         fun() ->
             Existing = [<<"svc1">>, <<"svc2">>],
             ?assertEqual([<<"svc1">>, <<"svc2">>],
                         macula_rpc_service_interests:merge_interests(Existing, []))
         end},

        {"merging empty with new returns new",
         fun() ->
             New = [<<"svc1">>, <<"svc2">>],
             ?assertEqual([<<"svc1">>, <<"svc2">>],
                         macula_rpc_service_interests:merge_interests([], New))
         end},

        {"result is sorted",
         fun() ->
             Existing = [<<"zebra">>, <<"apple">>],
             New = [<<"mango">>],
             Result = macula_rpc_service_interests:merge_interests(Existing, New),
             ?assertEqual([<<"apple">>, <<"mango">>, <<"zebra">>], Result)
         end}
    ].

%%%===================================================================
%%% Create Find Value Message Tests
%%%===================================================================

create_find_value_message_test_() ->
    [
        {"creates service key from service name",
         fun() ->
             ServiceName = <<"my.service">>,
             {ServiceKey, _Msg} = macula_rpc_service_interests:create_find_value_message(ServiceName),
             %% Service key should be SHA256 hash of service name
             ExpectedKey = crypto:hash(sha256, ServiceName),
             ?assertEqual(ExpectedKey, ServiceKey)
         end},

        {"creates valid FIND_VALUE message",
         fun() ->
             ServiceName = <<"test.service">>,
             {_ServiceKey, Msg} = macula_rpc_service_interests:create_find_value_message(ServiceName),
             ?assert(is_map(Msg))
         end}
    ].

