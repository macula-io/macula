-module(macula_topic_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% build / fact / hope
%%====================================================================

fact_builds_correct_topic_test() ->
    Topic = macula_topic:fact(<<"io.macula">>, <<"beam-campus/hecate">>,
                              <<"mpong">>, <<"lobby_opened">>, 1),
    ?assertEqual(<<"io.macula/beam-campus/hecate/mpong/lobby_opened_v1">>, Topic).

hope_builds_correct_topic_test() ->
    Topic = macula_topic:hope(<<"io.macula">>, <<"beam-campus/hecate">>,
                              <<"llm">>, <<"chat_to_model">>, 1),
    ?assertEqual(<<"io.macula/beam-campus/hecate/llm/chat_to_model_v1">>, Topic).

version_2_test() ->
    Topic = macula_topic:fact(<<"io.macula">>, <<"acme-org/trader">>,
                              <<"portfolio">>, <<"position_closed">>, 2),
    ?assertEqual(<<"io.macula/acme-org/trader/portfolio/position_closed_v2">>, Topic).

dotted_realm_test() ->
    Topic = macula_topic:fact(<<"io.macula">>, <<"beam-campus/hecate">>,
                              <<"weather">>, <<"wind_measured">>, 1),
    ?assertEqual(<<"io.macula/beam-campus/hecate/weather/wind_measured_v1">>, Topic).

%%====================================================================
%% parse
%%====================================================================

parse_valid_topic_test() ->
    {ok, Parts} = macula_topic:parse(<<"io.macula/beam-campus/hecate/mpong/lobby_opened_v1">>),
    ?assertEqual(<<"io.macula">>, maps:get(realm, Parts)),
    ?assertEqual(<<"beam-campus">>, maps:get(org, Parts)),
    ?assertEqual(<<"hecate">>, maps:get(app, Parts)),
    ?assertEqual(<<"beam-campus/hecate">>, maps:get(app_id, Parts)),
    ?assertEqual(<<"mpong">>, maps:get(domain, Parts)),
    ?assertEqual(<<"lobby_opened">>, maps:get(name, Parts)),
    ?assertEqual(1, maps:get(version, Parts)).

parse_higher_version_test() ->
    {ok, Parts} = macula_topic:parse(<<"io.macula/acme/trader/portfolio/position_closed_v12">>),
    ?assertEqual(12, maps:get(version, Parts)).

parse_rejects_no_version_test() ->
    {error, {missing_version_suffix, _}} =
        macula_topic:parse(<<"io.macula/org/app/domain/no_version">>).

parse_rejects_wrong_segment_count_test() ->
    {error, {invalid_structure, _}} =
        macula_topic:parse(<<"only.three.segments">>).

parse_rejects_too_many_segments_test() ->
    {error, {invalid_structure, _}} =
        macula_topic:parse(<<"a/b/c/d/e/f">>).

%%====================================================================
%% validate
%%====================================================================

validate_good_topic_test() ->
    ok = macula_topic:validate(<<"io.macula/beam-campus/hecate/llm/chat_to_model_v1">>).

validate_system_topic_test() ->
    ok = macula_topic:validate(<<"_mesh.node.up">>).

validate_rejects_bad_topic_test() ->
    {error, _} = macula_topic:validate(<<"no.structure.here">>).

%%====================================================================
%% is_system_topic
%%====================================================================

system_topic_test() ->
    ?assert(macula_topic:is_system_topic(<<"_mesh.node.up">>)),
    ?assertNot(macula_topic:is_system_topic(<<"io.macula/org/app/domain/name_v1">>)).

%%====================================================================
%% error cases
%%====================================================================

build_rejects_bad_app_id_test() ->
    ?assertError({invalid_app_id, <<"no-slash">>},
        macula_topic:build(<<"io.macula">>, <<"no-slash">>,
                           <<"domain">>, <<"name">>, 1)).

build_rejects_zero_version_test() ->
    ?assertError(function_clause,
        macula_topic:build(<<"io.macula">>, <<"org/app">>,
                           <<"domain">>, <<"name">>, 0)).

build_rejects_uppercase_test() ->
    ?assertError({invalid_segment, domain, <<"MyDomain">>},
        macula_topic:build(<<"io.macula">>, <<"org/app">>,
                           <<"MyDomain">>, <<"name">>, 1)).

%%====================================================================
%% roundtrip
%%====================================================================

roundtrip_test() ->
    Original = macula_topic:fact(<<"io.macula">>, <<"beam-campus/hecate">>,
                                  <<"mpong">>, <<"lobby_opened">>, 1),
    {ok, Parts} = macula_topic:parse(Original),
    Rebuilt = macula_topic:build(
        maps:get(realm, Parts),
        maps:get(app_id, Parts),
        maps:get(domain, Parts),
        maps:get(name, Parts),
        maps:get(version, Parts)
    ),
    ?assertEqual(Original, Rebuilt).
