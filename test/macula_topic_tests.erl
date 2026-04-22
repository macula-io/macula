-module(macula_topic_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Realm-tier builders
%%====================================================================

realm_fact_builds_correct_topic_test() ->
    Topic = macula_topic:realm_fact(<<"io.macula">>, <<"membership">>, <<"revoked">>, 1),
    ?assertEqual(<<"io.macula/_realm/_realm/membership/revoked_v1">>, Topic).

realm_hope_builds_correct_procedure_test() ->
    Proc = macula_topic:realm_hope(<<"io.macula">>, <<"auth">>, <<"check_health">>, 1),
    ?assertEqual(<<"io.macula/_realm/_realm/auth/check_health_v1">>, Proc).

realm_higher_version_test() ->
    Topic = macula_topic:realm_fact(<<"io.macula">>, <<"identity">>, <<"public_key_announced">>, 3),
    ?assertEqual(<<"io.macula/_realm/_realm/identity/public_key_announced_v3">>, Topic).

%%====================================================================
%% Org-tier builders
%%====================================================================

org_fact_builds_correct_topic_test() ->
    Topic = macula_topic:org_fact(<<"io.macula">>, <<"beam-campus">>,
                                  <<"licenses">>, <<"issued_batch">>, 1),
    ?assertEqual(<<"io.macula/beam-campus/_org/licenses/issued_batch_v1">>, Topic).

org_hope_builds_correct_procedure_test() ->
    Proc = macula_topic:org_hope(<<"io.macula">>, <<"beam-campus">>,
                                 <<"billing">>, <<"get_quota">>, 1),
    ?assertEqual(<<"io.macula/beam-campus/_org/billing/get_quota_v1">>, Proc).

%%====================================================================
%% App-tier builders
%%====================================================================

app_fact_builds_correct_topic_test() ->
    Topic = macula_topic:app_fact(<<"io.macula">>, <<"beam-campus">>, <<"hecate">>,
                                  <<"mpong">>, <<"lobby_opened">>, 1),
    ?assertEqual(<<"io.macula/beam-campus/hecate/mpong/lobby_opened_v1">>, Topic).

app_hope_builds_correct_procedure_test() ->
    Proc = macula_topic:app_hope(<<"io.macula">>, <<"beam-campus">>, <<"hecate">>,
                                 <<"llm">>, <<"chat_to_model">>, 1),
    ?assertEqual(<<"io.macula/beam-campus/hecate/llm/chat_to_model_v1">>, Proc).

app_other_org_test() ->
    Topic = macula_topic:app_fact(<<"io.macula">>, <<"acme-org">>, <<"trader">>,
                                  <<"portfolio">>, <<"position_closed">>, 2),
    ?assertEqual(<<"io.macula/acme-org/trader/portfolio/position_closed_v2">>, Topic).

%%====================================================================
%% Sentinel rejection at build time
%%====================================================================

build_rejects_realm_sentinel_in_user_org_when_app_real_test() ->
    %% User attempts to manually call build/6 with mismatched sentinels.
    ?assertError({invalid_tier_combination, app_must_be_realm_when_org_realm},
        macula_topic:build(<<"io.macula">>, <<"_realm">>, <<"hecate">>,
                           <<"membership">>, <<"revoked">>, 1)).

build_rejects_realm_sentinel_in_app_when_org_real_test() ->
    ?assertError({invalid_tier_combination, org_must_be_realm_when_app_realm},
        macula_topic:build(<<"io.macula">>, <<"beam-campus">>, <<"_realm">>,
                           <<"membership">>, <<"revoked">>, 1)).

build_rejects_org_sentinel_in_org_slot_test() ->
    ?assertError({invalid_tier_combination, org_sentinel_only_in_app_slot},
        macula_topic:build(<<"io.macula">>, <<"_org">>, <<"hecate">>,
                           <<"licenses">>, <<"issued_batch">>, 1)).

%%====================================================================
%% Segment validation
%%====================================================================

build_rejects_uppercase_org_test() ->
    ?assertError({invalid_segment, org, <<"BeamCampus">>},
        macula_topic:app_fact(<<"io.macula">>, <<"BeamCampus">>, <<"hecate">>,
                              <<"mpong">>, <<"lobby_opened">>, 1)).

build_rejects_uppercase_app_test() ->
    ?assertError({invalid_segment, app, <<"Hecate">>},
        macula_topic:app_fact(<<"io.macula">>, <<"beam-campus">>, <<"Hecate">>,
                              <<"mpong">>, <<"lobby_opened">>, 1)).

build_rejects_uppercase_domain_test() ->
    ?assertError({invalid_segment, domain, <<"MyDomain">>},
        macula_topic:app_fact(<<"io.macula">>, <<"beam-campus">>, <<"hecate">>,
                              <<"MyDomain">>, <<"event">>, 1)).

build_rejects_uppercase_name_test() ->
    ?assertError({invalid_segment, name, <<"BadName">>},
        macula_topic:app_fact(<<"io.macula">>, <<"beam-campus">>, <<"hecate">>,
                              <<"mpong">>, <<"BadName">>, 1)).

build_rejects_zero_version_test() ->
    ?assertError(function_clause,
        macula_topic:app_fact(<<"io.macula">>, <<"beam-campus">>, <<"hecate">>,
                              <<"mpong">>, <<"lobby_opened">>, 0)).

build_rejects_negative_version_test() ->
    ?assertError(function_clause,
        macula_topic:app_fact(<<"io.macula">>, <<"beam-campus">>, <<"hecate">>,
                              <<"mpong">>, <<"lobby_opened">>, -1)).

build_rejects_user_supplied_underscore_org_test() ->
    %% Org slot starting with underscore, but not the literal `_org` sentinel.
    %% Caught by the segment regex via app-tier path.
    ?assertError({invalid_segment, org, <<"_custom">>},
        macula_topic:app_fact(<<"io.macula">>, <<"_custom">>, <<"hecate">>,
                              <<"mpong">>, <<"lobby_opened">>, 1)).

%%====================================================================
%% parse — tier inference
%%====================================================================

parse_realm_tier_test() ->
    {ok, Parts} = macula_topic:parse(<<"io.macula/_realm/_realm/membership/revoked_v1">>),
    ?assertEqual(realm, maps:get(tier, Parts)),
    ?assertEqual(<<"io.macula">>, maps:get(realm, Parts)),
    ?assertEqual(<<"membership">>, maps:get(domain, Parts)),
    ?assertEqual(<<"revoked">>, maps:get(name, Parts)),
    ?assertEqual(1, maps:get(version, Parts)),
    %% No org/app keys for realm tier.
    ?assertNot(maps:is_key(org, Parts)),
    ?assertNot(maps:is_key(app, Parts)).

parse_org_tier_test() ->
    {ok, Parts} = macula_topic:parse(<<"io.macula/beam-campus/_org/licenses/issued_batch_v1">>),
    ?assertEqual(org, maps:get(tier, Parts)),
    ?assertEqual(<<"beam-campus">>, maps:get(org, Parts)),
    ?assertEqual(<<"licenses">>, maps:get(domain, Parts)),
    ?assertEqual(<<"issued_batch">>, maps:get(name, Parts)),
    ?assertNot(maps:is_key(app, Parts)).

parse_app_tier_test() ->
    {ok, Parts} = macula_topic:parse(<<"io.macula/beam-campus/hecate/mpong/lobby_opened_v1">>),
    ?assertEqual(app, maps:get(tier, Parts)),
    ?assertEqual(<<"beam-campus">>, maps:get(org, Parts)),
    ?assertEqual(<<"hecate">>, maps:get(app, Parts)),
    ?assertEqual(<<"mpong">>, maps:get(domain, Parts)),
    ?assertEqual(<<"lobby_opened">>, maps:get(name, Parts)).

parse_higher_version_test() ->
    {ok, Parts} = macula_topic:parse(<<"io.macula/acme/trader/portfolio/position_closed_v12">>),
    ?assertEqual(12, maps:get(version, Parts)).

parse_rejects_no_version_test() ->
    {error, {missing_version_suffix, _}} =
        macula_topic:parse(<<"io.macula/beam-campus/hecate/domain/no_version">>).

parse_rejects_wrong_segment_count_test() ->
    {error, {invalid_structure, _}} =
        macula_topic:parse(<<"only.three.segments">>).

parse_rejects_too_many_segments_test() ->
    {error, {invalid_structure, _}} =
        macula_topic:parse(<<"a/b/c/d/e/f">>).

parse_rejects_too_few_segments_test() ->
    {error, {invalid_structure, _}} =
        macula_topic:parse(<<"a/b/c/d">>).

%%====================================================================
%% parse — sentinel mismatch detection
%%====================================================================

parse_rejects_realm_sentinel_in_org_only_test() ->
    {error, {mismatched_realm_sentinel, app_must_be_realm_too}} =
        macula_topic:parse(<<"io.macula/_realm/hecate/membership/revoked_v1">>).

parse_rejects_realm_sentinel_in_app_only_test() ->
    {error, {mismatched_realm_sentinel, org_must_be_realm_too}} =
        macula_topic:parse(<<"io.macula/beam-campus/_realm/membership/revoked_v1">>).

parse_rejects_org_sentinel_in_org_slot_test() ->
    {error, {misplaced_org_sentinel, only_in_app_slot}} =
        macula_topic:parse(<<"io.macula/_org/hecate/licenses/issued_batch_v1">>).

parse_rejects_invalid_org_in_org_tier_test() ->
    {error, {invalid_segment, org, <<"BadOrg">>}} =
        macula_topic:parse(<<"io.macula/BadOrg/_org/licenses/issued_batch_v1">>).

parse_rejects_invalid_org_in_app_tier_test() ->
    {error, {invalid_segment, org, <<"BadOrg">>}} =
        macula_topic:parse(<<"io.macula/BadOrg/hecate/mpong/lobby_opened_v1">>).

parse_rejects_invalid_app_in_app_tier_test() ->
    {error, {invalid_segment, app, <<"BadApp">>}} =
        macula_topic:parse(<<"io.macula/beam-campus/BadApp/mpong/lobby_opened_v1">>).

%%====================================================================
%% validate
%%====================================================================

validate_good_realm_tier_test() ->
    ok = macula_topic:validate(<<"io.macula/_realm/_realm/membership/revoked_v1">>).

validate_good_org_tier_test() ->
    ok = macula_topic:validate(<<"io.macula/beam-campus/_org/licenses/issued_batch_v1">>).

validate_good_app_tier_test() ->
    ok = macula_topic:validate(<<"io.macula/beam-campus/hecate/llm/chat_to_model_v1">>).

validate_system_topic_test() ->
    ok = macula_topic:validate(<<"_mesh.node.up">>),
    ok = macula_topic:validate(<<"_mesh.relay.ping">>).

validate_rejects_dot_form_test() ->
    %% The legacy realm publisher's form. Must reject loudly.
    {error, _} = macula_topic:validate(<<"io.macula.membership.revoked">>).

validate_rejects_old_5_segment_no_tier_test() ->
    %% Old hardcoded /beam-campus/hecate/ form used as realm-owned topic.
    %% Now resolves to app-tier — valid topic, but tier semantics changed.
    %% This test confirms it parses as app, which is the correct outcome —
    %% callers must explicitly use realm_fact for realm-owned events now.
    ok = macula_topic:validate(<<"io.macula/beam-campus/hecate/membership/revoked_v1">>),
    {ok, #{tier := app}} = macula_topic:parse(<<"io.macula/beam-campus/hecate/membership/revoked_v1">>).

validate_rejects_arbitrary_garbage_test() ->
    {error, _} = macula_topic:validate(<<"no.structure.here">>),
    {error, _} = macula_topic:validate(<<"">>),
    {error, _} = macula_topic:validate(<<"/////">>).

validate_rejects_non_mesh_underscore_prefix_test() ->
    %% Only `_mesh.*` is a valid system topic. Other underscore prefixes
    %% are rejected — they are NOT exempt.
    {error, _} = macula_topic:validate(<<"_dht.list_gateways">>),
    {error, _} = macula_topic:validate(<<"_realm.foo.bar">>).

%%====================================================================
%% is_system_topic
%%====================================================================

system_topic_test() ->
    ?assert(macula_topic:is_system_topic(<<"_mesh.node.up">>)),
    ?assert(macula_topic:is_system_topic(<<"_mesh.node.down">>)),
    ?assert(macula_topic:is_system_topic(<<"_mesh.relay.ping">>)),
    ?assertNot(macula_topic:is_system_topic(<<"io.macula/_realm/_realm/membership/revoked_v1">>)),
    ?assertNot(macula_topic:is_system_topic(<<"io.macula/beam-campus/hecate/mpong/lobby_opened_v1">>)),
    ?assertNot(macula_topic:is_system_topic(<<"_dht.list_gateways">>)),
    ?assertNot(macula_topic:is_system_topic(<<"_realm.foo">>)).

%%====================================================================
%% Roundtrip per tier
%%====================================================================

roundtrip_realm_test() ->
    Original = macula_topic:realm_fact(<<"io.macula">>, <<"identity">>, <<"public_key_announced">>, 1),
    {ok, #{tier := realm, realm := R, domain := D, name := N, version := V}} =
        macula_topic:parse(Original),
    Rebuilt = macula_topic:realm_fact(R, D, N, V),
    ?assertEqual(Original, Rebuilt).

roundtrip_org_test() ->
    Original = macula_topic:org_fact(<<"io.macula">>, <<"beam-campus">>,
                                     <<"licenses">>, <<"revoked">>, 1),
    {ok, #{tier := org, realm := R, org := O, domain := D, name := N, version := V}} =
        macula_topic:parse(Original),
    Rebuilt = macula_topic:org_fact(R, O, D, N, V),
    ?assertEqual(Original, Rebuilt).

roundtrip_app_test() ->
    Original = macula_topic:app_fact(<<"io.macula">>, <<"beam-campus">>, <<"hecate">>,
                                     <<"mpong">>, <<"lobby_opened">>, 1),
    {ok, #{tier := app, realm := R, org := O, app := A, domain := D, name := N, version := V}} =
        macula_topic:parse(Original),
    Rebuilt = macula_topic:app_fact(R, O, A, D, N, V),
    ?assertEqual(Original, Rebuilt).

%%====================================================================
%% Hope vs Fact share the same topic shape
%%====================================================================

hope_and_fact_same_shape_test() ->
    %% Builders enforce structure, not tense — that's a code-review concern.
    %% Verifies a fact and a hope built with the same args produce the same string.
    F = macula_topic:app_fact(<<"io.macula">>, <<"beam-campus">>, <<"hecate">>,
                              <<"x">>, <<"event_v">>, 1),
    H = macula_topic:app_hope(<<"io.macula">>, <<"beam-campus">>, <<"hecate">>,
                              <<"x">>, <<"event_v">>, 1),
    ?assertEqual(F, H).

%%====================================================================
%% Realm with multiple dots is allowed
%%====================================================================

multi_dot_realm_test() ->
    Topic = macula_topic:app_fact(<<"io.macula.demo">>, <<"beam-campus">>, <<"hecate">>,
                                  <<"mpong">>, <<"lobby_opened">>, 1),
    ?assertEqual(<<"io.macula.demo/beam-campus/hecate/mpong/lobby_opened_v1">>, Topic),
    {ok, #{realm := <<"io.macula.demo">>}} = macula_topic:parse(Topic).
