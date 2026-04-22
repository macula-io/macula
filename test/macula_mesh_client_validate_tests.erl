%%% Validation gate at macula_mesh_client public API.
%%%
%%% These tests verify that every entry point that accepts a topic
%%% or RPC procedure name rejects non-canonical strings BEFORE any
%%% gen_server interaction happens. They prove that drift like the
%%% dot-form realm publisher (which silently published to a dead
%%% route for months) cannot recur — a malformed topic now errors
%%% loudly at the call site.
%%%
%%% The tests pass `self()` as the Pid because validation throws
%%% before the cast/call ever reaches the gen_server. No real
%%% mesh_client process is required.
-module(macula_mesh_client_validate_tests).
-include_lib("eunit/include/eunit.hrl").

-define(BAD_DOT_TOPIC, <<"io.macula.membership.revoked">>).
-define(BAD_GARBAGE,   <<"no_structure_at_all">>).
-define(GOOD_REALM,    <<"io.macula/_realm/_realm/membership/revoked_v1">>).
-define(GOOD_APP,      <<"io.macula/beam-campus/hecate/mpong/lobby_opened_v1">>).
-define(SYSTEM_TOPIC,  <<"_mesh.node.up">>).

%%====================================================================
%% Entry points that take a Topic
%%====================================================================

publish_rejects_dot_form_test() ->
    ?assertError({invalid_topic, ?BAD_DOT_TOPIC, _},
        macula_mesh_client:publish(self(), ?BAD_DOT_TOPIC, #{})).

publish_rejects_garbage_test() ->
    ?assertError({invalid_topic, ?BAD_GARBAGE, _},
        macula_mesh_client:publish(self(), ?BAD_GARBAGE, #{})).

publish_rejects_non_binary_test() ->
    ?assertError({invalid_topic, "string-not-binary", not_a_binary},
        macula_mesh_client:publish(self(), "string-not-binary", #{})).

publish_rejects_empty_test() ->
    ?assertError({invalid_topic, <<>>, _},
        macula_mesh_client:publish(self(), <<>>, #{})).

subscribe_rejects_dot_form_test() ->
    ?assertError({invalid_topic, ?BAD_DOT_TOPIC, _},
        macula_mesh_client:subscribe(self(), ?BAD_DOT_TOPIC, fun(_) -> ok end)).

%%====================================================================
%% Entry points that take an RPC Procedure name
%%====================================================================

advertise_rejects_dot_form_test() ->
    ?assertError({invalid_topic, <<"io.macula.realm.check_health">>, _},
        macula_mesh_client:advertise(self(),
            <<"io.macula.realm.check_health">>,
            fun(_) -> {ok, #{}} end)).

unadvertise_rejects_dot_form_test() ->
    ?assertError({invalid_topic, <<"io.macula.realm.verify_api_key">>, _},
        macula_mesh_client:unadvertise(self(),
            <<"io.macula.realm.verify_api_key">>)).

call_4_rejects_dot_form_test() ->
    ?assertError({invalid_topic, <<"io.macula.realm.join_with_token">>, _},
        macula_mesh_client:call(self(),
            <<"io.macula.realm.join_with_token">>, #{}, 1000)).

call_5_rejects_dot_form_test() ->
    ?assertError({invalid_topic, <<"io.macula.realm.get_shared_key">>, _},
        macula_mesh_client:call(self(),
            <<"io.macula.realm.get_shared_key">>, #{}, 1000, #{})).

%%====================================================================
%% Streaming entry points
%%====================================================================

advertise_stream_3_rejects_dot_form_test() ->
    ?assertError({invalid_topic, ?BAD_DOT_TOPIC, _},
        macula_mesh_client:advertise_stream(self(), ?BAD_DOT_TOPIC,
            fun(_StreamPid, _Init) -> ok end)).

advertise_stream_4_rejects_dot_form_test() ->
    ?assertError({invalid_topic, ?BAD_DOT_TOPIC, _},
        macula_mesh_client:advertise_stream(self(), ?BAD_DOT_TOPIC,
            server_stream, fun(_StreamPid, _Init) -> ok end)).

call_stream_4_rejects_dot_form_test() ->
    ?assertError({invalid_topic, ?BAD_DOT_TOPIC, _},
        macula_mesh_client:call_stream(self(), ?BAD_DOT_TOPIC, term, #{})).

call_stream_5_rejects_dot_form_test() ->
    ?assertError({invalid_topic, ?BAD_DOT_TOPIC, _},
        macula_mesh_client:call_stream(self(), ?BAD_DOT_TOPIC, term, #{}, 1000)).

open_stream_4_rejects_dot_form_test() ->
    ?assertError({invalid_topic, ?BAD_DOT_TOPIC, _},
        macula_mesh_client:open_stream(self(), ?BAD_DOT_TOPIC, term, #{})).

open_stream_5_rejects_dot_form_test() ->
    ?assertError({invalid_topic, ?BAD_DOT_TOPIC, _},
        macula_mesh_client:open_stream(self(), ?BAD_DOT_TOPIC, term, #{}, 1000)).

%%====================================================================
%% Sentinel-mismatch rejection — these are valid 5-segment shapes
%% but tier-illegal (e.g. _realm in only one publisher slot).
%%====================================================================

publish_rejects_partial_realm_sentinel_test() ->
    Bad = <<"io.macula/_realm/hecate/membership/revoked_v1">>,
    ?assertError({invalid_topic, Bad, _},
        macula_mesh_client:publish(self(), Bad, #{})).

publish_rejects_misplaced_org_sentinel_test() ->
    Bad = <<"io.macula/_org/hecate/licenses/issued_batch_v1">>,
    ?assertError({invalid_topic, Bad, _},
        macula_mesh_client:publish(self(), Bad, #{})).

%%====================================================================
%% System topics are exempt — _mesh.* must NOT be rejected
%%====================================================================
%% These cannot be tested against a real cast/call without a running
%% mesh_client (validation passes → cast proceeds → no recipient).
%% We assert validation passes by calling macula_topic:validate/1
%% directly here; the integration is structurally identical.

system_topic_passes_validation_test() ->
    ok = macula_topic:validate(?SYSTEM_TOPIC).

good_realm_topic_passes_validation_test() ->
    ok = macula_topic:validate(?GOOD_REALM).

good_app_topic_passes_validation_test() ->
    ok = macula_topic:validate(?GOOD_APP).
