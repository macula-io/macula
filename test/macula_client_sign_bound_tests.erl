%% EUnit tests for a record a pool signs about itself under a bound on its expiry. The pool stamps the record with its
%% own clock and ends it at the bound when the bound comes before the record's lifetime runs out, so the time a call
%% takes never carries a signed record past the bound. A bound already passed is refused by name, the refusals of
%% sign_node_record/2 stand under a bound, and the facade signs as the pool does.
-module(macula_client_sign_bound_tests).

-include_lib("eunit/include/eunit.hrl").

sign_bound_test_() ->
    {timeout, 60, {setup, fun started/0, fun stopped/1, fun cases/1}}.

started() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    {ok, NodeId} = macula_node_keys:node_id(Key),
    {ok, Pool} = macula_client:connect([], #{node_identity => Key}),
    #{pool => Pool, node_id => NodeId, profile => Profile}.

stopped(#{pool := Pool}) ->
    macula_client:close(Pool).

cases(World) ->
    [{"a bound after the record's lifetime keeps the lifetime it was built with", ?_test(bound_after_lifetime(World))},
     {"a bound within the record's lifetime ends the record at the bound", ?_test(bound_within_lifetime(World))},
     {"a bound already passed is refused by name", ?_test(bound_passed(World))},
     {"the refusals of sign_node_record/2 stand under a bound", ?_test(refusals_stand(World))},
     {"no bound signs as sign_node_record/2 does", ?_test(no_bound(World))},
     {"the facade signs under a bound as the pool does", ?_test(facade_signs_under_a_bound(World))},
     {"a bound that is not an integer raises in the caller", ?_test(bound_not_an_integer(World))}].

bound_after_lifetime(#{pool := Pool, node_id := NodeId, profile := Profile}) ->
    Built = advertisement(NodeId),
    Lifetime = lifetime(Built),
    Before = erlang:system_time(millisecond),
    {ok, Signed} = macula_client:sign_node_record(Pool, Built, #{not_after => Before + 10 * Lifetime}),
    ?assert(macula_record:created_at(Signed) >= Before),
    ?assertEqual(Lifetime, lifetime(Signed)),
    ?assertMatch({ok, #{key_id := NodeId}}, macula_record:verify(macula_record:encode(Signed), Profile)).

bound_within_lifetime(#{pool := Pool, node_id := NodeId, profile := Profile}) ->
    Built = advertisement(NodeId),
    NotAfter = erlang:system_time(millisecond) + lifetime(Built) div 2,
    {ok, Signed} = macula_client:sign_node_record(Pool, Built, #{not_after => NotAfter}),
    ?assertEqual(NotAfter, macula_record:expires_at(Signed)),
    ?assertMatch({ok, _}, macula_record:verify(macula_record:encode(Signed), Profile)).

bound_passed(#{pool := Pool, node_id := NodeId}) ->
    ?assertEqual({error, not_after_passed},
                 macula_client:sign_node_record(Pool, advertisement(NodeId),
                                                #{not_after => erlang:system_time(millisecond) - 1})).

refusals_stand(#{pool := Pool}) ->
    NotAfter = erlang:system_time(millisecond) + 60_000,
    ?assertEqual([{error, key_id_mismatch}, {error, not_a_node_signed_type}],
                 [macula_client:sign_node_record(Pool, Record, #{not_after => NotAfter})
                  || Record <- [macula_record:node_record(<<7:256>>, [], 0), macula_record:station_endpoint(4433)]]).

no_bound(#{pool := Pool, node_id := NodeId}) ->
    Built = advertisement(NodeId),
    {ok, Signed} = macula_client:sign_node_record(Pool, Built, #{}),
    ?assertEqual(lifetime(Built), lifetime(Signed)).

facade_signs_under_a_bound(#{pool := Pool, node_id := NodeId}) ->
    Built = advertisement(NodeId),
    NotAfter = erlang:system_time(millisecond) + lifetime(Built) div 2,
    {ok, Signed} = macula:sign_node_record(Pool, Built, #{not_after => NotAfter}),
    ?assertEqual(NotAfter, macula_record:expires_at(Signed)).

bound_not_an_integer(#{pool := Pool, node_id := NodeId}) ->
    ?assertError(function_clause, macula_client:sign_node_record(Pool, advertisement(NodeId), #{not_after => soon})).

advertisement(NodeId) ->
    macula_record:procedure_advertisement(NodeId, <<1:256>>, <<"acme/echo_v1">>, <<7:256>>).

lifetime(Record) ->
    macula_record:expires_at(Record) - macula_record:created_at(Record).
