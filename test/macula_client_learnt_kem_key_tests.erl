%% A KEM key a link learnt from a provider's own signed `sealed_required' refusal, written onto the candidate the pool
%% remembered for that provider (DESIGN_E2E_SEALED_BY_DEFAULT §3, release A0). Two properties are what make it safe and
%% useful, and neither is visible by inspection: it lands only on that provider's candidate, so it never seals a call
%% to another provider, and it lives exactly as long as the candidate it was written on, so a refreshed candidate of
%% the same provider carries it and one of another provider does not. Both are driven here with explicit readings.
-module(macula_client_learnt_kem_key_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<1:256>>).
-define(PROC, <<"io.macula.echo">>).
-define(PROVIDER, <<3:256>>).
-define(OTHER_PROVIDER, <<5:256>>).
-define(STATION, <<2:256>>).
-define(KEM_KEY, <<7:12544>>).

%% The learnt key is written onto the candidate of the provider it came from, and the entry keeps its horizon.
a_learnt_key_is_written_onto_its_providers_candidate_test() ->
    Entry = remembered_entry(?PROVIDER, 1_000),
    Learnt = macula_client:learnt(key(), ?PROVIDER, ?KEM_KEY, #{key() => Entry}),
    ?assertEqual(learnt_candidate(?PROVIDER), candidate_of(Learnt)),
    ?assertEqual(1_000, maps:get(until_mono, entry(Learnt))).

%% A key for another provider leaves the entry exactly as it was: a key names a provider, and the entry answers for
%% another one.
a_learnt_key_of_another_provider_leaves_the_candidate_alone_test() ->
    Entry = remembered_entry(?PROVIDER, 1_000),
    ?assertEqual(#{key() => Entry}, macula_client:learnt(key(), ?OTHER_PROVIDER, ?KEM_KEY, #{key() => Entry})).

%% With no candidate remembered, there is nowhere to write it and nothing it could save.
a_learnt_key_with_no_remembered_candidate_writes_nothing_test() ->
    ?assertEqual(#{}, macula_client:learnt(key(), ?PROVIDER, ?KEM_KEY, #{})).

%% A refreshed candidate of the same provider carries the learnt key over: a refusal and the answer that followed it
%% reach the pool in either order, and the key stays in force for the advertisement's remaining lifetime.
a_refreshed_candidate_carries_the_learnt_key_test() ->
    Learnt = macula_client:learnt(key(), ?PROVIDER, ?KEM_KEY, #{key() => remembered_entry(?PROVIDER, 1_000)}),
    Refreshed = macula_client:remembered(key(), candidate(?PROVIDER), 2_000, 500, Learnt),
    ?assertEqual(learnt_candidate(?PROVIDER), candidate_of(Refreshed)),
    ?assertEqual(2_000, maps:get(until_mono, entry(Refreshed))).

%% A refreshed candidate of another provider does not: the key answered a question about the old one.
a_refreshed_candidate_of_another_provider_does_not_test() ->
    Learnt = macula_client:learnt(key(), ?PROVIDER, ?KEM_KEY, #{key() => remembered_entry(?PROVIDER, 1_000)}),
    Refreshed = macula_client:remembered(key(), candidate(?OTHER_PROVIDER), 2_000, 500, Learnt),
    ?assertEqual(candidate(?OTHER_PROVIDER), candidate_of(Refreshed)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

key() -> {?REALM, ?PROC}.

candidate(Provider) ->
    #{provider => Provider, version => <<7:128>>, station => ?STATION}.

learnt_candidate(Provider) ->
    (candidate(Provider))#{learnt_kem_key => ?KEM_KEY}.

remembered_entry(Provider, UntilMono) ->
    #{candidate => candidate(Provider), until_mono => UntilMono}.

entry(Resolved) ->
    maps:get(key(), Resolved).

candidate_of(Resolved) ->
    maps:get(candidate, entry(Resolved)).
