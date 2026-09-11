%% EUnit tests for the record part of macula_direct_dial's publish and resolve paths. A provider's advertisement is
%% signed with its node identity key and put as its wire form. A caller resolves a procedure through the verified
%% advertisements under the procedure's storage key, trusts only those that pass advertisement_trusted/2, and dials a
%% station only through an endpoint record the station itself signed. The facade is mocked at macula:links/1,
%% macula:find_records/2, macula:find_record/2 and macula:call_station/7, and the pool's RPC at macula_client:call/5.
-module(macula_direct_dial_resolve_tests).

-include_lib("eunit/include/eunit.hrl").

-define(EU_TIMEOUT, 120).
-define(REALM, <<16#11:256>>).
-define(PROCEDURE, <<"forecast_v1">>).

resolve_test_() ->
    {timeout, ?EU_TIMEOUT, {setup, fun start/0, fun stop/1, fun cases/1}}.

%% Every case runs on its own, so each passes or fails by itself.
cases(Keys) ->
    [{case_name(Case), fun() -> Case(Keys) end}
     || Case <- [fun a_published_advertisement_is_signed_by_the_node_identity_and_put_as_its_wire_form/1,
                 fun a_call_dials_the_station_a_trusted_advertisement_names/1,
                 fun an_endpoint_signed_by_another_node_is_not_dialed/1,
                 fun an_org_namespaced_procedure_without_an_authorization_resolves_to_nothing/1]].

a_published_advertisement_is_signed_by_the_node_identity_and_put_as_its_wire_form(Keys) ->
    #{provider := Provider, station := Station, profile := Profile} = Keys,
    StationId = macula_node_keys:key_id(Station),
    Test = self(),
    ok = meck:expect(macula, links, fun(_Pool) -> {ok, [#{connected => true, node_id => StationId}]} end),
    ok = meck:expect(macula_client, call,
                     fun(_Pool, _Realm, Procedure, Payload, _TimeoutMs) ->
                         Test ! {called, Procedure, Payload},
                         {ok, ok}
                     end),
    ?assertEqual(ok, macula_direct_dial:publish_advertisement(self(), ?REALM, ?PROCEDURE, Provider)),
    {<<"_dht.put_record">>, Wire} = receive {called, P, W} -> {P, W} after 1000 -> erlang:error(not_put) end,
    {ok, Verified} = macula_record:verify(Wire, Profile),
    ?assertEqual(#{realm_id => ?REALM, procedure => ?PROCEDURE, advertiser_node => macula_node_keys:key_id(Provider),
                   serving_station => StationId, authorization => undefined},
                 macula_record:read_procedure_advertisement(Verified)).

a_call_dials_the_station_a_trusted_advertisement_names(Keys) ->
    #{provider := Provider, station := Station, profile := Profile} = Keys,
    StationId = macula_node_keys:key_id(Station),
    Test = self(),
    ok = meck:expect(macula, find_records,
                     fun(_Pool, Key) ->
                         Test ! {find_records, Key},
                         {ok, [advertisement(Provider, StationId, ?PROCEDURE, Profile)]}
                     end),
    ok = meck:expect(macula, find_record,
                     fun(_Pool, Key) ->
                         Test ! {find_record, Key},
                         {ok, endpoint(Station, Profile)}
                     end),
    ok = meck:expect(macula, call_station,
                     fun(_Pool, DialUrl, _Realm, _Procedure, _Payload, _TimeoutMs, Opts) ->
                         Test ! {dialed, DialUrl, Opts},
                         {ok, answered}
                     end),
    ?assertEqual({ok, answered}, macula_direct_dial:call(self(), ?REALM, ?PROCEDURE, <<"payload">>, 1000)),
    ?assertEqual(macula_record:procedure_key(?REALM, ?PROCEDURE), receive {find_records, K1} -> K1 after 0 -> none end),
    ?assertEqual(macula_record:station_endpoint_key(StationId), receive {find_record, K2} -> K2 after 0 -> none end),
    ?assertMatch({<<"quic://[::1]:4433">>, #{expected_node_id := StationId}},
                 receive {dialed, Url, Opts} -> {Url, Opts} after 0 -> none end).

an_endpoint_signed_by_another_node_is_not_dialed(Keys) ->
    #{provider := Provider, station := Station, other := Other, profile := Profile} = Keys,
    StationId = macula_node_keys:key_id(Station),
    ok = meck:expect(macula, find_records,
                     fun(_Pool, _Key) -> {ok, [advertisement(Provider, StationId, ?PROCEDURE, Profile)]} end),
    ok = meck:expect(macula, find_record, fun(_Pool, _Key) -> {ok, endpoint(Other, Profile)} end),
    ok = meck:expect(macula, call_station, fun(_, _, _, _, _, _, _) -> erlang:error(dialed) end),
    ?assertEqual({error, {unresolved, station_endpoint_signer_mismatch}},
                 macula_direct_dial:call(self(), ?REALM, ?PROCEDURE, <<"payload">>, 1000)).

an_org_namespaced_procedure_without_an_authorization_resolves_to_nothing(Keys) ->
    #{provider := Provider, station := Station, profile := Profile} = Keys,
    Procedure = <<"acme/forecast_v1">>,
    StationId = macula_node_keys:key_id(Station),
    ok = meck:expect(macula, find_records,
                     fun(_Pool, _Key) -> {ok, [advertisement(Provider, StationId, Procedure, Profile)]} end),
    ok = meck:expect(macula, call_station, fun(_, _, _, _, _, _, _) -> erlang:error(dialed) end),
    ?assertEqual({error, {unresolved, no_trusted_advertisement}},
                 macula_direct_dial:call(self(), ?REALM, Procedure, <<"payload">>, 1000)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

start() ->
    {ok, _} = application:ensure_all_started(macula),
    {ok, Profile} = macula_crypto_profile:configured(),
    Keys = #{profile => Profile, provider => key(Profile), station => key(Profile), other => key(Profile)},
    ok = meck:new(macula, [passthrough]),
    ok = meck:new(macula_client, [passthrough]),
    Keys.

stop(_Keys) ->
    meck:unload([macula, macula_client]).

case_name(Case) ->
    {name, Name} = erlang:fun_info(Case, name),
    atom_to_list(Name).

key(Profile) ->
    {ok, Key} = macula_node_keys:generate(identity, Profile),
    Key.

%% A record as the facade hands it over: verified under the node's profile.
verified(Signed, Profile) ->
    {ok, Verified} = macula_record:verify(macula_record:encode(Signed), Profile),
    Verified.

advertisement(Provider, StationId, Procedure, Profile) ->
    Unsigned = macula_record:procedure_advertisement(macula_node_keys:key_id(Provider), ?REALM, Procedure, StationId),
    verified(macula_record:sign(Unsigned, Provider), Profile).

endpoint(Signer, Profile) ->
    Unsigned = macula_record:station_endpoint(4433, #{host_advertised => [<<"::1">>]}),
    verified(macula_record:sign(Unsigned, Signer), Profile).
