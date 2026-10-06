%% EUnit tests for macula:resolve_app/2,3 (macula#75): app MRI, then the app record under its key, trusted through the
%% org directory it carries against the realm key the pool pinned, then each service's procedures through the provider
%% lookup a call uses. The DHT, the provider lookup and the pinned realm key are stubbed through
%% macula_app_resolve's resolve_io seam; the records are real.
-module(macula_app_resolve_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM_NAME, <<"io.example">>).
-define(MRI, <<"mri:app:io.example/acme/weather">>).
-define(MINUTE, 60000).
-define(HOUR, 3600000).

resolve_app_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun(F) -> {Title, fun() -> Test(F) end} end
      || {Title, Test} <- [{"an app resolves to its services and the providers of each procedure", fun resolves/1},
                           {"a procedure nobody serves is listed with no providers and the reason",
                            fun unserved_procedure/1},
                           {"no record under the MRI is app_not_registered", fun not_registered/1},
                           {"a record no pinned realm key vouches for is not trusted, with the refusal",
                            fun untrusted/1},
                           {"of several trusted records the newest is used", fun newest_wins/1},
                           {"a record in the slot for another app is not this app", fun another_app_in_the_slot/1},
                           {"anything but an app MRI is refused before any lookup", fun not_an_app_mri/1}]]}.

setup() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Realm = key(realm, Profile),
    Org = key(org, Profile),
    #{profile => Profile, realm => Realm, org => Org}.

teardown(_Fixture) ->
    ok.

%% The seam: the pinned realm key, the providers, and a DHT that answers the app slot with `Records' and tells
%% the test which key it was asked for.
io(#{realm := Realm}, Records) ->
    Test = self(),
    #{realm_key => fun(_Pool, RealmId) -> pinned(RealmId =:= realm_id(), macula_node_keys:public_key(Realm)) end,
      providers => fun(_Pool, _RealmId, Procedure, _TimeoutMs) -> serving(Procedure) end,
      find_records => fun(_Pool, Key, _TimeoutMs) -> Test ! {looked_up, Key}, Records end}.

resolve(F, Records) ->
    macula_app_resolve:resolve(self(), ?MRI, 5000, io(F, Records)).

pinned(true, Key) -> {ok, Key};
pinned(false, _Key) -> none.

serving(<<"acme/get_forecast_v1">>) -> {ok, [#{provider => fill(1), station => fill(2)}]};
serving(_Other) -> {error, {unresolved, procedure_not_advertised}}.

%%------------------------------------------------------------------

resolves(F) ->
    {ok, #{app := App, services := [Forecast, _Alerts]}} = resolve(F, stored(F, [app(F, <<"1.4.0">>, #{})])),
    ?assertEqual(#{realm_id => realm_id(), org_name => <<"acme">>, app_name => <<"weather">>, version => <<"1.4.0">>},
                 App),
    ?assertMatch(#{name := <<"forecast">>,
                   procedures := [#{procedure := <<"acme/get_forecast_v1">>, providers := [#{provider := _}]} | _]},
                 Forecast),
    ?assertEqual([macula_record:app_key(realm_id(), <<"acme">>, <<"weather">>)], looked_up()).

unserved_procedure(F) ->
    {ok, #{services := [#{procedures := [_, Watch]}, Alerts]}} = resolve(F, stored(F, [app(F, <<"1.4.0">>, #{})])),
    ?assertEqual(#{procedure => <<"acme/watch_forecast_v1">>, providers => [], unresolved => procedure_not_advertised},
                 Watch),
    ?assertEqual(#{name => <<"alerts">>, procedures => []}, Alerts).

not_registered(F) ->
    ?assertEqual({error, {unresolved, app_not_registered}}, resolve(F, {error, not_found})).

untrusted(F) ->
    #{profile := Profile} = F,
    ?assertEqual({error, {unresolved, {no_trusted_app_record, [org_directory_wrong_realm]}}},
                 resolve(F, stored(F, [app(F, <<"1.4.0">>, #{dir_signer => key(realm, Profile)})]))).

newest_wins(F) ->
    Older = app(F, <<"1.4.0">>, #{}),
    timer:sleep(5),
    Newer = app(F, <<"1.5.0">>, #{}),
    ?assertMatch({ok, #{app := #{version := <<"1.5.0">>}}}, resolve(F, stored(F, [Newer, Older]))).

another_app_in_the_slot(F) ->
    ?assertEqual({error, {unresolved, app_not_registered}},
                 resolve(F, stored(F, [app(F, <<"1.4.0">>, #{app_name => <<"tides">>})]))).

not_an_app_mri(F) ->
    Io = io(F, {error, not_found}),
    ?assertMatch({error, {invalid_app_mri, _}}, macula:resolve_app(self(), <<"not an mri">>)),
    ?assertEqual({error, {invalid_app_mri, not_an_app}},
                 macula_app_resolve:resolve(self(), <<"mri:org:io.example/acme">>, 5000, Io)),
    ?assertEqual([], looked_up()).

%%------------------------------------------------------------------

%% The DHT's answer for the slot: these records, verified as macula:find_records/3 hands them back.
stored(#{profile := Profile}, Records) ->
    {ok, [begin {ok, V} = macula_record:verify(macula_record:encode(R), Profile), V end || R <- Records]}.

%% The keys the DHT was asked for, in order.
looked_up() ->
    receive {looked_up, Key} -> [Key | looked_up()] after 0 -> [] end.

app(#{realm := Realm, org := Org}, Version, Overrides) ->
    Dir = macula_record:sign(macula_record:org_directory(realm_id(), <<"acme">>, macula_node_keys:key_id(Org),
                                                         #{ttl_ms => 6 * ?HOUR}),
                             maps:get(dir_signer, Overrides, Realm)),
    macula_record:sign(
      macula_record:app_record(macula_node_keys:key_id(Org), realm_id(), <<"acme">>,
                               maps:get(app_name, Overrides, <<"weather">>),
                               #{version => Version, org_directory => macula_record:encode(Dir), ttl_ms => 30 * ?MINUTE,
                                 services => [#{name => <<"forecast">>,
                                                procedures => [<<"acme/get_forecast_v1">>, <<"acme/watch_forecast_v1">>]},
                                              #{name => <<"alerts">>, procedures => []}]}),
      Org).

key(Purpose, Profile) ->
    {ok, Key} = macula_node_keys:generate(Purpose, Profile),
    Key.

realm_id() ->
    macula_realm:id(?REALM_NAME).

fill(Byte) ->
    binary:copy(<<Byte>>, 32).
