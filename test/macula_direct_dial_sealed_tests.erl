%% `macula:call/5,6' seals from the advertisement it resolved (E2E design
%% §8.1, Amendment A1): each candidate hands its verified advertisement to
%% the station call, which seals to its KEM key. A `sealed_refused' naming the
%% provider's current key is followed by ONE fresh lookup. The call is sealed
%% again only if the provider's re-resolved advertisement names exactly that
%% key; otherwise it fails naming both ids, and it is never sent in the clear.
-module(macula_direct_dial_sealed_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<0:256>>).

sealed_test_() ->
    {foreach, fun setup/0, fun(_) -> ok end,
     [{Name, fun() -> Case() end} || {Name, Case} <- cases()]}.

cases() ->
    [{"the call carries the resolved advertisement", fun the_call_carries_the_advertisement/0},
     {"a sealed_refused is followed by one re-resolve and one more call", fun one_reseal_after_a_refusal/0},
     {"a re-resolved advertisement naming another key fails naming both", fun another_key_fails_naming_both/0},
     {"a second refusal is the result: no third call", fun no_third_call/0},
     {"a re-resolve finding no key fails closed", fun a_keyless_reresolve_fails_closed/0},
     {"a provider that holds no key fails closed", fun a_provider_without_a_key_fails_closed/0},
     {"a provider that lost its key is re-resolved once", fun a_provider_that_lost_its_key_is_resealed/0},
     {"a required call passes its policy to the station call", fun required_reaches_the_station_call/0},
     {"a stream carries the resolved advertisement", fun a_stream_carries_the_advertisement/0},
     {"a stream passes its policy to the station open", fun a_streams_policy_reaches_the_station_open/0},
     {"a stream's reseal re-resolves once, bound to the key the refusal names",
      fun a_streams_reseal_is_bound_to_the_named_key/0},
     {"a stream's reseal takes the provider's advertisement that names the key, not the first",
      fun a_streams_reseal_finds_the_named_key_among_the_providers_advertisements/0}].

the_call_carries_the_advertisement() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))]], [{ok, <<"pong">>}]),
    ?assertEqual({ok, <<"pong">>}, call(F, #{})),
    [Opts] = calls(),
    ?assertEqual(kem(1), maps:get(kem_key, macula_record:read_procedure_advertisement(maps:get(advertisement, Opts)))).

one_reseal_after_a_refusal() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))], [keyed_ad(F, kem(2))]], [{error, {sealed_refused, kem_id(2)}}, {ok, <<"pong">>}]),
    ?assertEqual({ok, <<"pong">>}, call(F, #{})),
    [_First, Second] = calls(),
    ?assertEqual(kem(2), maps:get(kem_key, macula_record:read_procedure_advertisement(maps:get(advertisement, Second)))).

another_key_fails_naming_both() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))], [keyed_ad(F, kem(3))]], [{error, {sealed_refused, kem_id(2)}}]),
    ?assertEqual({error, {confidentiality, {key_mismatch, kem_id(2), kem_id(3)}}}, call(F, #{})),
    ?assertEqual(1, length(calls())).

no_third_call() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))], [keyed_ad(F, kem(2))]],
           [{error, {sealed_refused, kem_id(2)}}, {error, {sealed_refused, kem_id(4)}}]),
    ?assertEqual({error, {sealed_refused, kem_id(4)}}, call(F, #{})),
    ?assertEqual(2, length(calls())).

a_keyless_reresolve_fails_closed() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))], [keyless_ad(F)]], [{error, {sealed_refused, kem_id(2)}}]),
    ?assertEqual({error, {confidentiality, no_kem_key}}, call(F, #{})),
    ?assertEqual(1, length(calls())).

a_provider_without_a_key_fails_closed() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))], [keyless_ad(F)]], [{error, {sealed_refused, no_key}}]),
    ?assertEqual({error, {confidentiality, no_kem_key}}, call(F, #{})),
    ?assertEqual(1, length(calls())).

%% A provider whose keyring was lost answers no_key (signed, for this
%% request); its fresh advertisement names a new key, which is sealed to once.
a_provider_that_lost_its_key_is_resealed() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))], [keyed_ad(F, kem(5))]], [{error, {sealed_refused, no_key}}, {ok, <<"pong">>}]),
    ?assertEqual({ok, <<"pong">>}, call(F, #{})),
    [_First, Second] = calls(),
    ?assertEqual(kem(5), maps:get(kem_key, macula_record:read_procedure_advertisement(maps:get(advertisement, Second)))).

required_reaches_the_station_call() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))]], [{ok, <<"pong">>}]),
    ?assertEqual({ok, <<"pong">>}, call(F, #{confidential => required})),
    [Opts] = calls(),
    ?assertEqual(required, maps:get(confidential, Opts)).

%% A stream is opened on the same terms: the station open is handed the
%% candidate's verified advertisement to seal from.
a_stream_carries_the_advertisement() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))]], [{ok, self()}]),
    ?assertEqual({ok, self()}, stream(F, #{mode => bidi})),
    [Opts] = calls(),
    ?assertEqual(kem(1), maps:get(kem_key, macula_record:read_procedure_advertisement(maps:get(advertisement, Opts)))),
    ?assertEqual(bidi, maps:get(mode, Opts)).

a_streams_policy_reaches_the_station_open() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))]], [{ok, self()}]),
    ?assertEqual({ok, self()}, stream(F, #{confidential => required})),
    [Opts] = calls(),
    ?assertEqual(required, maps:get(confidential, Opts)).

%% A stream's refused open is resealed by the stream itself (it was opened
%% before the refusal arrived), with the reseal direct dial hands it: ONE fresh
%% lookup, and only the key the refusal named. Another key fails naming both,
%% none (or a provider that names no key) fails closed.
a_streams_reseal_is_bound_to_the_named_key() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))], [keyed_ad(F, kem(2))], [keyed_ad(F, kem(3))], [keyless_ad(F)]],
           [{ok, self()}]),
    ?assertEqual({ok, self()}, stream(F, #{})),
    [Opts] = calls(),
    Reseal = maps:get(reseal, Opts),
    ?assertEqual({ok, kem(2)}, Reseal(kem_id(2))),
    ?assertEqual({error, {confidentiality, {key_mismatch, kem_id(2), kem_id(3)}}}, Reseal(kem_id(2))),
    ?assertEqual({error, {confidentiality, no_kem_key}}, Reseal(kem_id(2))),
    ?assertEqual({error, {confidentiality, no_kem_key}}, Reseal(no_key)).

%% While a provider rotates, the DHT can still serve its previous
%% advertisement beside the new one: the reseal takes the one naming the key
%% the refusal named, wherever it comes in the lookup.
a_streams_reseal_finds_the_named_key_among_the_providers_advertisements() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))], [keyed_ad(F, kem(3)), keyed_ad(F, kem(2))]], [{ok, self()}]),
    ?assertEqual({ok, self()}, stream(F, #{})),
    [Opts] = calls(),
    ?assertEqual({ok, kem(2)}, (maps:get(reseal, Opts))(kem_id(2))).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

setup() ->
    _ = application:load(macula),
    erase(),
    ok.

%% A provider, the station it serves from, and its own-namespace procedure.
fixture() ->
    {ok, Provider} = macula_node_keys:generate(identity, profile()),
    {ok, Station} = macula_node_keys:generate(identity, profile()),
    ProviderId = macula_node_keys:key_id(Provider),
    #{provider => Provider, provider_id => ProviderId, station => Station,
      station_id => macula_node_keys:key_id(Station),
      procedure => <<"~", (binary:encode_hex(ProviderId, lowercase))/binary, "/echo">>}.

kem(N) ->
    Key = get({kem, N}),
    kem_or_new(Key, N).

kem_or_new(undefined, N) ->
    {Public, _} = macula_seal:generate_key(profile()),
    Carried = macula_seal:key_as_carried(Public),
    put({kem, N}, Carried),
    Carried;
kem_or_new(Carried, _N) ->
    Carried.

kem_id(N) -> macula_seal:key_id(kem(N)).

keyed_ad(#{provider := Key, provider_id := Id, station_id := Station, procedure := Proc}, KemKey) ->
    verified(macula_record:sign(macula_record:procedure_advertisement(Id, ?REALM, Proc, Station, #{kem_key => KemKey}),
                                Key)).

keyless_ad(#{provider := Key, provider_id := Id, station_id := Station, procedure := Proc}) ->
    verified(macula_record:sign(macula_record:procedure_advertisement(Id, ?REALM, Proc, Station, #{}), Key)).

endpoint(#{station := Key}) ->
    verified(macula_record:sign(macula_record:station_endpoint(4433, #{host_advertised => [<<"::1">>]}), Key)).

verified(Signed) ->
    {ok, Verified} = macula_record:verify(macula_record:encode(Signed), profile()),
    Verified.

%% Each lookup of the procedure answers the next list of advertisements, and
%% each station call the next answer.
script(F, Lookups, Answers) ->
    put(fixture, F),
    put(lookups, Lookups),
    put(answers, Answers),
    put(calls, []).

calls() -> lists:reverse(get(calls)).

call(#{procedure := Proc}, Opts) ->
    macula_direct_dial:call(fake_pool(), ?REALM, Proc, <<"ping">>, 3_000, Opts#{dial_io => dial_io()}).

%% A pool that pins no realm key: an own-namespace procedure needs none.
fake_pool() ->
    spawn(fun FakePool() ->
              receive {'$gen_call', From, {realm_key, _Realm}} -> gen_server:reply(From, none), FakePool()
              after 10_000 -> ok
              end
          end).

stream(#{procedure := Proc}, StreamOpts) ->
    macula_direct_dial:call_stream(fake_pool(), ?REALM, Proc, #{}, StreamOpts, #{dial_io => dial_io()}).

dial_io() ->
    #{find_records => fun(_Pool, _Key, _TimeoutMs) -> next(lookups, {ok, []}) end,
      call_stream_station => fun(_Pool, _DialUrl, _Provider, _Realm, _Proc, _Args, Opts) ->
                                 put(calls, [Opts | get(calls)]),
                                 next(answers, {error, no_more_answers})
                             end,
      find_record => fun(_Pool, _Key, _TimeoutMs) -> {ok, endpoint(get(fixture))} end,
      call_station => fun(_Pool, _DialUrl, _Provider, _Realm, _Proc, _Payload, _TimeoutMs, Opts) ->
                          put(calls, [Opts | get(calls)]),
                          next(answers, {error, no_more_answers})
                      end,
      resolved_candidate => fun(_Pool, _Realm, _Proc) -> none end,
      remember_resolved => fun(_Pool, _Realm, _Proc, _Candidate, _TtlMs) -> ok end}.

next(Key, Default) ->
    next_of(get(Key), Key, Default).

next_of([Next | Rest], Key, _Default) -> put(Key, Rest), wrapped(Key, Next);
next_of(_Empty, _Key, Default) -> Default.

wrapped(lookups, Records) -> {ok, Records};
wrapped(answers, Answer) -> Answer.

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.
