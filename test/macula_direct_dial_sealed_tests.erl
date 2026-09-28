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
      fun a_streams_reseal_finds_the_named_key_among_the_providers_advertisements/0},
     {"a stream whose provider lost its key reseals to any key its fresh advertisements name",
      fun a_streams_reseal_after_a_lost_key_takes_any_named_key/0},
     {"a call's reseal takes the provider's advertisement that names the key, not the first",
      fun a_calls_reseal_finds_the_named_key_among_the_providers_advertisements/0},
     {"a call whose provider lost its key reseals to the first advertisement that names any key",
      fun a_calls_reseal_after_a_lost_key_skips_a_keyless_advertisement/0},
     {"a call refuses confidential => off before any lookup", fun a_call_refuses_off/0},
     {"a stream refuses confidential => off before any lookup", fun a_stream_refuses_off/0},
     {"a confidential that names no policy is an invalid option", fun an_unknown_confidential_is_invalid/0},
     {"a call asking for its report passes it down and returns the station call's report",
      fun a_reported_call_returns_the_station_calls_report/0},
     {"a call not asking for its report passes none down", fun an_unreported_call_passes_no_report/0},
     {"a resealed call returns the second call's report", fun a_resealed_call_returns_the_reseals_report/0},
     {"a reported answer is remembered as an answer", fun a_reported_answer_is_remembered/0},
     {"a report that is not a boolean is an invalid option", fun a_non_boolean_report_is_invalid/0},
     {"a call's ucan_token reaches the station call", fun a_calls_ucan_token_reaches_the_station_call/0},
     {"a call without a ucan_token passes none down", fun a_call_without_a_ucan_token_passes_none/0},
     {"a ucan_token that is not bytes is an invalid option", fun a_non_binary_ucan_token_is_invalid/0}].

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

%% A lookup never downgrades a call (§8.1), so direct dial cannot send in the clear: `confidential => off' is the
%% application's decision for a target it names itself (`call_station/8'). Accepted here it would do nothing, since
%% the call is sealed to the advertisement it resolves, so it is refused by name before anything is looked up.
a_call_refuses_off() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))]], [{ok, <<"pong">>}]),
    ?assertEqual({error, {confidentiality, off_needs_explicit_target}}, call(F, #{confidential => off})),
    ?assertEqual([], calls()),
    ?assertEqual([[keyed_ad_marker]], lookups_left()).

a_stream_refuses_off() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))]], [{ok, self()}]),
    ?assertEqual({error, {confidentiality, off_needs_explicit_target}}, stream(F, #{confidential => off})),
    ?assertEqual([], calls()),
    ?assertEqual([[keyed_ad_marker]], lookups_left()).

an_unknown_confidential_is_invalid() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))]], [{ok, <<"pong">>}, {ok, self()}]),
    ?assertEqual({error, {invalid_option, confidential}}, call(F, #{confidential => sometimes})),
    ?assertEqual({error, {invalid_option, confidential}}, stream(F, #{confidential => sometimes})),
    ?assertEqual([], calls()),
    ?assertEqual([[keyed_ad_marker]], lookups_left()).

%% DESIGN_E2E_SEAL_REPORT §4: `report => true' rides down to the station call, which builds the report where the
%% answer is opened; direct dial returns it as it is.
a_reported_call_returns_the_station_calls_report() ->
    F = fixture(),
    Report = #{sealed => 1, provider => maps:get(provider_id, F), seal_key_id => kem_id(1)},
    script(F, [[keyed_ad(F, kem(1))]], [{ok, <<"pong">>, Report}]),
    ?assertEqual({ok, <<"pong">>, Report}, call(F, #{report => true})),
    [Opts] = calls(),
    ?assertEqual(true, maps:get(report, Opts)).

an_unreported_call_passes_no_report() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))]], [{ok, <<"pong">>}]),
    ?assertEqual({ok, <<"pong">>}, call(F, #{report => false})),
    [Opts] = calls(),
    ?assertNot(is_map_key(report, Opts)).

%% The report describes the exchange that produced the result: after a reseal, the second call's (§3).
a_resealed_call_returns_the_reseals_report() ->
    F = fixture(),
    Second = #{sealed => 1, provider => maps:get(provider_id, F), seal_key_id => kem_id(2)},
    script(F, [[keyed_ad(F, kem(1))], [keyed_ad(F, kem(2))]],
           [{error, {sealed_refused, kem_id(2)}}, {ok, <<"pong">>, Second}]),
    ?assertEqual({ok, <<"pong">>, Second}, call(F, #{report => true})),
    [First, Resealed] = calls(),
    ?assertEqual(true, maps:get(report, First)),
    ?assertEqual(true, maps:get(report, Resealed)).

a_reported_answer_is_remembered() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))]], [{ok, <<"pong">>, #{sealed => 1, provider => maps:get(provider_id, F),
                                                           seal_key_id => kem_id(1)}}]),
    {ok, <<"pong">>, _Report} = call(F, #{report => true}),
    ?assertEqual(1, length(get(remembered))).

a_non_boolean_report_is_invalid() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))]], [{ok, <<"pong">>}]),
    ?assertEqual({error, {invalid_option, report}}, call(F, #{report => yes})),
    ?assertEqual([], calls()),
    ?assertEqual([[keyed_ad_marker]], lookups_left()).

%% The lookups a script still holds, each record shown as a marker: none was taken.
lookups_left() ->
    [[keyed_ad_marker || _ <- Records] || Records <- get(lookups)].

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
%% none fails closed. (A provider that holds no key is re-resolved once, as a
%% call is: see the lost-key case.)
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

a_streams_reseal_after_a_lost_key_takes_any_named_key() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))], [keyless_ad(F), keyed_ad(F, kem(5))]], [{ok, self()}]),
    ?assertEqual({ok, self()}, stream(F, #{})),
    [Opts] = calls(),
    ?assertEqual({ok, kem(5)}, (maps:get(reseal, Opts))(no_key)).

%% A call is resealed on the same pick as a stream: the provider's
%% advertisement naming the refused key, wherever the lookup gives it.
a_calls_reseal_finds_the_named_key_among_the_providers_advertisements() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))], [keyed_ad(F, kem(3)), keyed_ad(F, kem(2))]],
           [{error, {sealed_refused, kem_id(2)}}, {ok, <<"pong">>}]),
    ?assertEqual({ok, <<"pong">>}, call(F, #{})),
    [_First, Second] = calls(),
    ?assertEqual(kem(2), maps:get(kem_key, macula_record:read_procedure_advertisement(maps:get(advertisement, Second)))).

a_calls_reseal_after_a_lost_key_skips_a_keyless_advertisement() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))], [keyless_ad(F), keyed_ad(F, kem(5))]],
           [{error, {sealed_refused, no_key}}, {ok, <<"pong">>}]),
    ?assertEqual({ok, <<"pong">>}, call(F, #{})),
    [_First, Second] = calls(),
    ?assertEqual(kem(5), maps:get(kem_key, macula_record:read_procedure_advertisement(maps:get(advertisement, Second)))).

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
    put(calls, []),
    put(remembered, []).

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
      remember_resolved => fun(_Pool, _Realm, _Proc, Candidate, _TtlMs) ->
                               put(remembered, [Candidate | get(remembered)]),
                               ok
                           end}.

next(Key, Default) ->
    next_of(get(Key), Key, Default).

next_of([Next | Rest], Key, _Default) -> put(Key, Rest), wrapped(Key, Next);
next_of(_Empty, _Key, Default) -> Default.

wrapped(lookups, Records) -> {ok, Records};
wrapped(answers, Answer) -> Answer.

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.

%% A gated provider takes the caller's UCAN from the call's own token (the advertise policy checks it before the
%% handler runs), so a call direct dial resolves presents it as `call_station/8' does: to every candidate it tries,
%% the resealed call included.
a_calls_ucan_token_reaches_the_station_call() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))], [keyed_ad(F, kem(2))]], [{error, {sealed_refused, kem_id(2)}}, {ok, <<"pong">>}]),
    ?assertEqual({ok, <<"pong">>}, call(F, #{ucan_token => <<"token">>})),
    ?assertEqual([<<"token">>, <<"token">>], [maps:get(ucan_token, Opts) || Opts <- calls()]).

a_call_without_a_ucan_token_passes_none() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))]], [{ok, <<"pong">>}]),
    ?assertEqual({ok, <<"pong">>}, call(F, #{})),
    [Opts] = calls(),
    ?assertNot(maps:is_key(ucan_token, Opts)).

a_non_binary_ucan_token_is_invalid() ->
    F = fixture(),
    script(F, [[keyed_ad(F, kem(1))]], [{ok, <<"pong">>}]),
    ?assertEqual({error, {invalid_option, ucan_token}}, call(F, #{ucan_token => "token"})),
    ?assertEqual([], calls()),
    ?assertEqual([[keyed_ad_marker]], lookups_left()).
