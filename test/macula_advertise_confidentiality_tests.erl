%% Whether a provider's advertisements name its KEM key (E2E design §8.2,
%% Amendment A1): only once the node is switched on with `kem_advertise'
%% (after every station runs the release that stores a keyed advertisement),
%% and only when the advertise's `confidential' option is not `off'.
%% `required' also refuses every clear call, so while the switch is off it is
%% refused at advertise time: a provider that names no key and refuses clear
%% calls would be unreachable. The spec carries the decision, and a renewal
%% makes the same one.
-module(macula_advertise_confidentiality_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<9:256>>).

confidentiality_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{Name, fun() -> Case() end} || {Name, Case} <- cases()]}.

cases() ->
    [{"switched off, a provider names no key", fun switched_off_names_no_key/0},
     {"switched off, required is refused at advertise time", fun switched_off_refuses_required/0},
     {"switched on, a provider names its key by default", fun switched_on_names_the_key/0},
     {"switched on, off names no key", fun switched_on_off_names_no_key/0},
     {"switched on, required names the key and refuses clear calls", fun switched_on_required/0},
     {"a confidential value outside its set is refused", fun an_unknown_value_is_refused/0},
     {"a renewal makes the same decision", fun a_renewal_keeps_the_decision/0}].

switched_off_names_no_key() ->
    ?assertEqual(ok, advertise(#{})),
    Spec = advertised(),
    ?assertNot(is_map_key(kem, Spec)),
    ?assertNot(is_map_key(confidential, Spec)).

switched_off_refuses_required() ->
    ?assertEqual({error, {confidentiality, kem_advertise_disabled}}, advertise(#{confidential => required})),
    ?assertEqual(not_sent, receive {advertised, _} -> sent after 100 -> not_sent end).

switched_on_names_the_key() ->
    ok = application:set_env(macula, kem_advertise, enabled),
    ?assertEqual(ok, advertise(#{})),
    Spec = advertised(),
    ?assertEqual(true, maps:get(kem, Spec)),
    ?assertNot(is_map_key(confidential, Spec)).

switched_on_off_names_no_key() ->
    ok = application:set_env(macula, kem_advertise, enabled),
    ?assertEqual(ok, advertise(#{confidential => off})),
    ?assertNot(is_map_key(kem, advertised())).

switched_on_required() ->
    ok = application:set_env(macula, kem_advertise, enabled),
    ?assertEqual(ok, advertise(#{confidential => required})),
    ?assertMatch(#{kem := true, confidential := required}, advertised()).

an_unknown_value_is_refused() ->
    ok = application:set_env(macula, kem_advertise, enabled),
    ?assertEqual({error, {confidentiality, {not_a_mode, sometimes}}}, advertise(#{confidential => sometimes})).

a_renewal_keeps_the_decision() ->
    ok = application:set_env(macula, kem_advertise, enabled),
    {M, F, [Realm, Procedure, Seams]} = macula:renewal(?REALM, procedure(), (seams())#{confidential => required}),
    ?assertMatch({ok, #{kem := true, confidential := required}}, apply(M, F, [self(), Realm, Procedure, Seams])).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

setup() ->
    _ = application:load(macula),
    Previous = application:get_env(macula, kem_advertise),
    ok = application:unset_env(macula, kem_advertise),
    Previous.

teardown(undefined) -> application:unset_env(macula, kem_advertise);
teardown({ok, Value}) -> application:set_env(macula, kem_advertise, Value).

advertise(Opts) ->
    macula:advertise(self(), ?REALM, procedure(), fun(_) -> {ok, 1} end, maps:merge(seams(), Opts)).

advertised() ->
    receive {advertised, Spec} -> Spec after 1_000 -> error(not_advertised) end.

%% The seams an own-namespace advertise reads: the pool's node id, and the
%% fan-out, which records the spec it is handed.
seams() ->
    Self = self(),
    #{status => fun(_Pool) -> {ok, #{self_node_id => node_id()}} end,
      advertise => fun(_Pool, _Realm, _Proc, _Handler, _Policy, Spec) -> Self ! {advertised, Spec}, ok end}.

procedure() ->
    <<"~", (binary:encode_hex(node_id(), lowercase))/binary, "/ring">>.

node_id() ->
    <<42:256>>.
