%% A stream to an explicit target decides to seal exactly as a call does
%% (E2E design §8.1): `macula:call_stream_station/7' goes through
%% `macula:call_seal/5', and hands the pool the seal it decided (`seal' in the
%% stream's options) and nothing of the signed state it decided from. With no
%% signed state the open is refused before the pool is asked. This process
%% stands in for the pool.
-module(macula_call_stream_seal_tests).

-include_lib("eunit/include/eunit.hrl").

-define(REALM, <<5:256>>).
-define(PROC, <<"acme/count_v1">>).
-define(STATION, #{host => <<"127.0.0.1">>, port => 4433, expected_node_id => <<3:256>>}).

no_signed_state_is_refused_before_the_pool_test() ->
    #{target := Target} = fixture(),
    ?assertEqual({error, {confidentiality, no_signed_state}},
                 macula:call_stream_station(self(), ?STATION, Target, ?REALM, ?PROC, #{}, #{})),
    ?assertEqual(none, receive {'$gen_call', _, _} -> asked after 100 -> none end).

off_opens_in_the_clear_test() ->
    #{target := Target} = fixture(),
    ?assertEqual(clear, seal_asked(Target, #{confidential => off})).

a_keyed_advertisement_opens_sealed_to_its_key_test() ->
    #{target := Target, kem_key := KemKey} = F = fixture(),
    ?assertEqual({sealed_to, KemKey}, seal_asked(Target, #{advertisement => keyed_ad(F)})).

a_keyless_advertisement_opens_in_the_clear_test() ->
    #{target := Target} = F = fixture(),
    ?assertEqual(clear, seal_asked(Target, #{advertisement => keyless_ad(F)})).

required_refuses_a_keyless_advertisement_test() ->
    #{target := Target} = F = fixture(),
    ?assertEqual({error, {confidentiality, no_kem_key}},
                 macula:call_stream_station(self(), ?STATION, Target, ?REALM, ?PROC, #{},
                                            #{advertisement => keyless_ad(F), confidential => required})).

%% The pool is handed the seal, not the advertisement or the policy.
the_pool_is_handed_the_seal_and_not_the_signed_state_test() ->
    #{target := Target} = F = fixture(),
    Opts = opts_asked(Target, #{advertisement => keyed_ad(F), mode => bidi}),
    ?assertNot(is_map_key(advertisement, Opts)),
    ?assertNot(is_map_key(confidential, Opts)),
    ?assertEqual(bidi, maps:get(mode, Opts)).

%%------------------------------------------------------------------
%% Helpers
%%------------------------------------------------------------------

seal_asked(Target, Opts) ->
    maps:get(seal, opts_asked(Target, Opts)).

%% The stream options call_stream_station/7 hands the pool for Opts.
opts_asked(Target, Opts) ->
    Test = self(),
    Caller = spawn_link(fun() ->
                            Test ! {opened, macula:call_stream_station(Test, ?STATION, Target, ?REALM, ?PROC, #{},
                                                                       Opts)}
                        end),
    receive
        {'$gen_call', From, {call_stream_station, _Station, Target, ?REALM, ?PROC, #{}, StreamOpts, _LinkOpts}} ->
            gen_server:reply(From, {error, stopped_here}),
            receive {opened, {error, stopped_here}} -> ok after 1000 -> error({no_answer, Caller}) end,
            StreamOpts
    after 1000 ->
        error(pool_never_asked)
    end.

fixture() ->
    _ = application:load(macula),
    {ok, Key} = macula_node_keys:generate(identity, profile()),
    {Public, _Private} = macula_seal:generate_key(profile()),
    #{key => Key, target => macula_node_keys:key_id(Key), kem_key => macula_seal:key_as_carried(Public)}.

keyed_ad(#{key := Key, target := Target, kem_key := KemKey}) ->
    macula_record:sign(macula_record:procedure_advertisement(Target, ?REALM, ?PROC, <<1:256>>, #{kem_key => KemKey}),
                       Key).

keyless_ad(#{key := Key, target := Target}) ->
    macula_record:sign(macula_record:procedure_advertisement(Target, ?REALM, ?PROC, <<1:256>>, #{}), Key).

profile() ->
    {ok, Profile} = macula_crypto_profile:configured(),
    Profile.
